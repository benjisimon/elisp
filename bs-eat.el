;; -*- lexical-binding: t; -*-

;;; Repairing and debugging mangled eat terminals.
;;
;; Claude Code (and other full-screen TUIs) paint by cursor-addressing
;; against their own model of what is on screen.  Eat keeps a separate
;; model.  When the two disagree the display gets mangled: duplicated
;; prompt boxes, orphaned rows from a dismissed picker, and so on.
;;
;; `bs-eat-repaint' resolves the disagreement.  `bs-eat-snapshot' and
;; `bs-eat-trace-*' capture what led up to it.

(require 'cl-lib)

(declare-function eat-term-send-string "eat" (terminal string))
(declare-function eat-term-size "eat" (terminal))
(declare-function eat-reset "eat" ())
(declare-function eat-trace-mode "eat" (&optional arg))
(defvar eat-terminal)
(defvar eat--trace-output-buffer)
(defvar eat-term-name)
(defvar eat-term-scrollback-size)
(defvar eat-enable-alternative-display)
(defvar eat-maximum-latency)
(defvar eat-minimum-latency)

(defvar bs-eat-debug-directory (locate-user-emacs-file "eat-debug/")
  "Directory where `bs-eat-snapshot' and `bs-eat-trace-save' write.")


;;; Repair

;;;###autoload
(defun bs-eat-repaint (&optional arg)
  "Repair a mangled terminal display in the current buffer.

Sends C-l, which makes a TUI such as Claude Code clear its screen and
redraw from scratch, then forces a full Emacs redisplay.  This discards
the mangled rows without touching the session -- Claude keeps its
conversation, a shell keeps its history.

With prefix ARG, reset eat's own terminal state first via `eat-reset'.
Use that when C-l alone does not clean it up, which suggests the
corruption is in eat's model rather than in what the TUI painted."
  (interactive "P")
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not an eat buffer: %s" (buffer-name)))
  (unless (bound-and-true-p eat-terminal)
    (user-error "No live terminal in %s" (buffer-name)))
  (when arg
    (eat-reset))
  (eat-term-send-string eat-terminal "\C-l")
  (redraw-display)
  (message "Repainted %s%s" (buffer-name) (if arg " (after reset)" "")))


;;; Resize log
;;
;; Full tracing has to be armed before the session starts, and a
;; snapshot is a still frame.  Neither answers "did the pty get resized
;; without me asking?" for a failure that will not reproduce on demand.
;; This ring is cheap enough to leave on always, so it is already
;; recording when the display next goes wrong.

(defvar-local bs-eat-resize-log nil
  "Recent pty resizes in this buffer, newest first.
Each entry is (TIME WIDTH HEIGHT WINDOW-COUNT).  WINDOW-COUNT matters
because `window-adjust-process-window-size-smallest' sizes the pty to
the narrowest window showing the buffer, so a second window appearing
shrinks the terminal with no user action.")

(defvar bs-eat-resize-log-size 50
  "How many resize events to keep per buffer.")

(defun bs-eat--record-resize (fn process windows)
  "Record the resize FN performs, then return its value.
PROCESS and WINDOWS are passed through to FN."
  (let ((size (funcall fn process windows)))
    (when (and size (derived-mode-p 'eat-mode))
      (let ((prev (car bs-eat-resize-log)))
        ;; Emacs calls this on every redisplay; only log real changes.
        (unless (and prev
                     (eq (nth 1 prev) (car size))
                     (eq (nth 2 prev) (cdr size)))
          (push (list (float-time) (car size) (cdr size)
                      (length (get-buffer-window-list
                               (current-buffer) nil t)))
                bs-eat-resize-log)
          (when (> (length bs-eat-resize-log) bs-eat-resize-log-size)
            (setcdr (nthcdr (1- bs-eat-resize-log-size)
                            bs-eat-resize-log)
                    nil)))))
    size))

(advice-add 'eat--adjust-process-window-size :around
            #'bs-eat--record-resize)

(defun bs-eat--format-resize-log ()
  "Return `bs-eat-resize-log' with times as readable stamps and deltas."
  (let ((now (float-time)))
    (mapcar (lambda (entry)
              (list :at (format-time-string "%H:%M:%S"
                                            (seconds-to-time (nth 0 entry)))
                    :seconds-ago (round (- now (nth 0 entry)))
                    :width (nth 1 entry)
                    :height (nth 2 entry)
                    :windows-on-buffer (nth 3 entry)))
            bs-eat-resize-log)))


;;; After-the-fact snapshot

(defun bs-eat--window-info (buffer)
  "Describe every window currently displaying BUFFER, across all frames."
  (mapcar
   (lambda (win)
     (list :window (format "%S" win)
           :frame (format "%S" (window-frame win))
           :selected (eq win (selected-window))
           :side (window-parameter win 'window-side)
           :body-width (window-body-width win)
           :body-height (window-body-height win)
           :total-width (window-total-width win)
           :max-chars-per-line (with-selected-window win
                                 (window-max-chars-per-line))))
   (get-buffer-window-list buffer nil t)))

;;;###autoload
(defun bs-eat-snapshot (&optional lines)
  "Write a debugging snapshot of the current eat buffer to a file.

Captures the terminal size eat believes it has, the geometry of every
window showing the buffer, and the last LINES lines of rendered text
\(default 200, or the prefix argument).  Control characters in the text
are escaped so stray escape sequences stay visible.

This works after the fact, so it is the one to reach for when the
display has already gone wrong.  It cannot show how it got that way --
`bs-eat-trace-start' does that, but must be armed in advance."
  (interactive "P")
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not an eat buffer: %s" (buffer-name)))
  (let* ((lines (if (numberp lines) lines 200))
         (buffer (current-buffer))
         (proc (get-buffer-process buffer))
         (text (buffer-substring-no-properties
                (save-excursion (goto-char (point-max))
                                (forward-line (- lines))
                                (point))
                (point-max)))
         (file (bs-eat--debug-file "snapshot"))
         ;; Everything below has to be read here, in the eat buffer.
         ;; `with-temp-file' switches buffers, so reading a
         ;; buffer-local there would silently report the global value.
         (report
          `((emacs-version . ,emacs-version)
            (system-type . ,system-type)
            (window-system . ,window-system)
            (buffer . ,(buffer-name buffer))
            (major-mode . ,major-mode)
            (process-command . ,(and proc (process-command proc)))
            (process-tty . ,(and proc (process-tty-name proc)))
            (process-status . ,(and proc (process-status proc)))
            ;; What eat thinks the terminal is.  Compare against the
            ;; window geometry below -- a mismatch means the child
            ;; process is painting for a size that is not on screen.
            (eat-term-size . ,(and (bound-and-true-p eat-terminal)
                                   (eat-term-size eat-terminal)))
            (eat-term-scrollback-size . ,eat-term-scrollback-size)
            (eat-enable-alternative-display
             . ,eat-enable-alternative-display)
            (eat-maximum-latency . ,eat-maximum-latency)
            (eat-minimum-latency . ,eat-minimum-latency)
            ;; The TERM the child was given, not the one Emacs itself
            ;; inherited -- as a GUI app that is "dumb" and tells us
            ;; nothing.
            (child-term-name . ,(if (functionp eat-term-name)
                                    (funcall eat-term-name)
                                  eat-term-name))
            ;; Emacs takes the *smallest* of these by default, so a
            ;; second window on the buffer silently shrinks the pty.
            (window-adjust-process-window-size-function
             . ,window-adjust-process-window-size-function)
            (windows . ,(bs-eat--window-info buffer))
            (frame-width . ,(frame-width))
            (frame-height . ,(frame-height))
            ;; Newest first.  A resize here that you did not perform is
            ;; the thing worth finding.
            (resize-log . ,(bs-eat--format-resize-log)))))
    (with-temp-file file
      (insert ";; -*- mode: lisp-data -*-\n")
      (insert ";; eat display snapshot\n\n")
      (pp report (current-buffer))
      (insert "\n;; Last " (number-to-string lines)
              " lines of rendered text follow.\n\n")
      (insert (bs-eat--escape-controls text)))
    (message "Wrote eat snapshot to %s" file)
    file))

(defun bs-eat--escape-controls (string)
  "Escape control characters in STRING as \\xNN, leaving newlines alone."
  (replace-regexp-in-string
   (rx (any (0 . 9) (11 . 31) 127))
   (lambda (s) (format "\\x%02x" (aref s 0)))
   string))

(defun bs-eat--debug-file (kind)
  "Return a fresh timestamped file name for a debug dump of KIND."
  (make-directory bs-eat-debug-directory t)
  (expand-file-name
   (format "%s-%s.el" kind (format-time-string "%Y%m%d-%H%M%S"))
   bs-eat-debug-directory))


;;; Full tracing
;;
;; `eat-trace-mode' only attaches to a terminal inside `eat-exec', so it
;; has to be enabled before the session starts.  It then records every
;; byte from the child plus every resize, which is what identifies a
;; resize you did not ask for.

;;;###autoload
(defun bs-eat-trace-start ()
  "Turn on `eat-trace-mode' so the next eat session is recorded.

Tracing attaches at process startup, so this does nothing for terminals
that are already running -- start a fresh Claude session afterwards.
Reproduce the problem, then call `bs-eat-trace-save' from that buffer."
  (interactive)
  (require 'eat)
  (eat-trace-mode 1)
  (message "eat tracing on. Start a NEW session, reproduce, then M-x bs-eat-trace-save"))

;;;###autoload
(defun bs-eat-trace-save (&optional stop)
  "Write the trace for the current eat buffer to a file.

With prefix argument STOP, also turn `eat-trace-mode' back off.

The trace contains every byte the child process wrote, so it includes
whatever was on screen -- conversation text, file contents, output.
Read it before sharing it."
  (interactive "P")
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not an eat buffer: %s" (buffer-name)))
  (let ((trace (and (boundp 'eat--trace-output-buffer)
                    eat--trace-output-buffer)))
    (unless (buffer-live-p trace)
      (user-error
       "No trace for %s -- tracing must be on before the session starts"
       (buffer-name)))
    (let ((file (bs-eat--debug-file "trace")))
      (with-current-buffer trace
        (write-region (point-min) (point-max) file))
      (when stop (eat-trace-mode -1))
      (message "Wrote eat trace to %s (%s)" file
               (file-size-human-readable
                (file-attribute-size (file-attributes file))))
      file)))

(provide 'bs-eat)
