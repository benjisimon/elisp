;; -*- lexical-binding: t; -*-

;;; Repairing and debugging mangled eat terminals.
;;
;; The failure this was written for, now measured rather than guessed:
;; the child process and eat disagree about the size of the terminal.
;;
;; claude-code-ide launches Claude through a wrapper that runs
;; `stty rows 24 columns 80' before exec'ing it, so every session starts
;; at 24x80 and depends on something correcting it afterwards.  The only
;; thing that ever does is `window--adjust-process-windows'
;; (window.el:10880), which Emacs runs from `window-configuration-change-hook'
;; and nowhere else.  claude-code-ide calls `eat-exec' on a buffer made
;; with `get-buffer-create' and displays it afterwards, so if no
;; configuration change fires with the process already attached, the pty
;; keeps 24x80 for the life of the session and nothing retries.
;;
;; Eat meanwhile sizes its own grid from the window.  The result is
;; Claude painting a correct 24x80 screen inside an 86x62 grid: rows
;; below 24 hold whatever was there before and are never written again,
;; and the 80-versus-86 column difference shifts writes sideways so text
;; from separate frames interleaves character by character.
;;
;; `bs-eat-check' measures the disagreement, `bs-eat-resync' repairs it,
;; and `bs-eat-startup-guard-mode' prevents it.  `bs-eat-repaint' is for
;; ordinary mangling and will NOT fix a size mismatch -- the child just
;; repaints at the same wrong size.

(require 'cl-lib)

(declare-function eat-term-send-string "eat" (terminal string))
(declare-function eat-term-size "eat" (terminal))
(declare-function eat-term-in-alternative-display-p "eat" (terminal))
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


;;; Size agreement
;;
;; Three sizes have to line up: the kernel's winsize on the pty (what
;; the child sees), eat's grid (what gets rendered), and the window
;; (what you look at).  Only the first two matter for the mangling;
;; the window matters because it is where eat's grid size comes from.

(defun bs-eat--pty-size (process)
  "Return the kernel's window size for PROCESS's pty as (WIDTH . HEIGHT).

This is the size the child actually sees.  Emacs has no reader for it --
`set-process-window-size' only writes -- so this shells out to stty.
Cheap enough to run on demand, not cheap enough for a timer.

Returns nil if it cannot be determined."
  (when-let* ((tty (and process (process-tty-name process)))
              ;; -f is the BSD/macOS spelling; -F is GNU.
              (flag (if (eq system-type 'gnu/linux) "-F" "-f"))
              (out (with-output-to-string
                     (ignore-errors
                       (call-process "stty" nil standard-output nil
                                     flag tty "size"))))
              (trimmed (string-trim out)))
    (when (string-match (rx bos (group (+ digit)) (+ space) (group (+ digit)))
                        trimmed)
      ;; stty prints "rows cols"; everything here speaks (width . height).
      (cons (string-to-number (match-string 2 trimmed))
            (string-to-number (match-string 1 trimmed))))))

(defun bs-eat--size-info (buffer)
  "Return a plist comparing the pty, eat's grid and the windows on BUFFER."
  (with-current-buffer buffer
    (let* ((proc (get-buffer-process buffer))
           (grid (and (bound-and-true-p eat-terminal)
                      (eat-term-size eat-terminal)))
           (pty (and proc (bs-eat--pty-size proc))))
      (list :pty pty
            :grid grid
            ;; The whole diagnosis in one field.
            :agree (and pty grid (equal pty grid))
            :windows (mapcar (lambda (win)
                               (cons (window-body-width win)
                                     (window-body-height win)))
                             (get-buffer-window-list buffer nil t))))))

(defun bs-eat--format-size (size)
  "Format SIZE, a (WIDTH . HEIGHT) cons, for a message."
  (if size (format "%dx%d" (car size) (cdr size)) "unknown"))

;;;###autoload
(defun bs-eat-check ()
  "Report whether the child process and eat agree on the terminal size.

This is the check that matters: a disagreement means the child is
painting a screen of one shape into a grid of another, which is what
strands frozen rows below the live region and interleaves text.

Run it before repairing anything -- `bs-eat-resync' and `bs-eat-repaint'
both destroy the evidence."
  (interactive)
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not an eat buffer: %s" (buffer-name)))
  (let* ((info (bs-eat--size-info (current-buffer)))
         (pty (plist-get info :pty))
         (grid (plist-get info :grid)))
    (cond
     ((null grid)
      (message "No live terminal in %s" (buffer-name)))
     ((null pty)
      (message "eat grid %s; could not read the pty size"
               (bs-eat--format-size grid)))
     ((equal pty grid)
      (message "OK: child and eat agree at %s (window %s)"
               (bs-eat--format-size grid)
               (mapconcat #'bs-eat--format-size
                          (plist-get info :windows) ", ")))
     (t
      (message "MISMATCH: child sees %s, eat renders %s -- M-x bs-eat-resync"
               (bs-eat--format-size pty)
               (bs-eat--format-size grid))))))


;;; Repair

;;;###autoload
(defun bs-eat-resync ()
  "Make the pty size match eat's grid, so the child repaints correctly.

This is the fix for a session whose lower rows are frozen and whose text
is interleaved.  `bs-eat-repaint' cannot help there -- it makes the
child redraw at whatever size it already believes in, which is the wrong
one.

Deliberately sets a wrong size first: `set-process-window-size' sends no
SIGWINCH when the kernel already holds the dimensions being written, and
a session stuck this way is usually stuck at a size Emacs thinks it
already set."
  (interactive)
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not an eat buffer: %s" (buffer-name)))
  (let ((proc (get-buffer-process (current-buffer))))
    (unless proc
      (user-error "No process in %s" (buffer-name)))
    (unless (bound-and-true-p eat-terminal)
      (user-error "No live terminal in %s" (buffer-name)))
    (let* ((size (eat-term-size eat-terminal))
           (width (car size))
           (height (cdr size)))
      (set-process-window-size proc (max 1 (1- height)) width)
      ;; Recomputes from the windows and writes the real size, which now
      ;; differs from what we just set, so the signal is guaranteed.
      (window--adjust-process-windows)
      (eat-term-send-string eat-terminal "\C-l")
      (redraw-display)
      (message "Resynced %s to %s" (buffer-name)
               (bs-eat--format-size (eat-term-size eat-terminal))))))

;;;###autoload
(defun bs-eat-repaint (&optional arg)
  "Repair a mangled terminal display in the current buffer.

Sends C-l, which makes a TUI such as Claude Code clear its screen and
redraw from scratch, then forces a full Emacs redisplay.  This discards
mangled rows without touching the session -- Claude keeps its
conversation, a shell keeps its history.

With prefix ARG, reset eat's own terminal state first via `eat-reset'.

Only useful when the two ends agree on the size.  If they do not, the
child redraws at its own wrong size and nothing improves; run
`bs-eat-check' first, and `bs-eat-resync' if it reports a mismatch."
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


;;; Prevention

(defvar bs-eat-startup-guard-delays '(1 3 8)
  "Seconds after `eat-exec' at which to re-run the pty size adjustment.
More than one because the buffer may not be in a window yet when the
first fires, and `window--adjust-process-windows' only sizes processes
whose buffer is currently displayed.")

(defun bs-eat--guard-after-exec (&rest _)
  "Schedule the pty size adjustment that a new eat session may not get."
  (dolist (delay bs-eat-startup-guard-delays)
    (run-with-timer delay nil #'window--adjust-process-windows)))

;;;###autoload
(define-minor-mode bs-eat-startup-guard-mode
  "Re-run the pty size adjustment shortly after each eat session starts.

Closes the startup race described at the top of this file: the wrapper
leaves the pty at 24x80 and only a window configuration change ever
corrects it, so a session that starts without one stays wrong forever.
Running the adjustment on a timer supplies the missing correction.

Idempotent and cheap -- `window--adjust-process-windows' recomputes from
the current layout and writes the same size it would have written
anyway, so firing it when nothing is wrong costs nothing."
  :global t
  :lighter nil
  :group 'eat
  (if bs-eat-startup-guard-mode
      (advice-add 'eat-exec :after #'bs-eat--guard-after-exec)
    (advice-remove 'eat-exec #'bs-eat--guard-after-exec)))


;;; Resize log
;;
;; Records eat's own grid resizes.  Now that the fault is known to be a
;; resize that never happened, an empty log for a session is itself the
;; symptom rather than merely an absence of evidence.

(defvar-local bs-eat-resize-log nil
  "Recent grid resizes in this buffer, newest first.
Each entry is (TIME WIDTH HEIGHT WINDOW-COUNT).")

(defvar bs-eat-resize-log-size 50
  "How many resize events to keep per buffer.")

(defvar bs-eat-resize-log-armed-at nil
  "When the resize advice was installed, as a float time.
Recording only starts at load, so an empty log means \"was not
watching\" rather than \"nothing happened\" unless this predates the
session.  The snapshot reports it for exactly that reason.")

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
;; Only on the first load, so re-evaluating this file while debugging
;; does not fake a fresh arming time.
(unless bs-eat-resize-log-armed-at
  (setq bs-eat-resize-log-armed-at (float-time)))

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

(defun bs-eat--process-start-time (process)
  "Return when PROCESS started, as a float time, or nil if unavailable.
Used to tell whether the resize log was already armed when this session
began -- if it was not, an empty log means nothing was recorded rather
than nothing happened."
  (when-let* ((pid (process-id process))
              (start (alist-get 'start (process-attributes pid))))
    (float-time start)))

;;;###autoload
(defun bs-eat-snapshot (&optional lines)
  "Write a debugging snapshot of the current eat buffer to a file.

Captures the pty size, the size eat believes it has, the geometry of
every window showing the buffer, and the last LINES lines of rendered
text \(default 200, or the prefix argument).  Control characters in the
text are escaped so stray escape sequences stay visible."
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
            ;; Read this first.  `:agree' nil is the fault: the child is
            ;; painting one shape into a grid of another.
            (size . ,(bs-eat--size-info buffer))
            (eat-term-scrollback-size . ,eat-term-scrollback-size)
            (eat-enable-alternative-display
             . ,eat-enable-alternative-display)
            (in-alternative-display
             . ,(and (bound-and-true-p eat-terminal)
                     (eat-term-in-alternative-display-p eat-terminal)))
            (eat-maximum-latency . ,eat-maximum-latency)
            (eat-minimum-latency . ,eat-minimum-latency)
            ;; Approximate: claude-code-ide binds `eat-term-name' to
            ;; "xterm-256color" only around its own `eat-exec' call, so
            ;; the global read here is not what that child was given.
            (global-term-name . ,(if (functionp eat-term-name)
                                     (funcall eat-term-name)
                                   eat-term-name))
            (window-adjust-process-window-size-function
             . ,window-adjust-process-window-size-function)
            (windows . ,(bs-eat--window-info buffer))
            (frame-width . ,(frame-width))
            (frame-height . ,(frame-height))
            (startup-guard . ,(bound-and-true-p bs-eat-startup-guard-mode))
            ;; Newest first.  An empty log for a session that started
            ;; after `resize-log-armed' means the adjustment never ran,
            ;; which is the fault itself.
            (resize-log-armed
             . ,(if bs-eat-resize-log-armed-at
                    (format-time-string
                     "%H:%M:%S"
                     (seconds-to-time bs-eat-resize-log-armed-at))
                  'never))
            (resize-log-armed-before-session
             . ,(let ((started (and proc
                                    (bs-eat--process-start-time proc))))
                  (cond ((not bs-eat-resize-log-armed-at) nil)
                        ((not started) 'unknown)
                        (t (< bs-eat-resize-log-armed-at started)))))
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
;; byte from the child plus every resize, and `eat-trace-replay'
;; (eat.el:8182) replays that offline, frame by frame -- which turns an
;; occasional fault into one that can be single-stepped.
;;
;; Worth keeping for the residual mangling that is NOT a size mismatch:
;; if `bs-eat-check' says the sizes agree and the display is still
;; wrong, this is what is left.

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
(defun bs-eat-trace-status ()
  "Report how much trace data is currently being held in memory.

Tracing records every byte the child writes and never trims, so leaving
it armed for a fault that only shows up occasionally accumulates
indefinitely.  Check this now and then to decide whether waiting longer
is still worth it."
  (interactive)
  (let ((traces (seq-filter
                 (lambda (b) (string-prefix-p "*eat-trace " (buffer-name b)))
                 (buffer-list))))
    (if (null traces)
        (message "No trace buffers. Tracing is %s."
                 (if (bound-and-true-p eat-trace-mode)
                     "on -- but only records sessions started after it was enabled"
                   "off"))
      (message "%s"
               (mapconcat
                (lambda (b)
                  ;; Characters, not bytes -- close enough to judge by.
                  (format "%s  %s" (file-size-human-readable (buffer-size b))
                          (buffer-name b)))
                (sort traces (lambda (a b) (> (buffer-size a) (buffer-size b))))
                "\n")))))

;;;###autoload
(defun bs-eat-trace-save (&optional stop)
  "Write the trace for the current eat buffer to a file.

With prefix argument STOP, also turn `eat-trace-mode' back off.

Open the saved file and run \\[eat-trace-replay] to step through it.

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
