;;; bs-eat.el --- Additions to eat  -*- lexical-binding: t; -*-

(declare-function eat-term-send-string "eat" (terminal string))
(declare-function eat-term-size "eat" (terminal))
(defvar eat-terminal)

;;;###autoload
(defun bs-eat-resync ()
  "Tell this buffer's child process how big its terminal really is.

Use this when the display is mangled: rows below some invisible line
frozen and never repainted, text from separate screens interleaved
sideways.  That is a pty smaller than the window eat renders into -- the
child is painting a correct screen of the wrong shape.  C-l cannot fix
it, because C-l reaches the child, which redraws at the size it still
believes in.

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
      (let ((now (eat-term-size eat-terminal)))
        (message "Resynced %s to %dx%d" (buffer-name) (car now) (cdr now))))))

(provide 'bs-eat)
