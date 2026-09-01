;;; bs-mistty.el --- Additions to mistty  -*- lexical-binding: t; -*-

(require 'mistty)

;;;###autoload
(defun bs-mistty-home (&optional n)
  "Switch to general shell N, starting it in ~/ if it is not live.

N is a prefix argument.  Without one the shell is *mistty*; with one it
is *mistty-N*.  These shells belong to no project and always start at
home, so a given N is always the same shell."
  (interactive "P")
  (let* ((default-directory "~/")
         (mistty-buffer-name
          (cons (if n (format "mistty-%d" (prefix-numeric-value n)) "mistty")
                (cdr mistty-buffer-name)))
         (name (mistty-new-buffer-name))
         (buf (get-buffer name)))
    (if (and buf (mistty-live-buffer-p buf))
        (pop-to-buffer buf)
      (when buf
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf)))
      (mistty-create))))

(provide 'bs-mistty)
