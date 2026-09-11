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

(defcustom bs-mistty-osc-commands '(find-file)
  "Commands a shell running under MisTTY is allowed to invoke."
  :type '(repeat function)
  :group 'mistty)

(defun bs-mistty-osc-call (form window)
  "Apply the car of FORM to the rest of FORM, with WINDOW selected.

WINDOW may be nil or dead, in which case the selected window is used."
  (if (window-live-p window)
      (with-selected-window window (apply (car form) (cdr form)))
    (apply (car form) (cdr form))))

(defun bs-mistty-osc (_code text)
  "Run the command TEXT names in the window holding the shell that sent it.

TEXT is an elisp form, optionally prefixed with \"E\", whose car must be a
member of `bs-mistty-osc-commands'.  The rest of the form is passed as
arguments without being evaluated."
  (let ((form (ignore-errors
                (car (read-from-string
                      (if (string-prefix-p "E" text) (substring text 1) text)))))
        (window (and (buffer-live-p mistty-work-buffer)
                     (get-buffer-window mistty-work-buffer t))))
    (if (memq (car-safe form) bs-mistty-osc-commands)
        (run-at-time 0 nil #'bs-mistty-osc-call form window)
      (message "MisTTY: refusing to run %s" text))))

(add-to-list 'mistty-osc-handlers '("51" . bs-mistty-osc))

(provide 'bs-mistty)
