;;
;; Simple commands for accessing fake names. Useful for testing.
;;

(defvar fake-name-data-file "~/dt/i2x/elisp/src/main/data/fake-first-names.txt")
(defvar fake-name-posn-file "~/.emacs.d/.cache/fake-name-posn.txt")

(defun fake-name-current-posn ()
  "Return the current position we're choosing our name from"
  (if (file-exists-p fake-name-posn-file)
      (with-temp-buffer
        (insert-file-contents fake-name-posn-file)
        (string-to-number (buffer-string)))
    1))

(defun fake-name-save-new-posn (posn)
  "Save the position we're at so we can ge the next name easily."
  (with-temp-file fake-name-posn-file
    (insert (format "%d" posn))))

(defun fake-name-peek-next-name ()
  "Return the next name, but don't save this position."
  (let ((names-buffer (find-file-noselect fake-name-data-file))
        (bol (fake-name-current-posn)))
    (with-current-buffer names-buffer
      (goto-char bol)
      (let* ((eol (line-end-position))
             (name (buffer-substring bol eol)))
        name))))

(defun fake-name-advance-posn (name)
  "Advance our fake name position by `name`"
  (let ((current-posn (fake-name-current-posn)))
    (fake-name-save-new-posn (+ current-posn 1 (length name)))))

(defun fake-name-next-name ()
  "Return the next fake name."
  (let ((continue t)
        (name nil))
    (while continue
      (setq name  (fake-name-peek-next-name))
      (fake-name-advance-posn name)
      (setq continue (fake-name-exists-in-current-buffer-p name)))
    name))

(defun insert-fake-name ()
  "Insert the next name in our current buffer"
  (interactive)
  (insert (fake-name-next-name)))

(defun fake-name-exists-in-current-buffer-p (name)
  "True the fake name exist in the current buffer?"
  (let ((regexp (concat "\\(\\W\\|^\\)" name "\\(\\W\\|$\\)")))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

(defun fake-name-reset-names ()
  "Reset our names to the first possible name."
  (interactive)
  (fake-name-save-new-posn 1))


(provide 'fake-names)
