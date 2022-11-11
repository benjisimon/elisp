;;
;; Simple commands for accessing fake names. Useful for testing.
;;

(defvar fake-name-data-file "~/.emacs.d/local/data/fake-first-names.txt")
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

(defun fake-name-next-name ()
  "Return the next fake name."
  (let ((names-buffer (find-file-noselect fake-name-data-file))
        (bol (fake-name-current-posn)))
    (with-current-buffer names-buffer
      (goto-char bol)
      (let* ((eol (line-end-position))
             (name (buffer-substring bol eol)))
        (fake-name-save-new-posn (+ 1 eol))
        name))))

(defun insert-fake-name ()
  "Insert the next name in our current buffer"
  (interactive)
  (insert (fake-name-next-name)))

(defun fake-name-reset-names ()
  "Reset our names so we start at the beginning"
  (interactive)
  (fake-name-save-new-posn 1))


(provide 'fake-names)
