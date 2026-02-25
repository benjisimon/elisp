;;
;; working with table of contents files
;;

(defvar bs-toc-file  nil
  "The currently active toc file")

(defun bs-toc-setup-keys ()
  "Setup keybindings for working with toc's"
  (local-set-key (kbd "C-x t <") 'bs-toc-find-prev)
  (local-set-key (kbd "C-x t >") 'bs-toc-find-next))

(defun bs-toc-find-current ()
  "Find the currently marked TOC file, and set the current toc"
  (interactive)
  (setq bs-toc-file (buffer-file-name))
  (goto-char (point-min))
  (when (re-search-forward "^-- \\(.+\\)$" nil t)
    (find-file (match-string 1))
    (bs-toc-setup-keys)))

(defun bs-toc-step-and-find (n)
  "Move -- marker to `n` lines in rules.toc and open that file."
  (with-current-buffer (find-file-noselect bs-toc-file)
    (goto-char (point-min))
    (when (re-search-forward "^-- \\(.+\\)$" nil t)
      ;; Remove -- from current line
      (replace-match "\\1")
      ;; Move to next line
      (forward-line n)
      (unless (eobp)  ; check not at end of buffer
        ;; Add -- to beginning of line
        (beginning-of-line)
        (insert "-- ")
        (beginning-of-line)
        ;; Get the filename
        (when (re-search-forward "^-- \\(.+\\)$" nil t)
          (let ((filename (match-string 1)))
            (save-buffer)  ; save rules.toc
            (find-file filename)
            (bs-toc-setup-keys)))))))

  (defun bs-toc-find-next ()
    "Move -- mark 1 line and open the file"
    (interactive)
    (bs-toc-step-and-find 1))

  (defun bs-toc-find-prev ()
    (interactive)
    "Move -- mark 1 line and open the file"
    (bs-toc-step-and-find -1))

(provide 'bs-toc)
