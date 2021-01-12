;;
;; Handy functions that Ben uses
;;

;; extra functions
(defun insert-timestamp ()
  "Insert a timestamp into the current buffer."
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S"
                              (current-time))))

(defun insert-random-token ()
  "Insert a random token into the current buffer."
  (interactive)
  (insert (md5 (format "%s" (random)))))

(defun cleanup-iso-latin-chars ()
  "Cleanup a buffer full of iso latin characters into plain ASCII."
  (interactive)
  (let ((transforms '(("’" . "'") ("‘" . "'") ("“" ."\"") ("”" . "\"") ("–" . "-") ("™" . "(tm)")
                      ("…" . "...") ("—" . "--") ("©" . "&copy;"))))
    (while transforms
      (save-excursion
        (goto-char (point-min))
        (while (search-forward (caar transforms) nil t)
          (replace-match (cdar transforms)) nil t))
      (setq transforms (cdr transforms)))))


(defun insert-relative-path-at-point (path)
  "Prompt for a path, and insert the part relative to where we currently are."
  (interactive "GPath: \n")
  (let ((current-path (abbreviate-file-name default-directory)))
    (insert (substring path (length current-path)))))

(defun find-filename-at-point ()
  "Open the file the curson is poitning to."
  (interactive)
  (find-file (thing-at-point 'filename)))

(defun url-decode-region (start end)
  "Replace a region with the same contents, only URL decoded."
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun url-encode-region (start end)
  "Replace a region with the same contents, only URL encoded."
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun count-occurances-in-region (needle start end)
  (save-excursion
    (let ((found 0))
      (goto-char start)
      (while (search-forward needle end t nil)
        (setq found (+ found 1)))
      found)))

(defun url-humanify ()
  "Take the URL at point and make it human readable"
  (interactive)
  (let* ((area (bounds-of-thing-at-point 'url))
         (num-params  (count-occurances-in-region "&" (car area) (cdr area)))
         (i 0))
    (beginning-of-thing 'url)
    (when (search-forward "?" (cdr area) t nil)
      (insert "\n  ")
      (while (< i num-params)
        (search-forward "&" nil t nil)
        (insert "\n  ")
        (save-excursion
          (previous-line)
          (beginning-of-line)
          (let ((start (search-forward "="))
                (end (search-forward "&")))
            (url-decode-region start end)))
        (setq i (+ i 1))))))

(defun unix-timestamp-humanify ()
  "Take the timesetamp at point and make it human readable"
  (interactive)
  (let* ((ts (thing-at-point 'word t))
         (area (bounds-of-thing-at-point 'word))
         (text (format-time-string "%F %T" (seconds-to-time (string-to-number ts)) t)))
    (goto-char (cdr area))
    (insert (format "[%s]" text))))

;; Via: http://blog.bookworm.at/2007/08/emacs-unfill-region.html
(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs,
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

;; http://ergoemacs.org/emacs/emacs_html.html
(defun html-escape-region (start end)
  "Replace “<” to “&lt;” and other chars in HTML.
This works on the current region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&" nil t) (replace-match "&amp;" nil t))
    (goto-char (point-min))
    (while (search-forward "<" nil t) (replace-match "&lt;" nil t))
    (goto-char (point-min))
    (while (search-forward ">" nil t) (replace-match "&gt;" nil t))))


(defun dos2unix ()
  "Convert a buffer to have unix end of lines"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )

(defun unix2dos ()
  "Convert a buffer to have DOS end of lines"
  (interactive)
  (set-buffer-file-coding-system 'dos 't) )

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun multi-replace-regexp-in-string (todo string)
  "Perform repace-regexp-in-string multiple times"
  (let ((result string))
    (dolist (elt todo result)
      (setq result (replace-regexp-in-string (car elt) (cdr elt) result)))))

(defun code-review-region (beg end)
  (interactive "r")
  (let* ((text (chomp (buffer-substring-no-properties beg end)))
         (line-number (line-number-at-pos))
         (file (buffer-file-name))
         (path (multi-replace-regexp-in-string
                '(("^.*branches/" . "")
                  ("^.*trunk/" . "")
                  ("^.*development/" . "")
                  ("^.*production/" . "")
                  ("^.*src/" . "")) file)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match "| " nil nil))
      (goto-char (point-min))
      (insert (format "+---[%s:%s]\n" path line-number))
      (goto-char (point-max))
      (insert "\n+---\n")
      (kill-region (point-min) (point-max)))))


(defun mysql-insert-table-defn (table-name)
  "Insert a table definition at point for a MySQL table."
  (interactive "sTable name: \n")
  (insert (format "CREATE TABLE %s (\n" table-name))
  (insert "  id                  BIGINT            NOT NULL        AUTO_INCREMENT,\n"
          "  created             TIMESTAMP         NOT NULL        DEFAULT CURRENT_TIMESTAMP,\n"
          "  ")
  (save-excursion
    (insert "\n  INDEX(created),")
    (insert "\n  PRIMARY KEY(id)\n) Engine=InnoDB;\n")))

(defun kill-url-browse-url-function (url &rest ignore)
  (kill-new url)
  (message "Killed: %s" url))

(defun browse-php.net-doc ()
  (interactive)
  (let ((fn (thing-at-point 'symbol t)))
    (if fn
        (browse-url (format "https://php.net/%s" fn)))))

(defun write-compile-command-dir-locals (command)
  "Write out a .dir-locals.el with the sane compile command"
  (interactive "sCompile command: ")
  (with-temp-file (format "%s.dir-locals.el" default-directory)
    (insert (format "%S"
                    `((nil . ((compile-command . ,command))))))))
    
(provide 'bs-fns)
