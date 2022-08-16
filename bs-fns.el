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


(defun bs-ibuffer-filter-groups ()
  (let* ((projects (seq-filter (lambda (file)
                                 (and (not (equal "src" file))
                                      (not (equal (substring file 0 1) "."))))
                               (directory-files "~/dt/i2x")))
         (project-rules  (seq-map (lambda (f)
                                    `(,f (and (directory . ,f)
                                              (directory . "i2x"))))
                                  projects)))
    `(("main"
       ("emacs" (or (directory . ".emacs.d")
                    (mode . "Emacs-Lisp")))
       ("tools" (and (directory . "src") (directory . "trunk") (directory . "tools")))
       ,@project-rules))))


(defun forth-refresh ()
  "Launch a fresh instance of forth using the current buffer."
  (interactive)
  (when (process-live-p (get-buffer-process forth-process-buffer))
    (delete-process (get-buffer-process forth-process-buffer))
    (kill-buffer forth-process-buffer))
  (save-window-excursion
    (run-forth forth-program-name))
  (forth-load-file buffer-file-name)
  (forth-switch-to-interactive t))

(defun browse-project-resource-url (resource)
    "Go to a standard URL based on the project"
  (interactive
   (list (completing-read "Resource? "
                          '("ch" "bugs" "d"))))
  (let* ((dir default-directory)
         (client (replace-regexp-in-string ".*/i2x/\\([^/]*\\)/.*" "\\1" dir)))
    (browse-url (format "http://u.i2x.us/%s-%s" resource client))))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


(defun find-snippet-at-point ()
  "Find the snippet under the current cursor."
  (interactive)
  (let ((found (thing-at-point-looking-at "['\"]\\(.*?\\)['\"]")))
    (if found
        (let ((path (buffer-substring (match-beginning 1) (match-end 1)))
              (root (locate-dominating-file default-directory "snippets")))
          (xref-push-marker-stack)
          (find-file (concat root "snippets/" path ".php")))
      (error "Not on a snippet expression."))))



;; https://www.reddit.com/r/emacs/comments/ty5i2l/emacs_28_29_watching_eln_files_and_causing_too/
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/filenotify.el
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))


(defun current-vc-branch ()
  "Guess the current branch or return nil if aren't on one"
  (require 'dsvn)
  (let ((d default-directory))
    (cond
     ((locate-dominating-file d ".git")
      (vc-git--symbolic-ref default-directory))
     ((locate-dominating-file d ".svn")
      (let ((url (svn-current-url)))
        (cond ((string-match "/trunk\\(/\\|$\\)" url) "trunk")
              ((string-match "/branches/\\(.*?\\)\\(/\\|$\\)" url)
               (match-string 1 url))
              (t nil))))
     (t nil))))

(defun current-customer ()
  "Guess the customer we are looking at by looking at our path"
  (let ((d default-directory))
    (if (string-match "dt/i2x/\\(.*?\\)/" d)
        (match-string 1 d)
      nil)))


(defun project-notes-find-file-today ()
  (interactive)
  "Quickly open up a notes file or create one for today."
  (let* ((base "~/dt/i2x/project-notes/src/main")
         (timestamp (format-time-string "%Y-%m-%d"))
         (branch (current-vc-branch))
         (customer (current-customer))
         (dir (cond
               ((and branch customer)
                (format "%s/%s/%s" base customer branch))
               (customer
                (format "%s/%s/misc" base customer))
               (t (format "%s/internal/misc" base)))))
    (unless (file-accessible-directory-p dir)
      (mkdir dir t))
    (find-file (format "%s/%s.md" dir timestamp))))
    
         

(defun svnassist-switch-release ()
  "Switch the current repo to current release"
  (interactive)
  (let ((root (locate-dominating-file default-directory ".svn")))
    (let ((default-directory root))
      (shell-command "svnassist switch-release"))))

(defun bs-relative-path-to (current target)
  "Find a relative path to `target` starting from `current`"
  (let ((root (locate-dominating-file current target)))
    (file-relative-name (concat root "/" target) current)))


(provide 'bs-fns)
