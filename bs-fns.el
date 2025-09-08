;;
;; Handy functions that Ben uses
;;

(require 'gptel)

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

(defun insert-current-file-name-sans-extension ()
  "Insert the filename of the current buffer without an extension."
  (interactive)
  (insert (file-name-sans-extension (buffer-name))))


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
         (mode major-mode)
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
      (insert (format "[%s:%d]\n"  path line-number))
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

(defun browse-synonyms-url ()
  "Look up the current word on powerthesaurus"
  (interactive)
  (let* ((word (thing-at-point  'word))
         (url  (format "https://www.powerthesaurus.org/%s/synonyms" (url-encode-url word))))
    (browse-url url)))

(defun ediff-copy-both-to-C ()
  "Add missing feature to ediff-merge. Support selecting both regions."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))


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
  (let* ((d default-directory)
         (svn-dir (locate-dominating-file d ".svn")))
    (cond
     ((locate-dominating-file d ".git")
      (vc-file-clearprops default-directory)
      (vc-git--symbolic-ref default-directory))
     (svn-dir
      (let ((url (with-temp-buffer
                   (let ((default-directory svn-dir))
                     (svn-current-url)))))
        (cond ((string-match "/trunk\\(/\\|$\\)" url) "trunk")
              ((string-match "/branches/\\(.*?\\)\\(/\\|$\\)" url)
               (match-string 1 url))
              (t nil))))
     (t nil))))

(defun run-customer-mode-hook (mode-hook-name)
  "Run all hooks associated with a specific mode and the current customer."
  (interactive)
  (let ((customer-hook (intern (format "%s-%s" (current-customer) mode-hook-name))))
    (when (boundp customer-hook)
      (run-hooks customer-hook))))


(defun current-customer ()
  "Guess the customer we are looking at by looking at our path"
  (let ((d default-directory))
    (if (string-match "dt/i2x/\\(.*?\\)/" d)
        (match-string 1 d)
      nil)))



(defun bs-relative-path-to (current target)
  "Find a relative path to `target` starting from `current`"
  (let ((root (locate-dominating-file current target)))
    (file-relative-name (concat root target) (file-name-directory current))))

(defun svn-status-from-root ()
  "Run svn-status but from the root of this project"
  (interactive)
  (let ((root (locate-dominating-file default-directory ".svn")))
    (svn-status root)))

(defvar bs-gptel-define-word-prompt
  "Please give a short definition of this word or phrase. Then, provide 3 usage examples, synonyms and antyonyms"
  "The ChatGPT style prompt used define a word.")


(defun bs-gptel-stash-response (buffer prompt response)
  "Store a response in a well known buffer we can look at if we want"
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert prompt)
      (insert "\n\n-->\n\n")
      (insert response))))

(defun bs-gptel-define-word (start end)
  "Use ChatGPT to define the current word of the region."
  (interactive "r")
  (unless (region-active-p)
    (error "you must have a region set"))
  (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
    (gptel-request nil
      :callback (lambda (response info)
                  (bs-gptel-stash-response "*Last Definition*" (plist-get info :context) response)
                  (message response))
      :system bs-gptel-define-word-prompt
      :context input)))


(defun bs-svg-preview-region (start end)
  "Consider the region svg code and review it"
  (interactive "r")
  (unless (region-active-p)
    (error "you must have a region set"))
  (let ((svg (buffer-substring-no-properties (region-beginning) (region-end)))
        (buffer-name "*SVG Preview*"))
    (with-temp-buffer
      (switch-to-buffer (get-buffer-create buffer-name))
      (erase-buffer)
      (insert svg)
      (image-mode))))

(provide 'bs-fns)
