;;
;; Work with project notes
;;

(defvar project-notes-bookmark-name "project-notes-anchor")

(defun project-notes-find-file-today ()
  "Quickly open up a notes file or create one for today."
  (interactive)
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
    (bookmark-set project-notes-bookmark-name nil)
    (find-file (format "%s/%s.org" dir timestamp))))

(defun project-notes-jump-back ()
  "Go back to where we launched project notes"
  (interactive)
  (bookmark-jump project-notes-bookmark-name))

(define-minor-mode project-notes-mode
  "Project notes helper stuff"
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "C-x t n") 'project-notes-jump-back)
             map))

(defun project-notes-maybe-enable-mode ()
  "Maybe turn on our project notes."
  (when buffer-file-name
    (cond ((string-match ".*dt/i2x/project-notes/.*" buffer-file-name)
           (project-notes-mode)))))

(add-hook 'markdown-mode-hook 'project-notes-maybe-enable-mode)
(add-hook 'org-mode-hook 'project-notes-maybe-enable-mode)

(provide 'project-notes)
