;;
;; Define our auto inserts
;;
;; Inspired by: https://github.com/suzp1984/donkey/blob/master/elisp/auto-insert/my-auto-insert.el
;;

(require 'autoinsert)

(auto-insert-mode t)

(custom-set-variables
 '(auto-insert-alist '())
 '(auto-insert 'other)
 '(auto-insert-query nil)
 '(auto-insert-directory "~/.emacs.d/inserts/"))

(defun bs-inserts-reset ()
  "Make sure we have a sane bs-inserts list to work with"
  (setq auto-insert-alist '()))

(defun bs-inserts-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(defun bs-insert (regexp path)
  "Register an auto insert based on pattern and path"
  (let ((abs-path (expand-file-name (concat "~/.emacs.d/inserts/" path))))
    (unless (file-exists-p abs-path)
      (error "Snippet: %s (%s, %s) doesn't exist" abs-path path regexp))
    (add-to-list 'auto-insert-alist (cons regexp (vector abs-path 'bs-inserts-yas-expand)))))

(provide 'bs-inserts)
