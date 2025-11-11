;;
;; elfeed helper functions
;;

(defun bs-elfeed-search-tag-all ()
  (interactive)
  (let* ((available-tags (elfeed-db-get-all-tags))
         (new-tag (completing-read "Tag: " available-tags nil 'confirm)))
    (elfeed-search-tag-all (intern new-tag))))

(defun bs-elfeed-search-untag-all ()
  (interactive)
  (let* ((available-tags (elfeed-db-get-all-tags))
         (new-tag (completing-read "Tag: " available-tags nil nil)))
    (elfeed-search-untag-all (intern new-tag))))

(defun bs-elfeed-db-sync ()
  "Sync changes to our databse to a git repo for elfeed for remote access."
  (message "elfeedassist: sync")
  (shell-command "elfeedassist -a sync"))

(advice-add 'elfeed :before 'bs-elfeed-db-sync)
(advice-add 'elfeed-search-quit-window :after 'bs-elfeed-db-sync)

(provide 'bs-elfeed)
