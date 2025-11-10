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
  (elfeed-db-unload)
  (shell-command "elfeedassist -a sync"))

(defun bs-elfeed ()
  "Kick off elfeed but peform a sync first"
  (interactive)
  (bs-elfeed-db-sync)
  (elfeed))

(defun bs-elfeed-search-quit ()
  "Quit elfeed, but also do a sync"
  (interactive)
  (bs-elfeed-db-sync)
  (elfeed-search-quit-window))

(provide 'bs-elfeed)
