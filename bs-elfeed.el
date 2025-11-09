;;
;; elfeed helper functions
;;

(defun bs-elfeed-search-tag-all ()
  (interactive)
  (let* ((available-tags (elfeed-db-get-all-tags))
         (new-tag (completing-read "Tag: " available-tags nil nil)))
    (elfeed-search-tag-all new-tag)))

(defun bs-elfeed-search-untag-all ()
  (interactive)
  (let* ((current-tags (elfeed-entry-tags elfeed-show-entry))
         (available-tags (delete-dups (append current-tags
                                              (elfeed-db-get-all-tags))))
         (new-tag (completing-read "Tag: " available-tags nil nil)))
    (elfeed-search-untag-all (list new-tag))))

(provide 'bs-elfeed)
