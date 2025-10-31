;;
;; elfeed helper functions
;;

(defun bs-elfeed-extras-tag-with-completion ()
  (interactive)
  (let* ((current-tags (elfeed-entry-tags elfeed-show-entry))
         (available-tags (delete-dups (append current-tags
                                              (elfeed-db-get-all-tags))))
         (new-tag (completing-read "Tag: " available-tags nil nil)))
    (elfeed-search-tag-all (list new-tag))))

(defun bs-elfeed-extras-untag-with-completion ()
  (interactive)
  (let* ((current-tags (elfeed-entry-tags elfeed-show-entry))
         (available-tags (delete-dups (append current-tags
                                              (elfeed-db-get-all-tags))))
         (new-tag (completing-read "Tag: " available-tags nil nil)))
    (elfeed-search-untag-all (list new-tag))))

(provide 'bs-elfeed-extras)
