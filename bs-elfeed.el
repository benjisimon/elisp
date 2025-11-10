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

(provide 'bs-elfeed)
