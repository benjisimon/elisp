;;
;; Define our auto inserts
;;

(setq auto-insert-query nil)
(setq auto-insert-alist '())
(define-auto-insert "[.]php"
  '(\n
    "<?php" \n
    "/*" \n
    " * A PHP file for " _ \n
    " */" \n))

(define-auto-insert "[.]js"
  '(\n
    "/*" \n
    " * A JS file for " _ \n
    " */" \n))

(define-auto-insert "[.]css"
  '(\n
    "/*" \n
    " * A CSS file for " _ \n
    " */" \n))

(define-auto-insert "[.]el"
  '(\n
    ";;;" \n
    ";;; elisp to  " _ \n
    ";;;" \n))

(provide 'bs-inserts)
