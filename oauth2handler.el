;;
;; Code to support working with a oauth2handler, a general purpose
;; oauth endpoint
;;
;; See: https://www.blogbyben.com/2023/09/taming-youtube-part-2-replacing-oob.html
;;
;;

(defun oauth2handler-start-google-auth (client-id redirect-url scope)
  (let ((url (format "https://accounts.google.com/o/oauth2/auth?client_id=%s&redirect_uri=%s&scope=%s&response_type=code&access_type=offline&approval_prompt=force"
                     (url-encode-url client-id)
                     (url-encode-url redirect-url)
                     (url-encode-url scope))))
    (browse-url url)
    ;; and open up buffer to capture JSON output
    ))
