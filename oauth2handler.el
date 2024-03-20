;;
;; Code to support working with a oauth2handler, a general purpose
;; oauth endpoint
;;
;; See: https://www.blogbyben.com/2023/09/taming-youtube-part-2-replacing-oob.html
;;
;;

(require 'oauth2)

(defun oauth2handler-start-google-auth (redirect-url auth-url token-url scope client-id client-secret)
  "Given oauth2 parameters, launch a browser interact withoauth2handler.
This page will ultimately generate JSON which you should paste into the buffer
shown to the user."
  (let ((url (format "%s?client_id=%s&redirect_uri=%s&scope=%s&response_type=code&access_type=offline&approval_prompt=force"
                     auth-url
                     (url-encode-url client-id)
                     (url-encode-url redirect-url)
                     (url-encode-url scope))))
    (browse-url url)
    (oauth2handler-prepare-json-buffer auth-url token-url scope client-id client-secret)
    nil))

(defun oauth2handler-prepare-json-buffer (auth-url token-url scope client-id client-secret)
  "Create a buffer to catch the JSON shown in the browser."
  (switch-to-buffer (make-temp-name "*oauth2handler*"))
  (insert ";; Paste oauth2handler JSON below and press C-c C-c\n\n")
  (local-set-key (kbd "C-c C-c") 'oauth2handler-save-json)
  (setq-local oa2h-redirect-url redirect-url
              oa2h-auth-url auth-url
              oa2h-token-url token-url
              oa2h-scope scope
              oa2h-client-id client-id
              oa2h-client-secret client-secret))


(defun oauth2handler-save-json ()
  "Process the JSON pasted into the buffer"
  (interactive)
  (let* ((json (json-parse-string (replace-regexp-in-string "^;;.*\n" "" (buffer-string))))
         (access-token (gethash "access_token" json))
         (refresh-token (gethash "refresh_token" json))
         (plstore (plstore-open oauth2-token-file))
         (id (oauth2-compute-id oa2h-auth-url oa2h-token-url oa2h-scope))
         (token (make-oauth2-token
                 :client-id oa2h-client-id
                 :client-secret oa2h-client-secret
                 :access-token access-token
                 :refresh-token refresh-token
                 :token-url oa2h-token-url
                 :access-response json
                 :plstore plstore
                 :plstore-id id)))
        (plstore-put plstore id nil `(:access-token
                                      ,(oauth2-token-access-token token)
                                      :refresh-token
                                      ,(oauth2-token-refresh-token token)
                                      :access-response
                                      ,(oauth2-token-access-response token)))
        (plstore-save plstore)
        (kill-buffer (buffer-name))
        token))

(proivde 'oauth2handler)
