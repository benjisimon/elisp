;;
;; Code to support working with a oauth2handler, a general purpose
;; oauth endpoint
;;
;; See: https://www.blogbyben.com/2023/09/taming-youtube-part-2-replacing-oob.html
;;
;;

(require 'oauth2)

(defun oauth2handler-start-auth (redirect-url auth-url token-url scope client-id client-secret username hostname)
  "Given oauth2 parameters, launch a browser interact withoauth2handler.
This page will ultimately generate JSON which you should paste into the buffer
shown to the user."
  (let ((url (format "%s?client_id=%s&redirect_uri=%s&scope=%s&response_type=code&access_type=offline&approval_prompt=force"
                     auth-url
                     (url-encode-url client-id)
                     (url-encode-url redirect-url)
                     (url-encode-url scope))))
    (browse-url url)
    (oauth2handler-prepare-json-buffer auth-url token-url scope client-id client-secret username hostname)
    nil))

(defun oauth2handler-prepare-json-buffer (auth-url token-url scope client-id client-secret username hostname)
  "Create a buffer to catch the JSON shown in the browser."
  (switch-to-buffer (make-temp-name "*oauth2handler*"))
  (insert ";; Paste oauth2handler JSON below and press C-c C-c\n\n")
  (local-set-key (kbd "C-c C-c") 'oauth2handler-save-json)
  (setq-local oa2h-redirect-url redirect-url
              oa2h-auth-url auth-url
              oa2h-token-url token-url
              oa2h-scope scope
              oa2h-client-id client-id
              oa2h-client-secret client-secret
              oa2h-username username
              oa2h-hostname hostname))


(defun oauth2handler-setup-p (auth-url token-url scope client-id username)
  (let* ((id (oauth2-compute-id auth-url token-url scope client-id username))
         (plstore (plstore-open oauth2-token-file))
         (plist (plstore-get plstore id)))
    plist))

(defun oauth2handler-save-json ()
  "Process the JSON pasted into the buffer"
  (interactive)
  (let* ((json (json-parse-string (replace-regexp-in-string "^;;.*\n" "" (buffer-string)) :object-type 'alist))
         (access-token (cdr (assoc "access_token" json)))
         (refresh-token (cdr (assoc "refresh_token" json)))
         (plstore (plstore-open oauth2-token-file))
         (id (oauth2-compute-id oa2h-auth-url oa2h-token-url oa2h-scope oa2h-client-id oa2h-username))
         (request-cache (oauth2--update-request-cache oa2h-hostname
                                                      access-token
                                                      (oauth2--current-timestamp)))
         (token (make-oauth2-token
                 :client-id oa2h-client-id
                 :client-secret oa2h-client-secret
                 :access-token access-token
                 :request-cache request-cache
                 :code-verifier ""
                 :refresh-token refresh-token
                 :token-url oa2h-token-url
                 :access-response json)))
    (plstore-put plstore id nil
                 `(:request-cache
                   ,(oauth2-token-request-cache token)
                   :code-verifier
                   ,(oauth2-token-code-verifier token)
                   :access-response
                   ,(oauth2-token-access-response token)))
    (plstore-save plstore)
    (kill-buffer (buffer-name))
    token))

(provide 'oauth2handler)
