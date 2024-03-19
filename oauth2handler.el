;;
;; Code to support working with a oauth2handler, a general purpose
;; oauth endpoint
;;
;; See: https://www.blogbyben.com/2023/09/taming-youtube-part-2-replacing-oob.html
;;
;;

(defun oauth2handler-start-google-auth (client-id redirect-url scope)
  "Given oauth2 parameters, launch a browser interact withoauth2handler.
This page will ultimately generate JSON which you should paste into the buffer
shown to the user."
  (let ((url (format "https://accounts.google.com/o/oauth2/auth?client_id=%s&redirect_uri=%s&scope=%s&response_type=code&access_type=offline&approval_prompt=force"
                     (url-encode-url client-id)
                     (url-encode-url redirect-url)
                     (url-encode-url scope))))
    (browse-url url)
    (oauth2handler-prepare-json-buffer)))

(defun oauth2handler-prepare-json-buffer ()
  "Create a buffer to catch the JSON shown in the browser."
  (switch-to-buffer (make-temp-name "*oauth2handler*"))
  (insert ";; Paste oauth2handler JSON below and press C-c C-c\n\n")
  (local-set-key (kbd "C-c C-c") 'oauth2handler-save-json))

(defun oauth2handler-save-json ()
  "Process the JSON pasted into the buffer"
  (interactive)
  (let* ((json (json-parse-string (replace-regexp-in-string "^;;.*\n" "" (buffer-string))))
         (token (oauth2-auth
                 auth-url token-url client-id client-secret scope state redirect-uri)))
    ;; XXX not implemented
    ;; save json in plstore.
    ;; This will let futur calls to auth2-auth-and-store Just Work.
    ;; Grab from oauth2-auth-and-store:
    ;;   (let* ((plstore (plstore-open oauth2-token-file))
    ;;     (id (oauth2-compute-id auth-url token-url scope))
    ;; with a token, plstore and id, we can spike the plstore to have
    ;; our token in it for the next call to
    ;; oauth2-auth-and-store
    ;;
    (setf (oauth2-token-plstore token) plstore)
    (setf (oauth2-token-plstore-id token) id)
    (plstore-put plstore id nil `(:access-token
                                  ,(oauth2-token-access-token token)
                                  :refresh-token
                                  ,(oauth2-token-refresh-token token)
                                  :access-response
                                  ,(oauth2-token-access-response token)))
    (plstore-save plstore)))
