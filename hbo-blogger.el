;;
;; hbo-blogger - Half Baked Opportunistic Blogger interface.
;; Basically, what happens when you realize you can use
;; the oauth2 module to trivially make requests to the blogger API.
;;
;; These are some half baked functions to do easy stuff with blogger. 
;;

(require 'plstore)
(require 'oauth2)
(require 'epa-file)
(require 'json)
(require 'oauth2handler)
(require 'uuid)
u
(defvar hbo-blogger-posts-dir
  (format "%s%s" user-emacs-directory "posts"))

(defvar hbo-blogger-client-id nil)
(defvar hbo-blogger-client-secret nil)
(defvar hbo-blogger-oauth-scope
  "https://www.googleapis.com/auth/blogger")
(defvar hbo-blogger-oauth-redirect-url nil)
(defvar hbo-blogger-oauth-auth-url
  "https://accounts.google.com/o/oauth2/auth")
(defvar hbo-blogger-oauth-token-url
  "https://www.googleapis.com/oauth2/v3/token")

(defvar hbo-blogger-token nil)

(defun hbo-blogger-auth-start ()
  "Start the Oauth2 authentication process in the browser"
  (interactive)
  (oauth2handler-start-auth
   hbo-blogger-oauth-redirect-url
   hbo-blogger-oauth-auth-url
   hbo-blogger-oauth-token-url
   hbo-blogger-oauth-scope
   hbo-blogger-client-id
   hbo-blogger-client-secret))

(defun hbo-blogger-auth-setup ()
  "Set us up to be authenticated or trigger an error saying we need a setup."
  (interactive)
  (if (oauth2handler-setup-p hbo-blogger-oauth-auth-url
                             hbo-blogger-oauth-token-url
                             hbo-blogger-oauth-scope)
      (setq hbo-blogger-token (oauth2-auth-and-store
                               hbo-blogger-oauth-auth-url
                               hbo-blogger-oauth-token-url
                               hbo-blogger-oauth-scope
                               hbo-blogger-client-id
                               hbo-blogger-client-secret))
    (error "Authentication not yet set up for hbo-blogger. Invoke: M-x hbo-blogger-auth-start.")))


(defun hbo-blogger-verify-response (response)
  "Check a response to make sure it's valid"
  (let ((e (gethash "error" response)))
    (if e
        (error (gethash "message" e))
      response)))

(defun hbo-blogger-invoke (method url headers data)
  "General purpose blogger API request"
  (let ((buffer
         (oauth2-url-retrieve-synchronously
          hbo-blogger-token url
          method data headers)))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (hbo-blogger-verify-response (json-parse-buffer)))))

(defun hbo-blogger-get (url)
  "internal function to make http GET's to blogger easier"
  (hbo-blogger-invoke "GET" url nil nil))

(defun hbo-blogger-put (url content)
  "invoke a PUT against blogger's API"
  (hbo-blogger-invoke "PUT" url  '(("Content-Type" . "application/json")) (encode-coding-string (json-encode content) 'utf-8)))

(defun hbo-blogger-get-post (blog-id post-id)
  "Retrieve a post object from blogger."
  (hbo-blogger-get (format "https://www.googleapis.com/blogger/v3/blogs/%s/posts/%s?view=admin"
                           blog-id post-id)))

(defun hbo-blogger-users-blogs ()
  "Get an alist of all user's blogs."
  (let ((doc   (hbo-blogger-get "https://www.googleapis.com/blogger/v3/users/self/blogs")))
    (mapcar (lambda (item)
              (cons (gethash "id" item)
                    (gethash "url" item)))
            (gethash "items" doc))))

(defun hbo-blogger-blog-id-from-url (blog-url)
  (let ((doc (hbo-blogger-get
              (concat
               "https://www.googleapis.com/blogger/v3/blogs/byurl?"
               (url-build-query-string `(("url" ,blog-url)))))))
    (gethash "id" doc)))

(defun hbo-blogger-post-blog-id (post)
  (gethash "id" (gethash "blog" post)))

(defun hbo-blogger-post-id (post)
  (gethash "id" post))

(defun hbo-blogger-list-posts (blog-url params)
  "Get a list of recent posts given a blog URL."
  (let* ((id (hbo-blogger-blog-id-from-url blog-url))
         (posts-url (format "https://www.googleapis.com/blogger/v3/blogs/%s/posts?%s"
                            id (url-build-query-string params))))
    (let ((doc (hbo-blogger-get posts-url)))
      (gethash "items" doc))))

(defun hbo-blogger-edit-post (post)
  "Capture a post locally and start to edit it"
  (let* ((blog-id (hbo-blogger-post-blog-id post))
         (post-id (hbo-blogger-post-id post))
         (b (get-buffer-create (format "%s.%s.html" blog-id post-id))))
    (set-buffer b)
    (erase-buffer)
    (insert (gethash "content" post))
    (goto-char (point-min))
    (web-mode)
    (auto-fill-mode)
    (flyspell-mode)
    (local-set-key (kbd "C-x t p") 'hbo-blogger-preview-buffer)
    (write-file (format "%s/%s" hbo-blogger-posts-dir (buffer-name)))
    (add-hook 'after-save-hook 'hbo-blogger-save-buffer 0 t)
    (switch-to-buffer b)))

(defun hbo-blogger-edit-latest-draft-post (blog-url)
  (let ((post (elt (hbo-blogger-list-posts blog-url
                                           `(("status" "draft")
                                             ("maxResults" "1")
                                             ("orderBy" "updated"))) 0)))
    (hbo-blogger-edit-post post)))

(defun hbo-blogger-edit-latest-published-post (blog-url)
  (let ((post (elt (hbo-blogger-list-posts blog-url
                                           `(("status" "live")
                                             ("maxResults" "1")
                                             ("orderBy" "updated"))) 0)))
    (hbo-blogger-edit-post post)))


(defun hbo-blogger-buffer-name-to (what)
  (let ((parts (split-string (buffer-name) "[.]")))
    (pcase what
      ('blog-id (elt parts 0))
      ('post-id (elt parts 1)))))
                                  

(defun hbo-blogger-preview-buffer ()
  (interactive)
  (let ((blog-id (hbo-blogger-buffer-name-to 'blog-id))
        (post-id (hbo-blogger-buffer-name-to 'post-id)))
    (browse-url (format "https://draft.blogger.com/blog/post/preview/%s/%s"
                        blog-id post-id))))

(defun hbo-blogger-save-buffer ()
  (interactive)
  (let* ((blog-id (hbo-blogger-buffer-name-to 'blog-id))
         (post-id (hbo-blogger-buffer-name-to 'post-id))
         (post    (hbo-blogger-get-post blog-id post-id ))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (url     (format "https://www.googleapis.com/blogger/v3/blogs/%s/posts/%s"
                          blog-id post-id))
         (payload `((kind    . "blogger#post")
                    (id      . ,post-id)
                    (blog    . ((id . ,blog-id)))
                    (content . ,content)
                    (url     . ,(gethash "url" post))
                    (published . ,(gethash "published" post))
                    (author  . ((id . ,(gethash "id" (gethash "author" post)))))
                    (labels  . ,(gethash "labels" post))
                    (title   . ,(gethash "title" post)))))
    (hbo-blogger-put url payload)))


(defun hbo-blogger-proofread (start end)
  "Proofread either the current buffer or region using ChatGPT magic."
  (interactive "r")
  (when (not (use-region-p))
    (error "No region selected"))
  (let ((marker (format "[proof]"))
        (setup (concat "You are a blog proof reader. Your job is to "
                       "take in HTML text and generate a version of that text "
                       "that is free from spelling, grammer and other "
                       "significant mistakes. "
                       "If you make corrections in a paragraph you should "
                       "prefix the paragraph with an HTML comment with the following"
                       "structure:\n"
                       "<!-- PROOF: \n"
                       " (description of the changes formatted via markdown)\n"
                       "\n"
                       " (description of suggestions for how the text could be improved "
                       " formatted in markdown)\n"
                       "-- END FIXES --\n"
                       "\n"
                       "There should be one blank line between the content and the "
                       "fixes, and two blank lines after the fixes comment.")))
    (insert marker)))

(provide 'hbo-blogger)
