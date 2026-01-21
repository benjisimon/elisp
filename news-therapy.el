;;; news-therapy.el --- Analyze news articles with LLM assistance -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: news, analysis, llm
;; URL: https://github.com/yourusername/news-therapy

;;; Commentary:

;; This package helps you critically analyze news articles by setting up
;; an org-mode document with the article attached and a prompt for LLM analysis.

;;; Code:

(require 'url)
(require 'org)
(require 'org-attach)

(defgroup news-therapy nil
  "Analyze news articles with LLM assistance."
  :group 'applications
  :prefix "news-therapy-")

(defcustom news-therapy-directory "~//news-therapy/"
  "Directory where news therapy files are stored."
  :type 'directory
  :group 'news-therapy)

(defcustom news-therapy-default-prompt
  "I'd like to discuss the news article found [[%s][here]].

Here is a local copy so you can read it: [[attachment:%s]]

Can you start off by summarizing this article in a few bullet points,
and pointing out any red flags that pop up in terms of bias or other
indications that this news content may not be reliable."
  "Default prompt template for news therapy.
First %s will be replaced with the URL, second %s with attachment filename."
  :type 'string
  :group 'news-therapy)

(defun news-therapy--extract-domain (url)
  "Extract domain from URL."
  (when (string-match "https?://\\([^/]+\\)" url)
    (match-string 1 url)))

(defun news-therapy--generate-filename (url)
  "Generate filename for URL based on current date and domain."
  (let* ((domain (news-therapy--extract-domain url))
         (date (format-time-string "%Y-%m-%d"))
         (filename (format "%s.%s.org" date domain)))
    (expand-file-name filename news-therapy-directory)))

(defun news-therapy--download-url (url filename)
  "Download URL content to FILENAME."
  (url-copy-file url filename t))

(defun news-therapy--setup-org-file (url filepath)
  "Set up org file at FILEPATH with URL content and prompt."
  (let ((html-file (concat (file-name-sans-extension filepath) ".html"))
        (domain (news-therapy--extract-domain url)))

    ;; Download the URL content
    (message "Downloading %s..." url)
    (condition-case err
        (news-therapy--download-url url html-file)
      (error (message "Failed to download URL: %s" (error-message-string err))))

    ;; Create the org file
    (with-current-buffer (find-file-noselect filepath)
      (erase-buffer)
      (insert (format "#+TITLE: News Therapy - %s\n" domain))
      (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
      (insert (format "* Article Analysis\n\n"))

      ;; Set up org-attach
      (org-attach-dir-get-create)

      ;; Attach the HTML file
      (when (file-exists-p html-file)
        (org-attach-attach html-file nil 'mv)
        (let ((attach-filename (file-name-nondirectory html-file)))
          ;; Use attachment: link so gptel can track it
          (insert (format news-therapy-default-prompt url attach-filename))))

      (save-buffer)
      (goto-char (point-max)))

    (find-file filepath)))

;;;###autoload
(defun news-therapy-start (url)
  "Start news therapy session for URL.
Downloads the article, creates an org file with attachment and prompt."
  (interactive "sNews article URL: ")
  (unless (string-match-p "^https?://" url)
    (user-error "Please provide a valid URL starting with http:// or https://"))

  ;; Ensure directory exists
  (unless (file-exists-p news-therapy-directory)
    (make-directory news-therapy-directory t))

  (let ((filepath (news-therapy--generate-filename url)))
    (if (file-exists-p filepath)
        (if (y-or-n-p (format "File %s already exists. Open it? "
                              (file-name-nondirectory filepath)))
            (find-file filepath)
          (user-error "Aborted"))
      (news-therapy--setup-org-file url filepath))))

(provide 'news-therapy)
;;; news-therapy.el ends here
