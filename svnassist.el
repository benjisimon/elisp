;;
;; Wrapper code around the svnassist command line tool I've written
;; for myself
;;
(require 'dsvn)

(defun svnassist-switch-release ()
  "Switch the current repo to current release"
  (interactive)
  (let ((root (locate-dominating-file default-directory ".svn")))
    (let ((default-directory root))
      (shell-command "svnassist switch-release"))))


(defun svnassist-merge-release ()
  "Merge the release into the current directory"
  (interactive)
  (let ((root (locate-dominating-file default-directory ".svn")))
    (let ((default-directory root))
      (shell-command "svnassist merge-release"))))


(defun svnassist-merge-trunk ()
  "Merge the trunk into the current directory"
  (interactive)
  (let ((root (locate-dominating-file default-directory ".svn")))
    (let ((default-directory root))
      (shell-command "svnassist merge-in-trunk"))))

(defun svnassist-review-branch (url)
  (interactive (list (completing-read "Switch to (URL): "
                                      'svn-complete-url
                                      nil nil
                                      (svn-current-url)
                                      'svn-switch-history)))
  (let ((branch (cond ((string-match "^.*/branches/\\([^/]*\\)[/]?" url)
                       (match-string 1 url))
                      (t (error "Can't find branch in: %s" url))))
        (root (locate-dominating-file default-directory ".svn")))
    (let ((default-directory root))
      (shell-command (format "svnassist review-branch %s" branch)))))

(defun svnassist-show-issue ()
  "What issue does this branch fix?"
  (interactive)
  (let ((root (locate-dominating-file default-directory ".svn")))
    (let ((default-directory root))
      (shell-command "svnassist show-issue"))))

(defun svnassist-qlog ()
  "quick log of our directory"
  (interactive)
  (let ((root (locate-dominating-file default-directory ".svn")))
    (let ((default-directory root))
      (let ((output (shell-command-to-string "svnassist qlog")))
        (switch-to-buffer "*qlog*")
        (erase-buffer)
        (insert output)
        (beginning-of-buffer)))))

(provide 'svnassist)
