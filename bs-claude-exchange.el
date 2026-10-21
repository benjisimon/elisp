;;; bs-claude-exchange.el --- Read Claude Code replies outside the terminal  -*- lexical-binding: t; -*-

;; A long Claude Code reply lands in a terminal buffer whose scrollback
;; fights with typing.  This shows one exchange -- the prompt and the
;; reply -- as markdown in an ordinary buffer.

(require 'project)
(require 'markdown-mode)

(defcustom bs-claude-exchange-command '("claudeassist" "-a" "exchange")
  "Command that prints a Claude Code exchange as markdown.
It is run with \"-d DIR -n INDEX\" appended, where DIR is the session's
project directory and INDEX counts back from the most recent exchange,
0 being the latest.  It should exit non-zero when there is no such
exchange."
  :type '(repeat string)
  :group 'tools)

(defvar-local bs-claude-exchange-directory nil
  "Project directory of the session this buffer shows.")

(defvar-local bs-claude-exchange-index 0
  "How many exchanges back from the latest this buffer shows.")

(defvar-keymap bs-claude-exchange-mode-map
  :parent gfm-view-mode-map
  "p" #'bs-claude-exchange-older
  "n" #'bs-claude-exchange-newer
  "g" #'bs-claude-exchange-refresh
  "q" #'quit-window)

(define-derived-mode bs-claude-exchange-mode gfm-view-mode "Claude-Exchange"
  "Read-only view of one Claude Code exchange.
\\{bs-claude-exchange-mode-map}"
  (visual-line-mode 1))

(defun bs-claude-exchange--directory ()
  "Return the project directory a Claude Code session here would run in."
  (if-let ((project (project-current)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun bs-claude-exchange--fetch (directory index)
  "Return the markdown of exchange INDEX for DIRECTORY's session.
Signal a `user-error' carrying the command's message when it fails."
  (with-temp-buffer
    (let ((status (apply #'process-file (car bs-claude-exchange-command) nil t nil
                         (append (cdr bs-claude-exchange-command)
                                 (list "-d" directory
                                       "-n" (number-to-string index))))))
      (unless (eql status 0)
        (user-error "%s" (string-trim (buffer-string))))
      (buffer-string))))

(defun bs-claude-exchange--show (index)
  "Replace this buffer's contents with exchange INDEX."
  (let ((text (bs-claude-exchange--fetch bs-claude-exchange-directory index))
        (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (setq bs-claude-exchange-index index)
    (setq header-line-format
          (if (zerop index)
              "Latest exchange"
            (format "%d exchange%s back" index (if (= index 1) "" "s"))))))

;;;###autoload
(defun bs-claude-exchange (&optional index)
  "Show the latest exchange of this project's Claude Code session.
With a numeric prefix argument INDEX, show the one INDEX exchanges back."
  (interactive "P")
  (let* ((directory (bs-claude-exchange--directory))
         (buffer (get-buffer-create
                  (format "*claude-exchange[%s]*"
                          (abbreviate-file-name
                           (directory-file-name directory))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'bs-claude-exchange-mode)
        (bs-claude-exchange-mode))
      (setq bs-claude-exchange-directory directory)
      (bs-claude-exchange--show (prefix-numeric-value (or index 0))))
    (pop-to-buffer buffer)))

(defun bs-claude-exchange-older ()
  "Show the exchange before this one."
  (interactive)
  (bs-claude-exchange--show (1+ bs-claude-exchange-index)))

(defun bs-claude-exchange-newer ()
  "Show the exchange after this one."
  (interactive)
  (if (zerop bs-claude-exchange-index)
      (user-error "Already at the latest exchange")
    (bs-claude-exchange--show (1- bs-claude-exchange-index))))

(defun bs-claude-exchange-refresh ()
  "Show the latest exchange again, picking up any new reply."
  (interactive)
  (bs-claude-exchange--show 0))

(provide 'bs-claude-exchange)
