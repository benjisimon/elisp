;;; bs-workday.el --- Weekly Work Day planning  -*- lexical-binding: t; -*-

(require 'org)
(require 'subr-x)

(defvar bs-workday-plan-directory "~/dt/i2x/project-notes/src/main/tenten/main"
  "Directory holding one weekly plan org file per Sunday.")

(defconst bs-workday-output-buffer "*workday-plan*"
  "Buffer showing what `workdayassist' would do, or did, to the calendar.")

(defun bs-workday--command ()
  "Path of the workdayassist script."
  (or (executable-find "workdayassist")
      (expand-file-name "~/bin/workdayassist")))

(defun bs-workday--run (args on-done)
  "Run workdayassist with ARGS in the background.
ON-DONE is called with the exit status and the captured output once the
process finishes."
  (let ((output (generate-new-buffer " *workday-run*")))
    (make-process
     :name "workdayassist"
     :buffer output
     :command (cons (bs-workday--command) args)
     :noquery t
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (let ((status (process-exit-status process))
               (text (with-current-buffer output (buffer-string))))
           (kill-buffer output)
           (funcall on-done status text)))))))

(defun bs-workday--plan-file (&optional date)
  "Path of the weekly plan file for DATE, a YYYY-MM-DD string (default today)."
  (expand-file-name (format "%s.org" (or date (format-time-string "%Y-%m-%d")))
                    bs-workday-plan-directory))

(defun bs-workday--insert-page (buffer status text)
  "Put the generated page TEXT into BUFFER, or report the failure STATUS."
  (cond
   ((not (buffer-live-p buffer)) nil)
   ((/= status 0)
    (message "workdayassist org-page failed (%d): %s" status (string-trim text)))
   (t
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (insert text)))
    (message "Weekly plan ready"))))

;;;###autoload
(defun bs-workday-new-plan ()
  "Open this week's plan file, filling in a fresh plan when it is new.
The TODOs take a while to gather, so they arrive in the background."
  (interactive)
  (let ((buffer (find-file (bs-workday--plan-file))))
    (if (> (buffer-size buffer) 0)
        (message "Plan already started")
      (message "Generating weekly plan...")
      (bs-workday--run
       '("-a" "org-page")
       (lambda (status text)
         (bs-workday--insert-page buffer status text))))))

(defun bs-workday--show (text)
  "Display TEXT in the workday output buffer."
  (with-current-buffer (get-buffer-create bs-workday-output-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (display-buffer (current-buffer))))

(defun bs-workday--plan-args (mode)
  "Arguments for add-from-plan on the current buffer's plan, in MODE."
  (let ((date (file-name-base (buffer-file-name))))
    (unless (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date)
      (user-error "Not a dated plan file: %s" (buffer-file-name)))
    (list "-a" "add-from-plan" "-d" date "-f" (buffer-file-name) "-m" mode)))

(defun bs-workday--save-plan-to-calendar (args)
  "Apply the plan to the calendar using ARGS and show the result."
  (bs-workday--run
   args
   (lambda (status text)
     (bs-workday--show text)
     (message (if (= status 0) "Plan saved to the Work Day calendar" "Saving the plan failed")))))

;;;###autoload
(defun bs-workday-preview-plan ()
  "Show what the current plan would add to or delete from the calendar."
  (interactive)
  (org-table-recalculate-buffer-tables)
  (save-buffer)
  (bs-workday--run
   (bs-workday--plan-args "preview")
   (lambda (_status text) (bs-workday--show text))))

;;;###autoload
(defun bs-workday-commit-plan ()
  "Preview the current plan, then apply it to the calendar if confirmed."
  (interactive)
  (org-table-recalculate-buffer-tables)
  (save-buffer)
  (let ((save-args (bs-workday--plan-args "save")))
    (bs-workday--run
     (bs-workday--plan-args "preview")
     (lambda (status text)
       (bs-workday--show text)
       (if (/= status 0)
           (message "Plan has errors; nothing was changed")
         (when (y-or-n-p "Apply these changes to the Work Day calendar? ")
           (bs-workday--save-plan-to-calendar save-args)))))))

(provide 'bs-workday)
