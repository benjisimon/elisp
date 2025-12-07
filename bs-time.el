;;
;; Ben Simon time related functions

(require 'cl-lib)

(defun bs-parse-duration (spec)
  "Given a specification of a duration, return a fractional number of hours"
  (interactive)
  ;; spec can be in the format:
  ;;  T, T, T
  ;; where T is:
  ;;  - simple number, like 3 or 3.4 - for 3 or 3.4 hours
  ;;  - a time  duration: 3a - 7pm  - that's the number of hours between
  ;;     3:00 and 19:00
  ;;  - a time duration with minutes: 3.20a - 7.15p - that's
  ;;     3:20  to 19:15
  ;;  - military time:  5:00 - 19:02
  ;; The result is the sum of all the hours. So:
  ;;   2, 12p - 2p, 3:00 - 5:00
  ;; would be: 2 + 2 + 2 or 6 hours, or just the value 6.
  (let ((parts (split-string spec "," t "[ \t\n]+")))
    (apply #'+
           (mapcar
            (lambda (part)
              (if (string-match "\\([0-9:.]+[ap]?m?\\)[ \t]*-[ \t]*\\([0-9:.]+[ap]?m?\\)" part)
                  ;; Time range
                  (let ((start (bs--parse-time (match-string 1 part)))
                        (end (bs--parse-time (match-string 2 part))))
                    (- end start))
                ;; Simple number
                (string-to-number part)))
            parts))))

(defun bs-sum-durations (specs)
  "Add up durations as defined by the list of specifications."
  (interactive)
  (seq-reduce (lambda (carry spec)
                (+ carry (bs-parse-duration spec)))
              specs 0))


(defun bs--parse-time (time-str)
  "Parse a time string to hours as a float."
  (let* ((am-pm (cond ((string-match-p "[ap]m?$" time-str)
                       (substring time-str -1))
                      (t nil)))
         (num-str (replace-regexp-in-string "[ap]m?$" "" time-str))
         (parts (split-string num-str "[.:]"))
         (hours (string-to-number (car parts)))
         (minutes (if (cdr parts) (string-to-number (cadr parts)) 0)))
    ;; Adjust for AM/PM
    (when (and am-pm (string= am-pm "p") (< hours 12))
      (setq hours (+ hours 12)))
    (when (and am-pm (string= am-pm "a") (= hours 12))
      (setq hours 0))
    ;; Convert to fractional hours
    (+ hours (/ minutes 60.0))))


(provide 'bs-time)
