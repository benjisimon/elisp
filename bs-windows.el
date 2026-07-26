;; -*- lexical-binding: t; -*-

(defvar bs-side-window-width-cycle '(0.5 0.25 0.75)
  "Fractions of frame width to cycle through.")

(defun bs-cycle-sidewindow-width (arg)
  "Cycle selected window width through fractions of frame width.
With C-u, prompt for a percentage instead."
  (interactive "P")
  (let* ((win (selected-window))
         (frame-w (frame-width))
         (target-w
          (if arg
              (floor (* frame-w (/ (read-number "Width %: ") 100.0)))
            (let* ((idx (or (window-parameter win 'bs-width-idx) 0))
                   (next-idx (mod (1+ idx) (length bs-side-window-width-cycle))))
              (set-window-parameter win 'bs-width-idx next-idx)
              (floor (* frame-w (nth next-idx bs-side-window-width-cycle))))))
         (delta (- target-w (window-total-width win))))
    (window-resize win delta t)))

(provide 'bs-windows)
