;;; bs-frames.el --- Additions to frames  -*- lexical-binding: t; -*-

(require 'seq)

(defgroup bs-frames nil
  "Frame tweaks."
  :group 'frames)

(defcustom bs-frame-colors
  '("#e02b20" "#f07b12" "#f2d024" "#2eb82e" "#1e90ff" "#7c4dff" "#e75cff")
  "Colors `bs-frame-color' offers, handed out in this order.
Frames take a color as they are created, so creating them left to
right walks the spectrum left to right."
  :type '(repeat color)
  :group 'bs-frames)

(defcustom bs-frame-color-width 8
  "Width in pixels of the border `bs-frame-color' draws."
  :type 'integer
  :group 'bs-frames)

(defun bs-frame-color-unused ()
  "Return the first of `bs-frame-colors' that no frame wears."
  (let ((worn (mapcar (lambda (frame) (frame-parameter frame 'bs-frame-color))
                      (frame-list))))
    (or (seq-find (lambda (color) (not (member color worn))) bs-frame-colors)
        (car bs-frame-colors))))

;;;###autoload
(defun bs-frame-color (color &optional frame)
  "Border FRAME in COLOR, so it can be told apart at a glance.

FRAME defaults to the selected frame.  Interactively, read a color
from `bs-frame-colors', defaulting to one no other frame wears."
  (interactive (list (completing-read "Color: " bs-frame-colors
                                      nil nil nil nil (bs-frame-color-unused))))
  (let ((frame (or frame (selected-frame))))
    (set-frame-parameter frame 'bs-frame-color color)
    (set-frame-parameter frame 'internal-border-width bs-frame-color-width)
    (set-face-attribute 'internal-border frame :background color)))

;;;###autoload
(defun bs-frame-color-auto (&optional frame)
  "Border FRAME in a color no other frame wears, unless it wears one already.

Suitable for `after-make-frame-functions', which passes the new frame as
FRAME.  FRAME defaults to the selected frame."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (unless (frame-parameter frame 'bs-frame-color)
      (bs-frame-color (bs-frame-color-unused) frame))))

(provide 'bs-frames)
