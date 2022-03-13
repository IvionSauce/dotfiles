;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;;; Off the cuff font stuff, with inspiration from:
;;; https://emacs.stackexchange.com/questions/60690/

(defvar ivi-font-scaling 1.0)
(defvar ivi-font-scaling-step 0.1)
(defvar ivi-font-default-size
  (/ (face-attribute 'default :height) 10.0))

(defvar ivi-font-density-scaling t
  "Whether or not to automatically scale your font(s) to keep
  things readable on monitors with a high pixel density.")
(defvar ivi-font-density-scaling-alist
  '((20 . 1.6)
    (18 . 1.4)
    (16 . 1.2)
    (0 . 1.0)))


(defun ivi-ratio (alist blist)
  "Computes the ratio between the product of two lists. This
ratio will be a floating point number."
  (/ (apply #'* alist)
     (float (apply #'* blist))))

(defun ivi-density-of-current-monitor ()
  "Get the pixel density of the monitor/display dominating the
current frame; this is the ratio between the resolution (in
pixels) and the physical dimensions (in mm) of the
display. Returned density will be a floating point number."
  (let* ((mon-attr (frame-monitor-attributes))
	 (resolution (nthcdr 3 (assq 'geometry mon-attr)))
	 (dimensions (cdr (assq 'mm-size mon-attr))))
    (ivi-ratio resolution dimensions)))


;; Keep track of the font size, such that we only spit out a message
;; when the size is actually changing.
(let ((current-font-size (face-attribute 'default :height)))
  (defun ivi-change-font-size (new-size)
    "Change the size of the default font."
    (let ((rounded-size (round (* 10 new-size))))
      (unless (= rounded-size current-font-size)
	(setq current-font-size rounded-size)
	(set-face-attribute 'default nil :height rounded-size)
	(message "Font size set to: %s" (/ rounded-size 10.0))))
    current-font-size))


(defun ivi-density-scale ()
  "If `ivi-font-density-scaling' evaluates to true, return scale
according to `ivi-font-density-scaling-alist'. Otherwise return
1.0."
  (if ivi-font-density-scaling
      (let ((density (ivi-density-of-current-monitor)))
	(cl-dolist (cell ivi-font-density-scaling-alist)
	  (when (> density (car cell))
	    (cl-return (cdr cell)))))
    1.0))

(defun ivi-scale-font (&optional frame)
  "Scale default font according to `ivi-font-scaling' and the
pixel density of the current monitor/display."
  (ivi-change-font-size (* ivi-font-default-size
			   ivi-font-scaling
			   (ivi-density-scale))))


(defun ivi-font-toggle-density-scaling ()
  "Toggles pixel density font-scaling on/off, by manipulating
`ivi-font-density-scaling'."
  (interactive)
  (setq ivi-font-density-scaling (not ivi-font-density-scaling))
  (let ((msg "Automatic pixel density font-scaling")
	;; Both scales the font and returns the font size.
	(size (/ (ivi-scale-font) 10.0)))
    (if ivi-font-density-scaling
	(message "%s [On]  Font size: %s" msg size)
      (message "%s [Off]  Font size: %s" msg size))))

;; Keep track of the steps taken in either direction, this allows us
;; to easily undo the steps and return to neutral.
(let ((steps-delta 0))
  (defun ivi-font-size+ ()
    "Increase font scaling factor by `ivi-font-scaling-step'."
    (interactive)
    (setq
     ivi-font-scaling (+ ivi-font-scaling ivi-font-scaling-step)
     steps-delta (+ steps-delta ivi-font-scaling-step))
    (ivi-scale-font))

  (defun ivi-font-size- ()
    "Decrease font scaling factor by `ivi-font-scaling-step'."
    (interactive)
    (setq
     ivi-font-scaling (- ivi-font-scaling ivi-font-scaling-step)
     steps-delta (- steps-delta ivi-font-scaling-step))
    (ivi-scale-font))

  (defun ivi-font-size-reset ()
    "Undo all increases/decreases of the font scaling factor."
    (interactive)
    (setq
     ivi-font-scaling (- ivi-font-scaling steps-delta)
     steps-delta 0)
    (ivi-scale-font)))

(defun ivi-font-force-size (font-size)
  "Forcibly change font size; this also turns off automatic pixel
density font-scaling so it doesn't interfere."
  (interactive "nNew font size: ")
  (setq
   ivi-font-density-scaling nil
   ivi-font-default-size font-size)
  (ivi-font-size-reset))


(ivi-keys
 '(("M-c s" . ivi-font-force-size)
   ("M-c M-s" . ivi-font-toggle-density-scaling)
   ("s--" . ivi-font-size-)
   ("s-=" . ivi-font-size+)
   ("C-M-0" . ivi-font-size-reset)))

(add-hook 'window-size-change-functions 'ivi-scale-font)
(add-hook 'focus-in-hook 'ivi-scale-font)

(provide 'ivi-font-scaling)
