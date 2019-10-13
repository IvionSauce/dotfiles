;;; Normal functions

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (- arg 1))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun ivi-kill-region-or-word ()
  "When called interactively with no active region, kill word
backwards. Else kill active region."
  (interactive)

  (if mark-active
      (kill-region (region-beginning) (region-end))
    (kill-word -1)))

(defun isearch-exit-at-front ()
  "Always exit isearch at the front of search match."
  (interactive)
  (isearch-exit)
  (when isearch-forward
    (goto-char isearch-other-end)))

(defun isearch-exit-at-end ()
  "Always exit isearch at the end of search match."
  (interactive)
  (isearch-exit)
  (when (not isearch-forward)
    (goto-char isearch-other-end)))

;; Keybindings

(ivi-keys
 '(("C-a" . prelude-move-beginning-of-line)
   ("C-w" . ivi-kill-region-or-word)))

(ivi-keys-with-map
 isearch-mode-map
 '(("<return>" . isearch-exit-at-front)
   ("C-<return>" . isearch-exit-at-end)))

;;; Advices

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(provide 'custom-funcs)
