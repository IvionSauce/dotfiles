;;; Normal functions

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
 '(("C-w" . ivi-kill-region-or-word)))

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
