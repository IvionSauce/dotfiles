;; Enable Recentf mode
(recentf-mode 1)
(setq recentf-max-menu-items 42
      recentf-max-saved-items 420)

;; Show column number in modeline
(column-number-mode 1)
;; No blinking cursor
(blink-cursor-mode 0)
;; Highlight closing-opening character pairs
(show-paren-mode 1)
;; Automatically balance parentheses, quotes, etc
(electric-pair-mode 1)
;; Overwrite selection if active
(delete-selection-mode 1)
;; Automatically revert buffer if file has changed, but only if your buffer is
;; not modified
(global-auto-revert-mode 1)
;; Cleanup whitespace "errors" when saving
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Preferences for editing commit messages
(ivi-mode-setup 'log-edit (lambda ()
			    (setq fill-column 72)
			    (auto-fill-mode 1)))

(provide 'builtin-setup)
