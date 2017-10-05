;; Enable IDO mode
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

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

;; Enable word wrap (and visual movement) for markdown-mode
(add-hook 'markdown-mode-hook 'visual-line-mode)

(provide 'builtin-setup)
