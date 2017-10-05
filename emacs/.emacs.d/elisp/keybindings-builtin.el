;; Rebind buttons to similar, but better, alternatives
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Replace zap-to-char with zap-up-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Previously ido-find-file-read-only
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Start eshell or switch to it if it's active
;; Previously compose-mail
(global-set-key (kbd "C-x m") 'eshell)

(global-set-key (kbd "C-x C-y") 'hexl-find-file)

;; Bind other-window to something easy to reach
;; "C-x o" is the default bind
(global-set-key (kbd "s-o") 'other-window)

(provide 'keybindings-builtin)
