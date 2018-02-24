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

;;; Bind some difficult to reach keybinds to easier variants
;;; Most of these only work in the GUI version

;; C-x o
(global-set-key (kbd "s-o") 'other-window)
;; C-x 0
(global-set-key (kbd "C-0") 'delete-window)
;; C-x 1
(global-set-key (kbd "C-1") 'delete-other-windows)
;; C-x 2
(global-set-key (kbd "C-2") 'split-window-below)
;; C-x 3
(global-set-key (kbd "C-3") 'split-window-right)

;; M-S-4 / M-$
(global-set-key (kbd "C-4") 'ispell-word)
;; M-S-5 / M-%
(global-set-key (kbd "C-5") 'query-replace)
;; C-M-S-5 / C-M-%
(global-set-key (kbd "M-5") 'query-replace-regexp)

;; M-S-[ / M-{
(global-set-key (kbd "C-,") 'backward-paragraph)
;; M-S-] / M-}
(global-set-key (kbd "C-.") 'forward-paragraph)
;; M-S-, / M-<
(global-set-key (kbd "C-<") 'beginning-of-buffer)
;; M-S-. / M->
(global-set-key (kbd "C->") 'end-of-buffer)


(provide 'keybindings-builtin)
