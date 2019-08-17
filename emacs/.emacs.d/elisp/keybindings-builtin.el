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
;; For inspecting binary files in hex
(global-set-key (kbd "C-x y") 'hexl-find-file)

;;; What follows are GUI only binds
;; Swap windows around
(global-set-key (kbd "s-p") 'window-swap-states)
;; Quickly bury help/documentation windows
(global-set-key (kbd "s-\\") (lambda () (interactive)
			       (if (> (length (window-list)) 1)
				   (with-selected-window (next-window)
				     (quit-window))
				 (message "No other window to bury"))))


;;; Bind some difficult to reach and/or complex keybinds to easier variants
;;; Many of these only work in the GUI version

;; C-x 1
(global-set-key (kbd "C-1") 'delete-other-windows)
;; C-x 0
(global-set-key (kbd "M-1") 'delete-window)
;; C-x 3
(global-set-key (kbd "C-2") 'split-window-right)
;; C-x 2
(global-set-key (kbd "M-2") 'split-window-below)

;; C-x C-f • You don't save key presses with this bind, but is more comfortable
;; on QWERTY
(global-set-key (kbd "M-g M-f") 'find-file)
;; C-x 4 f
(global-set-key (kbd "M-g f") 'find-file-other-window)
;; C-x C-s
;; Previously a prefix for facemenu-set-* and other cosmetics
(global-set-key (kbd "M-o") 'save-buffer)
;; C-x C-s C-x C-c • Save-and-exit for quick jobs
(global-set-key (kbd "M-g RET") (lambda () (interactive)
				  (save-buffer) (save-buffers-kill-terminal)))

;; C-x o
(global-set-key (kbd "s-o") 'other-window)
;; C-x b
(global-set-key (kbd "s-[") 'switch-to-buffer)
;; C-x 4 b
(global-set-key (kbd "s-]") 'switch-to-buffer-other-window)
;; C-x C-b
;; Previously suspend-frame
(global-set-key (kbd "C-z") 'ibuffer)

;; M-S-4 / M-$
(global-set-key (kbd "C-4") 'ispell-word)
;; M-S-5 / M-%
(global-set-key (kbd "C-5") 'query-replace)
;; C-M-S-5 / C-M-%
(global-set-key (kbd "M-5") 'query-replace-regexp)
;; M-S-6 / M-^
(global-set-key (kbd "C-6") 'delete-indentation)
;; M-S-[ / M-{
(global-set-key (kbd "C-,") 'backward-paragraph)
;; M-S-] / M-}
(global-set-key (kbd "C-.") 'forward-paragraph)
;; M-S-, / M-<
(global-set-key (kbd "C-<") 'beginning-of-buffer)
;; M-S-. / M->
(global-set-key (kbd "C->") 'end-of-buffer)


(provide 'keybindings-builtin)
