;; Misc.el is not loaded by default
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; I mostly use the GUI client, so I don't exclude bindings that won't work in
;; the terminal (although if it works in both it's a plus).
(ivi-keys
 '(
   ;; Rebind buttons to similar, but better, alternatives
   ("C-h a" . apropos)
   ("M-/" . hippie-expand)
   ("C-x C-b" . ibuffer)
   ("M-z" . zap-up-to-char)

   ;; Previously ido-find-file-read-only
   ("C-x C-r" . recentf-open-files)
   ;; Start eshell or switch to it if it's active
   ;; Previously compose-mail
   ("C-x m" . eshell)
   ;; For inspecting binary files in hex
   ("C-x y" . hexl-find-file)

   ;; Swap windows around
   ("s-p" . window-swap-states)
   ;; Quickly bury help/documentation windows
   ("s-\\" . (lambda () (interactive)
	       (if (> (length (window-list)) 1)
		   (with-selected-window (next-window)
		     (quit-window))
		 (message "No other window to bury"))))


   ;;; Bind some difficult to reach and/or complex keybinds to easier variants

   ;; C-x 1
   ("C-1" . delete-other-windows)
   ;; C-x 0
   ("M-1" . delete-window)
   ;; C-x 3
   ("C-2" . split-window-right)
   ;; C-x 2
   ("M-2" . split-window-below)

   ;; C-x C-f • You don't save key presses with this bind, but is more
   ;; comfortable on QWERTY
   ("M-g M-f" . find-file)
   ;; C-x 4 f
   ("M-g f" . find-file-other-window)
   ;; C-x C-s
   ;; Previously a prefix for facemenu-set-* and other cosmetics
   ("M-o" . save-buffer)
   ;; C-x C-s C-x C-c • Save-and-exit for quick jobs
   ("M-g RET" . (lambda () (interactive)
		  (save-buffer) (save-buffers-kill-terminal)))

   ;; C-x v =
   ("M-g r" . vc-diff)
   ;; C-x v D
   ("M-g M-r" . vc-root-diff)

   ;; C-x o
   ("s-o" . other-window)
   ;; C-x b
   ("s-[" . switch-to-buffer)
   ;; C-x 4 b
   ("s-]" . switch-to-buffer-other-window)
   ;; C-x C-b
   ;; Previously suspend-frame
   ("C-z" . ibuffer)

   ;; M-S-4 / M-$
   ("C-4" . ispell-word)
   ;; M-S-5 / M-%
   ("C-5" . query-replace)
   ;; C-M-S-5 / C-M-%
   ("M-5" . query-replace-regexp)
   ;; M-S-6 / M-^
   ("C-6" . delete-indentation)
   ;; M-S-[ / M-{
   ("C-," . backward-paragraph)
   ;; M-S-] / M-}
   ("C-." . forward-paragraph)
   ;; M-S-, / M-<
   ("C-<" . beginning-of-buffer)
   ;; M-S-. / M->
   ("C->" . end-of-buffer)
   ))

(ivi-keys-local
 'log-view
 '((?. . log-view-msg-next)
   (?, . log-view-msg-prev)))

(provide 'keybindings-builtin)
