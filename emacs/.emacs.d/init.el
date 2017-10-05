;; Emacs insists on having package-initialize here, we load it in melpa-setup
;;(package-initialize)

;; Put all that custom junk away
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Set some general variables
(setq
 ;; Paste at point, not at click
 mouse-yank-at-point t
 ;; Save clipboard to kill ring before new kill
 save-interprogram-paste-before-kill t
 ;; Keep point in position when scrolling
 scroll-preserve-screen-position 1
 ;; Do not accelerate mouse scroll speed (ie stays constant)
 mouse-wheel-progressive-speed nil
 ;; C-n/C-p moves over logical lines (ie not visual)
 line-move-visual nil
 ;; No GNU Emacs welcome buffer
 inhibit-startup-message t)

;; My library path
(add-to-list 'load-path (locate-user-emacs-file "elisp"))
;; Autosave and backup settings
(require 'autosave-backup)
;; Setting up built-in packages
(require 'builtin-setup)
;; Setting up MELPA packages
(require 'melpa-setup)
;; Custom functions/advices
(require 'custom-funcs)
;; Mapping or remapping keybindings
(require 'keybindings-builtin)
(require 'keybindings-melpa)
;; Themeing and font settings
(require 'appearance)
