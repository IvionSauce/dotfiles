;; Emacs insists on having package-initialize here, we load it in melpa-setup
;;(package-initialize)

;; Put all that custom junk away
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Set some general/misc variables
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

;; Optionally use commit-patch, https://porkrind.org/commit-patch/
;; Maybe not useful for those that exclusively use Magit
(require 'commit-patch-buffer nil t)

;; My library path
(add-to-list 'load-path (locate-user-emacs-file "elisp"))
;; Helper functions and macros
(require 'init-helpers)
;; Autosave and backup settings
(require 'autosave-backup)
;; Setting up built-in packages
(require 'builtin-setup)
;; Mapping or remapping keybindings
(require 'keybindings-builtin)
;; Setting up MELPA packages (and their keybindings)
(require 'melpa-setup)
;; Setting up email (mu4e)
(require 'mail-setup)
;; Custom functions/advices
(require 'custom-funcs)
;; Themeing and font settings
(require 'appearance)
