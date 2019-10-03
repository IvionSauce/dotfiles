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

;; Helper functions to easily set keybindings
;; Typing global-set-key and kbd all the time gets old (even with completion)
(defun ivi-keys (binds-alist &optional keymap)
  "Perform keybindings listed in BINDS-ALIST. BINDS-ALIST has the form of
((KEY . DEF)) where KEY is either a single character or a string describing the
keys – as suitable for `kbd'. When KEYMAP is omitted `global-map' is assumed."
  (let ((map (or keymap global-map)))
    (dolist (cell binds-alist)
      (let ((key (car cell)) (def (cdr cell)))
	;; Handle single character keybinds
	(if (characterp key)
	    (define-key map (vector key) def)
	  (define-key map (kbd key) def))))))

(defun ivi-keys-with-map (keymap binds-alist)
  (ivi-keys binds-alist keymap))

(defun ivi-keys-local (mode-hooks binds-alist)
  "Create function that performs keybindings (local to mode) listed in
BINDS-ALIST and add it to the MODE-HOOKS. MODE-HOOKS can either be a list of
hooks or a single hook. BINDS-ALIST has the form of ((KEY . DEF)) where KEY
is either a single character or a string describing the keys – as suitable
for `kbd'."
  (let ((hooks (if (listp mode-hooks) mode-hooks (list mode-hooks)))
	(bind-keys (lambda ()
		     (let ((map (current-local-map)))
		       (if map (ivi-keys binds-alist map)
			 (error "No local keymap found"))))))
    (dolist (mode-hook hooks)
      (add-hook mode-hook bind-keys))))

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
;; Themeing and font settings
(require 'appearance)
