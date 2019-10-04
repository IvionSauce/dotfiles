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

(provide 'init-helpers)
