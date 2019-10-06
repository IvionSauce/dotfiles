;; -*- lexical-binding: t; -*-

;; Helper macro for getting secrets. Put it here instead of in ivi-secrets.el
;; because there's nothing secret about the macro.
(when (require 'ivi-secrets nil t)
  (defmacro ivi-reveal (elem)
    "Reveal secret associated with ELEM in `ivi-secrets-alist'."
    `(cdr (assq ',elem ivi-secrets-alist))))

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

(defun ivi-keys-local (mode binds-alist)
  "Perform keybindings, local to MODE, listed in BINDS-ALIST. The keymap the
bindings are defined in is derived from the symbol name of MODE. The binding of
the keys is deferred until MODE is loaded. See `ivi-keys' for a description of
BINDS-ALIST."
  (with-eval-after-load mode
    (let ((local-map (intern-soft (concat (symbol-name mode) "-mode-map"))))
      (if local-map
	  (ivi-keys binds-alist (symbol-value local-map))
	(error "No local keymap found for %s (tried: %s)" mode local-map)))))

(provide 'init-helpers)
