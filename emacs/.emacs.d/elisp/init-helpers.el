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
  "Perform keybindings listed in BINDS-ALIST. BINDS-ALIST has the
form of ((KEY . DEF)) where KEY is either a single character or a
string describing the keys - as suitable for `kbd'. When KEYMAP
is omitted `global-map' is assumed."
  (let ((map (or keymap global-map)))
    (dolist (cell binds-alist)
      (let ((key (car cell)) (def (cdr cell)))
	;; Handle single character keybinds
	(if (characterp key)
	    (define-key map (vector key) def)
	  (define-key map (kbd key) def))))))

(defun ivi-keys-with-map (keymap binds-alist)
  "Same as `ivi-keys', but with the KEYMAP argument first."
  (ivi-keys binds-alist keymap))

(defun ivi-keys-local (mode binds-alist)
  "Perform keybindings, local to MODE, listed in BINDS-ALIST. The
keymap the bindings are defined in is derived from the symbol
name of MODE. The binding of the keys is deferred until MODE is
loaded. See `ivi-keys' for a description of BINDS-ALIST."
  (with-eval-after-load mode
    (let* ((local-map-string (ivi-derive-from-symbol mode 'keymap))
	   (local-map (intern-soft local-map-string)))
      (if local-map
	  (ivi-keys binds-alist (symbol-value local-map))
	(error "No local keymap found for %s (tried: %s)"
	       mode local-map-string)))))

(defun ivi-mode-setup (feature-symbol setup-func)
  "When FEATURE-SYMBOL gets loaded add SETUP-FUNC to its mode-hook.
We derive a mode-hook symbol name from FEATURE-SYMBOL and use
that symbol name to add SETUP-FUNC to the mode hooks."
  (with-eval-after-load feature-symbol
    (let* ((hook-string (ivi-derive-from-symbol feature-symbol 'hook))
	   (hook (intern-soft hook-string)))
      (if hook
	  (add-hook hook setup-func)
	(error "No hook found for %s (tried: %s)"
	       feature-symbol hook-string)))))

(defun ivi-derive-from-symbol (symbol derive-type)
  "Derives from SYMBOL a new name, according to DERIVE-TYPE.
The new symbol name is returned as a string, so the caller still
has to `intern' or `intern-soft' the name as desired.\n
DERIVE-TYPE must be one of the following symbols:
• keymap / mode-map\tReturn symbol name to define keys in.
• hook / mode-hook\tReturn symbol name to add hooks to."
  (let* ((symbol-string (symbol-name symbol))
	 ;; Add either suffix unmodified, or prepended with -mode
	 (concat-cond
	  (lambda (suffix)
	    (concat symbol-string
		    (if (string-suffix-p "-mode" symbol-string)
			suffix (concat "-mode" suffix))))))
    (cond
     ((or (eq derive-type 'keymap)
	  (eq derive-type 'mode-map)) (funcall concat-cond "-map"))
     ((or (eq derive-type 'hook)
	  (eq derive-type 'mode-hook)) (funcall concat-cond "-hook"))
     (t (error "Invalid derive-type argument")))))

(provide 'init-helpers)
