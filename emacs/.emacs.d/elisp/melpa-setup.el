;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; Packages installed using MELPA
;;; Usually you don't have to load/require the libraries yourself

;; IDO ubiquitous
(ido-ubiquitous-mode 1)
(setq org-completion-use-ido t)		; Org mode got its own IDO handling

;; SMEX
(smex-initialize)

;; Work around for Haskell mode
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

(provide 'melpa-setup)
