;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Install packages by default
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :config
  (smex-initialize))

(use-package ibuffer-vc
  :config
  ;; Extends and configures ibuffer(-vc)
  (require 'gk-ibuffer))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package avy
  :bind
  (("M-g M-g" . avy-goto-word-1)
   ("M-g g" . avy-goto-line)
   ("M-g SPC" . avy-goto-char-timer)
   ("M-g M-SPC" . avy-goto-char-timer)))

(use-package markdown-mode
  :hook
  (markdown-mode . visual-line-mode))

(provide 'melpa-setup)
