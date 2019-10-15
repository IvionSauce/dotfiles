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

(use-package helm
  :demand t
  :config
  (require 'helm-config)
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-file-cache-fuzzy-match t
	helm-completion-in-region-fuzzy-match t)
  ;; Put completion window below and autoresize
  (setq helm-split-window-in-side-p t
	helm-autoresize-max-height 25)
  (helm-autoresize-mode 1)
  ;; Make Helm visually less obnoxious
  (setq helm-display-header-line nil)
  (set-face-attribute 'helm-source-header nil :height 0.8)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  :bind
  (("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)
   ("M-s o" . helm-occur)
   ("C-x C-x" . helm-all-mark-rings)
   ("M-u" . helm-mini)
   ("M-[" . (lambda () (interactive)
	      (other-window 1) (helm-mini)))
   ("M-g M-f" . helm-find-files)
   ("C-c h" . helm-command-prefix)))

(use-package magit
  :config
  (add-hook 'git-commit-mode-hook (lambda ()
				    (setq fill-column 72)))
  :bind
  (("M-g m" . magit-status)))

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
