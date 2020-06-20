;; MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Workaround for race condition with GnuTLS
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Install packages by default
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package helm
  :config
  (require 'helm-config)
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-M-x-fuzzy-match t
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
   ("C-c h" . helm-command-prefix)
   :map helm-map
   ("C-," . helm-previous-line)
   ("C-." . helm-next-line)
   :map helm-buffer-map
   ("C-c C-x" . helm-buffer-run-kill-buffers)))

(use-package helm-swoop
  :after helm
  :config
  (setq helm-swoop-speed-or-color t)
  :bind
  (("M-s s" . helm-swoop)))

(use-package magit
  :config
  (add-hook 'git-commit-mode-hook (lambda ()
				    (setq fill-column 72)))
  ;; Don't hide recent commits by default
  (add-to-list 'magit-section-initial-visibility-alist
	       '(unpushed . show))
  :bind
  (("M-g m" . magit-status)
   :map magit-mode-map
   ("," . magit-section-backward)
   ("." . magit-section-forward)))

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-k" . crux-smart-kill-line)
   ("C-<return>" . crux-smart-open-line)
   ("C-o" . crux-smart-open-line-above)
   ("C-M-z" . crux-indent-defun)
   ("M-0" . crux-top-join-line)))

(use-package ibuffer-vc
  :config
  ;; Extends and configures ibuffer(-vc)
  (require 'gk-ibuffer))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package avy
  :config
  ;; Reduce visual clutter by setting avy-background and simplifying the colour
  ;; coding for matches
  (setq avy-background t
	avy-lead-faces '(avy-lead-face avy-lead-face avy-lead-face))
  (set-face-attribute 'avy-background-face nil
		      :background "unspecified" :foreground "gray40")
  (set-face-attribute 'avy-lead-face nil
		      :background "unspecified" :foreground "red"
		      :weight 'normal)
  (setq avy-all-windows nil)
  :bind
  (("M-g M-g" . avy-goto-char-2)
   ("M-g g" . avy-goto-line)
   ("M-g SPC" . avy-goto-char-timer)
   ("M-g M-SPC" . avy-goto-char-timer)))

;; Set up avy-jump in Helm
(use-package ace-jump-helm-line
  :after helm
  :config
  (setq ace-jump-helm-line-default-action 'select)
  :bind
  (:map helm-map
	("C-'" . ace-jump-helm-line)))

(use-package markdown-mode
  :init
  ;; To ensure we have access to whitespace-style
  (require 'whitespace)
  :hook
  (markdown-mode . visual-line-mode)
  ;; Use only spaces in markdown and don't trim trailing whitespace,
  ;; it's sometimes significant.
  (markdown-mode . (lambda ()
		     (set (make-local-variable 'whitespace-style)
			  (remq 'trailing whitespace-style))
		     (setq indent-tabs-mode nil))))

(provide 'melpa-setup)
