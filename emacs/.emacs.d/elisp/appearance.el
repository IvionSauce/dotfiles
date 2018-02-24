;; Turn off certain GUI elements
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(add-to-list 'default-frame-alist '(font . "Go Mono-10"))
;; Only load theme when running a graphical client
(defun ivi-themeing (&optional frame)
  (if (display-graphic-p frame)
      (load-theme 'leuven-edit t)))
;; First hook makes sure standalone Emacs gets themed
;; Second hook makes sure Emacsclient gets themed
;; https://stackoverflow.com/questions/19054228/
(add-hook 'window-setup-hook 'ivi-themeing)
(add-hook 'after-make-frame-functions 'ivi-themeing)

(provide 'appearance)
