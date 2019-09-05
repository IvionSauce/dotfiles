;;; gk-ibuffer.el --- Ibuffer setup.                 -*- lexical-binding: t; -*-

;;; Commentary:
;; Lifted wholesale from https://www.reddit.com/r/emacs/comments/6qfvia/can_we_talk_about_managingswitching_between_many/dkz5v3n/

;;; Code:
(require 'ibuffer)
(require 'ibuffer-vc)
(require 'subr-x)

;;; IBuffer:
;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (let ((bs (buffer-size)))
    (cond ((> bs 1e6) (format "%7.1fmB" (/ bs 1e6)))
	  ((> bs 1e3) (format "%7.1fkB" (/ bs 1e3)))
	  (t          (format "%7d  " bs)))))

(setf ibuffer-formats
      '((mark modified read-only vc-status-mini " "
	      (name 18 18 :left :elide)
	      " "
	      (size-h 9 -1 :right)
	      " "
	      (mode 16 16 :left :elide)
	      " "
	      (vc-status 16 16 :left)
	      " "
	      filename-and-process))
      ibuffer-show-empty-filter-groups nil)

(define-ibuffer-filter name-not
    "Negated buffer name match."
  (:description "buffer name not"
   :reader (read-from-minibuffer "Exclude buffers (regexp): "))
  (not (string-match qualifier (buffer-name buf))))

(defvar gk-ibuffer-filters
  '(("Emacs"
     (name . "^\\*\\(scratch\\|Messages\\)\\*$"))
    ("VC"
     (name . "^\\*\\(vc\\|log\\)-?"))
    ("Documentation"
     (name . "^\\*\\(Help\\|info\\|Man [0-9]?\\)"))
    ("Special buffers"
     (name-not . "compilation")
     (name . "^\\*.*\\*$"))
    ("EWW Reading"
     (mode . eww-mode)))
  "Fixed filter definitions for ibuffer.")

(cl-defun gk-ibuffer-generate-filter-groups-by-dir ()
  "Create a set of ibuffer filter groups based on the dirs of buffers."
  (let* ((func
	  (lambda (buf)
	    (when-let ((bufnam (buffer-file-name buf)))
	      (file-name-directory (expand-file-name (file-truename bufnam))))))
	 (dirs
	  (ibuffer-remove-duplicates (delq nil (mapcar func (buffer-list))))))
    (mapcar (lambda (dir)
	      (cons (concat "Directory: " (abbreviate-file-name dir))
		    `((dir . ,dir))))
	    dirs)))

(define-ibuffer-filter dir
    "Toggle current view to buffers with dir QUALIFIER."
  (:description "directory" :reader (read-from-minibuffer "Filter by dir: "))
  (ibuffer-awhen (buffer-file-name buf)
    (string= qualifier (file-name-directory it))))

(define-advice ibuffer-update (:before (&rest args) autogroups)
  "Group related buffers together using ‘ibuffer-vc’ and ‘dir’,
and special ones sepatarely."
  (ignore args)
  (setf ibuffer-filter-groups
	(append
	 gk-ibuffer-filters
	 (ibuffer-vc-generate-filter-groups-by-vc-root)
	 (gk-ibuffer-generate-filter-groups-by-dir))))

;; Hide these buffers by default.
(defvar gk-ibuffer-collapsed-groups
  (list "Special buffers" "Emacs" "Documentation" "VC"))

;; Usage of functions and data structures glanced from 'ibuf-ext.el'.
(defun gk-ibuffer-hide-hook ()
  "Hide groups in ‘gk-ibuffer-collapsed-groups’."
  ;; Groups that are visible: not hidden and not empty.
  (let ((vis-groups
	(ibuffer-generate-filter-groups (ibuffer-current-state-list) t t)))
    (dolist (group gk-ibuffer-collapsed-groups)
      ;; Hide filter group if it is visible.
      (when (assoc group vis-groups)
	;; If we pushed without checking visibility we would make empty groups
	;; appear in *Ibuffer*, which disappear when you try to expand them.
	(push group ibuffer-hidden-filter-groups))))
  (ibuffer-redisplay t))

(setq ibuffer-default-sorting-mode 'filename/process)
(add-hook 'ibuffer-hook 'gk-ibuffer-hide-hook)

(provide 'gk-ibuffer)
;;; gk-ibuffer.el ends here
