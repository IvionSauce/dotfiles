(let ((autosave-backup-dir (locate-user-emacs-file "backups/")))
  (make-directory autosave-backup-dir :parents)
  (setq backup-directory-alist
	`(("." . ,autosave-backup-dir))
	auto-save-file-name-transforms
	`((".*" ,autosave-backup-dir t))))

(setq backup-by-copying-when-linked t)

(provide 'autosave-backup)
