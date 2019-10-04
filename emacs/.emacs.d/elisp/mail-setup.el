;; Only set up mail if we got mu4e
(when (require 'mu4e nil t)

  (setq mail-user-agent 'mu4e-user-agent
	;; After composing a message do not keep its buffer around
	message-kill-buffer-on-exit t
	mu4e-maildir "~/Mail/xs4all"
	mu4e-sent-folder "/Sent"
	mu4e-drafts-folder "/Drafts"
	mu4e-trash-folder "/Trash"
	mu4e-get-mail-command "mbsync --all"
	;; Make mu4e play nicely with mbsync
	mu4e-change-filenames-when-moving t
	;; Don't ask if we really want to quit
	mu4e-confirm-quit nil)

  (defvar ivi-secrets-alist nil
    "Association list of secrets.")

  (let ((mail-addresses
	 '("ivion@xs4all.nl")))
    (setq ivi-secrets-alist
	  `((full-name . nil)
	    (email-address . ,(first mail-addresses))
	    (email-address-list . ,mail-addresses))))

  (defmacro ivi-reveal (elem)
    "Reveal secret associated with ELEM in `ivi-secrets-alist'."
    `(cdr (assq ',elem ivi-secrets-alist)))

  (setq user-mail-address (ivi-reveal email-address)
	user-full-name (ivi-reveal full-name)
	mu4e-user-mail-address-list (ivi-reveal email-address-list))

  (setq mu4e-maildir-shortcuts
	'(("/INBOX" . ?i)
	  ("/Trash" . ?t)
	  ("/Sent" . ?s)
	  ("/Drafts" . ?d)
	  ("/Spam" . ?k)))

  (setq mu4e-headers-fields
	'((:human-date . 12)
	  (:flags . 6)
	  (:from-or-to . 22)
	  (:subject . 256)))

  ;; There is some discussion on what is meant by trashing a message,
  ;; see: https://github.com/djcb/mu/issues/1136
  ;; This is compounded by different server software having different attitudes
  ;; and configuration options, making it difficult to predict whether or not the
  ;; message will get deleted/expunged. Hence we'll go the safer route, never
  ;; marking messages +T.
  (setf (alist-get 'trash mu4e-marks)
	'(:char ("d" . "▼")
	  :prompt "dtrash"
	  :dyn-target (lambda (target msg)
			(mu4e-get-trash-folder msg))
	  :action (lambda (docid msg target)
		    (mu4e~proc-move
		     docid (mu4e~mark-check-target target) "-N"))))

  ;; Configure technical aspects of sending mail
  (setq send-mail-function 'smtpmail-send-it
	smtpmail-smtp-server "smtp.xs4all.nl"
	;; There seems to be some duplication of functionality going on. So
	;; alias these variables just to be safe.
	message-send-mail-function send-mail-function
	smtpmail-default-smtp-server smtpmail-smtp-server
	;; Use TLS to connect to the SMTP server
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465))

(provide 'mail-setup)
