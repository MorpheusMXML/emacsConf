(require 'smtpmail)

(use-package mu4e
  :config
  ;; Load org-mode integration
  ;; (require 'org-mu4e)
  ;; we installed this with homebrew
  (setq mu4e-mu-binary "/opt/homebrew/bin/mu")
  ;; this is the directory we created before:
  (setq mu4e-maildir "~/.maildir")
  ;; this command is called to sync imap servers:
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; how often to call it in seconds:
  (setq mu4e-update-interval 300)
  ;; save attachment to desktop by default
  ;; or another choice of yours:
  (setq mu4e-attachment-dir "~/Downloads")
  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; rename files when moving - needed for mbsync:
  (setq mu4e-change-filenames-when-moving t)

  ;; list of your email adresses:
  (setq mu4e-user-mail-address-list '("mbrosius@icloud.com"
                                      "brosiusmax@gmail.com"
                                      "hello@maxbrosius.de"
				      "mail@maxbrosius.de"
				      "maximilian.brosius@uni-hamburg.de"
				      "maximilian.brosius@studium.uni-hamburg.de"
				      "6brosius@informatik.uni-hamburg.de"))

  ;; store link to message if in header view, not to header query:
  (setq org-mu4e-link-query-in-headers-mode nil)
  ;; don't have to confirm when quitting:
  (setq mu4e-confirm-quit nil)
  ;; number of visible headers in horizontal split view:
  (setq mu4e-headers-visible-lines 20)
  ;; don't show threading by default:
  (setq mu4e-headers-show-threads nil)
  ;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
  (setq mu4e-hide-index-messages t)
  ;; customize the reply-quote-string:
  (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
  ;; M-x find-function RET message-citation-line-format for docs:
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  ;; by default do not show related emails:
  (setq mu4e-headers-include-related nil)
  ;; by default do not show threads:
  (setq mu4e-headers-show-threads nil)

  ;; check your ~/.maildir to see how the subdirectories are called
  ;; for the generic imap account:
  ;; e.g `ls ~/.maildir/example'
  (setq   mu4e-maildir-shortcuts
          '(("/icloud/INBOX" . ?i)
            ("/icloud/Sent Messages" . ?I)
            ("/gmail/INBOX" . ?g)
            ("/gmail/[Gmail]/Gesendet" . ?G)
	    ("/maxbrosius/INBOX" . ?p)
	    ("/maxbrosius/Sent Items" . ?P)
	    ("/hellomaxb/INBOX" . ?e)
            ("/hellomaxb/Sent Items" . ?E)
	    ("/uhhmail/INBOX" . ?c)
	    ("/uhhmail/Gesendete Elemente" . ?y)))

  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)


  (setq mu4e-contexts
	`(,(make-mu4e-context
            :name "maxbrosius"
            :enter-func
            (lambda () (mu4e-message "Entering mail@maxbrosius.de ..."))
            :leave-func
            (lambda () (mu4e-message "Leaveing mail@maxbrosius.de ..."))
            :match-func
            (lambda (msg)
              (when msg
		(mu4e-message-contact-field-matches msg
                                                    :to "mail@maxbrosius.de")))
            :vars '((user-mail-address . "mail@maxbrosius.de")
                    (user-full-name . "Maximilian Brosius")
                    ;; check your ~/.maildir to see how the subdirectories are called
                    (mu4e-drafts-folder . "/maxbrosius/Drafts")
                    (mu4e-refile-folder . "/maxbrosius/Archive")
                    (mu4e-sent-folder . "/maxbrosius/Sent Items")
                    (mu4e-trash-folder . "/maxbrosius/Trash")))
	  ,(make-mu4e-context
            :name "icloud"
            :enter-func
            (lambda () (mu4e-message "Entering mbrosius@icloud.com ..."))
            :leave-func
            (lambda () (mu4e-message "Leaveing mbrosius@icloud.com ..."))
            :match-func
            (lambda (msg)
              (when msg
		(mu4e-message-contact-field-matches msg
                                                    :to "mbrosius@icloud.com")))
            :vars '((user-mail-address . "mbrosius@icloud.com" )
                    (user-full-name . "Maximilian Brosius")
                    (mu4e-drafts-folder . "/icloud/Drafts")
                    (mu4e-refile-folder . "/icloud/Archive")
                    (mu4e-sent-folder . "/icloud/Sent Messages")
                    (mu4e-trash-folder . "/icloud/Deleted Messages")))

          ,(make-mu4e-context
            :name "gmail"
            :enter-func
            (lambda () (mu4e-message "Entering brosiusmax@gmail.com ..."))
            :leave-func
            (lambda () (mu4e-message "Leaveing brosiusmax@gmail.com ..."))
            :match-func
            (lambda (msg)
              (when msg
		(mu4e-message-contact-field-matches msg
                                                    :to "brosiusmax@gmail.com")))
            :vars '((user-mail-address . "brosiusmax@gmail.com")
                    (user-full-name . "Maximilian Brosius")
                    (mu4e-drafts-folder . "/gmail/[GMAIL]/Entw&APw-rfe")
                    (mu4e-refile-folder . "/gmail/[GMAIL]/Alle Nachrichten")
                    (mu4e-sent-folder . "/gmail/[GMAIL]/Gesendet")
                    (mu4e-trash-folder . "/gmail/[GMAIL]/Papierkorb")))

          ,(make-mu4e-context
            :name "hellomaxb"
            :enter-func
            (lambda () (mu4e-message "Entering hello@maxbrosius.de ..."))
            :leave-func
            (lambda () (mu4e-message "Leaveing hello@maxbrosius.de ..."))
            :match-func
            (lambda (msg)
              (when msg
		(mu4e-message-contact-field-matches msg
                                                    :to "hello@maxbrosius.de")))
            :vars '((user-mail-address . "hello@maxbrosius.de")
                    (user-full-name . "Maximilian Brosius")
                    ;; check your ~/.maildir to see how the subdirectories are called
                    (mu4e-drafts-folder . "/hellomaxb/Drafts")
                    (mu4e-refile-folder . "/hellomaxb/Archive")
                    (mu4e-sent-folder . "/hellomaxb/Sent Items")
                    (mu4e-trash-folder . "/hellomaxb/Trash")))


          ,(make-mu4e-context
            :name "uhhmail"
            :enter-func
            (lambda () (mu4e-message "Entering maximilian.brosius@uni-hamburg.de ..."))
            :leave-func
            (lambda () (mu4e-message "Leaveing maximilian.brosius@uni-hamburg.de ..."))
            :match-func
            (lambda (msg)
              (when msg
		(mu4e-message-contact-field-matches msg
                                                    :to "maximilian.brosius@uni-hamburg.de")))
            :vars '((user-mail-address . "maximilian.brosius@uni-hamburg.de")
                    (user-full-name . "Maximilian Brosius")
                    ;; check your ~/.maildir to see how the subdirectories are called
                    (mu4e-drafts-folder . "/uhhmail/Entw&APw-rfe")
                    (mu4e-refile-folder . "/uhhmail/Archiv")
                    (mu4e-sent-folder . "/uhhmail/Gesendete Elemente")
                    (mu4e-trash-folder . "/uhhmail/Gel&APY-schte Elemente")))

	  ,(make-mu4e-context 
	    :name "fbi"
	    :enter-func
            (lambda () (mu4e-message "Entering fbi Mail..."))
            :leave-func
            (lambda () (mu4e-message "Leaveing fbi Mail ..."))
	    :match-func
	    (lambda (msg) 
	      (when msg 
		(mu4e-message-contact-field-matches msg
						    `(:to :cc :from) "6brosius@informatik.uni-hamburg.de"))) 
	    :vars `((user-mail-address . "6brosius@informatik.uni-hamburg.de")
		    (user-full-name . "Maximilian Brosius")
                    ;; check your ~/.maildir to see how the subdirectories are called
                    (mu4e-drafts-folder . "/uhhmail/Entw&APw-rfe")
                    (mu4e-refile-folder . "/uhhmail/Archiv")
                    (mu4e-sent-folder . "/uhhmail/Gesendete Elemente")
                    (mu4e-trash-folder . "/uhhmail/Gel&APY-schte Elemente")))
	  ,(make-mu4e-context 
	    :name "studium"
	    :enter-func
            (lambda () (mu4e-message "Entering studium Mail..."))
            :leave-func
            (lambda () (mu4e-message "Leaveing studium Mail ..."))
	    :match-func
	    (lambda (msg) 
	      (when msg 
		(mu4e-message-contact-field-matches msg
						    `(:to :cc :from) "maximilian.brosius@studium.uni-hamburg.de"))) 
	    :vars `((user-mail-address . "maximilian.brosius@studium.uni-hamburg.de")
		    (user-full-name . "Maximilian Brosius")
                    ;; check your ~/.maildir to see how the subdirectories are called
                    (mu4e-drafts-folder . "/uhhmail/Entw&APw-rfe")
                    (mu4e-refile-folder . "/uhhmail/Archiv")
                    (mu4e-sent-folder . "/uhhmail/Gesendete Elemente")
                    (mu4e-trash-folder . "/uhhmail/Gel&APY-schte Elemente"))) 

	  ))


  ;; (setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
  (setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;

  ;; convenience function for starting the whole mu4e in its own frame
  ;; posted by the author of mu4e on the mailing list
  (defun mu4e-in-new-frame ()
    "Start mu4e in new frame."
    (interactive)
    (select-frame (make-frame))
    (mu4e))

  ;; spell checking
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

  ;; Disable Line No for Headers View..
  (add-hook 'mu4e-headers-mode-hook '(lambda () (display-line-numbers-mode -1)))
  (add-hook 'mu4e-main-mode-hook '(lambda () (display-line-numbers-mode -1)))
  ;; gpg encryptiom & decryption:
  ;; this can be left alone
  (require 'epa-file)
  (epa-file-enable)
  (setq epg-pinentry-mode 'loopback)
  (auth-source-forget-all-cached)

  ;; don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)

  ;; send function:
  ;; send program:
  ;; this is exeranal. remember we installed it before.
  (setq sendmail-program (executable-find "msmtp")
	send-mail-function 'smtpmail-send-it
	message-sendmail-f-is-evil t
	message-sendmail-extra-arguments '("--read-envelope-from")
	message-send-mail-function 'message-send-mail-with-sendmail)


  ;; the following is to show shortcuts in the main view.
  (add-to-list 'mu4e-bookmarks
	       '(:name "Inbox - iCloud"
		       :query "maildir:/icloud/INBOX"
		       :key ?a))
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox - Gmail"
		       :query "maildir:/gmail/INBOX"
		       :key ?g))
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox - Private"
		       :query "maildir:/maxbrosius/INBOX"
		       :key ?p))
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox - hello maxb"
		       :query "maildir:/hellomaxb/INBOX"
		       :key ?e))
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox - UHHmail"
		       :query "maildir:/uhhmail/INBOX"
		       :key ?u))
  (add-to-list 'mu4e-bookmarks
	       '(:name "All Inboxes"
		       :query "maildir:/icloud/INBOX OR maildir:/gmail/INBOX OR maildir:/maxbrosius/INBOX OR maildir:/hellomaxb/INBOX OR maildir:/uhhmail/INBOX"
		       :key ?I))

  (setq mabr/mu4e-inbox-query
        "(maildir:/icloud/INBOX OR maildir:/gmail/INBOX OR maildir:/maxbrosius/INBOX OR maildir:/hellomaxb/INBOX OR maildir:/uhhmail/INBOX) AND flag:unread")

  (defun mabr/go-to-inbox ()
    (interactive)
    (mu4e-headers-search mabr/mu4e-inbox-query))

  ;; select the right sender email from the context.
  (setq message-sendmail-envelope-from 'header)
  (mu4e t)
  )

;; chose from account before sending
;; this is a custom function that works for me.
;; well I stole it somewhere long ago.
;; I suggest using it to make matters easy
;; of course adjust the email adresses and account descriptions
(defun mabr/set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "mbrosius@icloud.com" from) "icloud")
               ((string-match "brosiusmax@gmail.com" from) "gmail")
               ((string-match "mail@maxbrosius.de" from) "maxbrosius")
	       ((string-match "hello@maxbrosius.de" from) "hellomaxb")
	       ((string-match "maximilian.brosius@uni-hamburg.de" from) "uhh")
	       ((string-match "maximilian.brosius@studium.uni-hamburg.de" from) "studium")
	       ((string-match "6brosius@informatik.uni-hamburg.de" from) "fbi"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'mabr/set-msmtp-account)

;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun mabr/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc: \n"))
            (save-excursion (message-add-header "Bcc: \n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)

(use-package mu4e-alert
  :after mu4e-debug
  :config
  ;; Shwo unread emails from all INboxes
  (setq mu4e-alert-interesting-mail-query mabr/mu4e-inbox-query)

  ;; Shiw notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)
  (mu4e-alert-enable-notifications))


(defun mabr/mu4e-choose-signature ()
  "Insert one of a number of predefined Signatures into Message Buffer"
  (interactive)
  (let ((message-signature
	 (mu4e-read-option "Signature:"
			   '(("formal Private" .
			      (concat
			       "Maximilian Brosius\n"
			       "In urgent cases don't use E-Mail - call me 2x!!\n"
			       "\n"
			       "mail@maxbrosius.de\n"
			       "+49 173 49 838 56\n"))
			     ("informal" .
			      "Max\n")))))
    (message-insert-signature)))

(autoload 'mail-hist-forward-header "mail-hist")
(autoload 'mail-text-start          "sendmail")

(defun my-message-signature-start ()
  "Return value of point at start of message signature."
  (save-mark-and-excursion
    (message-goto-signature)
    (point)))

(defun my-message-field-forward ()
  "Move point to next \"field\" in a `message-mode' buffer.
With each invocation, point is moved to the next field of
interest amongst header values, message body and message
signature, in that order."
  (interactive)
  (cond ((message-point-in-header-p)
         (unless (mail-hist-forward-header 1)
           (call-interactively #'message-goto-body)))
        ((>= (point) (my-message-signature-start))
         (message "No further field"))
        ((message-in-body-p)
         (message-goto-signature))
        (t ; Probably on `mail-header-separator' line
         (call-interactively #'message-goto-body))))

(defun my-message-field-backward ()
  "Like `my-message-field-forward', but in opposite direction."
  (interactive)
  (cond ((or (message-point-in-header-p)
             (<= (point) (mail-text-start)))
         (unless (mail-hist-forward-header
                  (if (message-point-in-header-p) -1 0))
           (message "No further field")))
        ((<= (point) (my-message-signature-start))
         (call-interactively #'message-goto-body))
        (t ; Beyond start of signature
         (message-goto-signature))))

;; (use-package mu4e-icalendar
;;   :after mu4e
;;   :init
;;   (mu4e-icalendar-setup))

(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(defun mabr/mu4e-view-save-all-attachments (&optional arg)
  "Save all attachments of a given message.

If ARG is nil, all attachments will be saved in
`mu4e-attachment-dir'. When non-nil, user will be prompted to
choose a specific directory where to save all the files."
  (interactive "P")
  (when (and (eq major-mode 'mu4e-view-mode)
             (derived-mode-p 'gnus-article-mode))
    (let ((parts (mu4e~view-gather-mime-parts))
          (handles '())
          (files '())
          (directory (if arg
                         (read-directory-name "Save to directory: ")
                       mu4e-attachment-dir)))
      (dolist (part parts)
        (let ((fname (or (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                         (cl-loop for item in part
                                  for name = (and (listp item) (assoc-default 'name item))
                                  thereis (and (stringp name) name)))))
          (when fname
            (push `(,fname . ,(cdr part)) handles)
            (push fname files))))
      (if files
          (cl-loop for (f . h) in handles
                   when (member f files)
                   do (mm-save-part-to-file
                       h (let ((file (expand-file-name f directory)))
                           (if (file-exists-p file)
                               (let (newname (count 1))
                                 (while (and
                                         (setq newname
                                               (format "%s-%s%s"
                                                       (file-name-sans-extension file)
                                                       count
                                                       (file-name-extension file t)))
                                         (file-exists-p newname))
                                   (cl-incf count))
                                 newname)
                             file))))
        (mu4e-message "No attached files found")))))

(general-def
  :states 'normal
  :keymaps 'mu4e-compose-mode-map
  "n" '(my-message-field-forward :which-key "Next Message Field")
  "p" '(my-message-field-backward :which-key "Prev Message Fielt")
  "a" '(mml-insert-file :which-key "Attachment")
  "q" '(message-kill-buffer :which-key "Kill Buffer")
  "d" '(message-dont-send :which-key "Save Draft")
  "s" '(mabr/mu4e-choose-signature :which-key "Signature")
  "b" '(message-goto-body :which-key "Jump to Body")
  "h" '(message-goto-to :which-key "Jump to Header")
  "e" '(mml-secure-message-encrypt-pgp :which-key "Encrypt")
  "S" '(mml-secure-message-sign-pgp :which-key "Sign"))

(general-def
  :states '(normal visual)
  :keymaps 'mu4e-view-mode-map
  "<" '(:ignore t :which-key "Save Attachment")
  "< a" '(mabr/mu4e-view-save-all-attachments :which-key "Save All Att.")
  "< s" '(mu4e-view-save-attachments :which-key "Save single Att."))

(provide 'a-mu4e)
