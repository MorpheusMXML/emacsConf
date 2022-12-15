(require 'smtpmail)

(use-package mu4e
  :config

  ;; Load org-mode integration
  (require 'org-mu4e)

;; we installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu"))

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
				    "maximilian.brosius@uni-hamburg.de"))

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
	  ("/uhhmail/INBOX" . ?u)
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
          :vars '((user-mail-address . "brosiusmac@gmail.com")
                  (user-full-name . "Maximilian Brosius")
                  (mu4e-drafts-folder . "/gmail/[GMAIL]/Entw&APw-rfe")
                  (mu4e-refile-folder . "/gmail/[GMAIL]/Alle Nachrichten")
                  (mu4e-sent-folder . "/gmail/[GMAIL]/Gesendet")
                  (mu4e-trash-folder . "/gmail/[GMAIL]/Papierkorb")))

        ,(make-mu4e-context
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
                  ;; e.g `ls ~/.maildir/example'
                  (mu4e-drafts-folder . "/maxbrosius/Drafts")
                  (mu4e-refile-folder . "/maxbrosius/Archive")
                  (mu4e-sent-folder . "/maxbrosius/Sent")
                  (mu4e-trash-folder . "/maxbrosius/Trash")))

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
                  ;; e.g `ls ~/.maildir/example'
                  (mu4e-drafts-folder . "/hellomaxb/Drafts")
                  (mu4e-refile-folder . "/hellomaxb/Archive")
                  (mu4e-sent-folder . "/hellomaxb/Sent")
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
                  ;; e.g `ls ~/.maildir/example'
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

;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epg-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;;Testing Pinentry mac setup 50004000000000000 
;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-sendmail-extra-arguments '("--read-envelope-from")
     message-send-mail-function 'message-send-mail-with-sendmail)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))
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
(mu4e t))

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
	       ((string-match "maximilian.brosius@uni-hamburg.de" from) "uhh"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'mabr/set-msmtp-account)

;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun mabr/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

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

(provide 'a-mu4e)
