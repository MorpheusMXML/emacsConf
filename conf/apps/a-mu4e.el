(use-package mu4e)

(require 'smtpmail)

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
(setq mu4e-attachment-dir "~/mu4e")

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
          :vars '((user-mail-address . "mbrosius@icloud.com")
                  (user-full-name . "Maximilian Brosius")
                  (mu4e-drafts-folder . "/gmail/Drafts")
                  (mu4e-refile-folder . "/gmail/Archive")
                  (mu4e-sent-folder . "/gmail/Sent")
                  (mu4e-trash-folder . "/gmail/Trash")))

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
                  (mu4e-drafts-folder . "/example/Drafts")
                  (mu4e-refile-folder . "/example/Archive")
                  (mu4e-sent-folder . "/example/Sent")
                  (mu4e-trash-folder . "/example/Trash")))

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
                  (mu4e-drafts-folder . "/example/Drafts")
                  (mu4e-refile-folder . "/example/Archive")
                  (mu4e-sent-folder . "/example/Sent")
                  (mu4e-trash-folder . "/example/Trash")))


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
                  (mu4e-drafts-folder . "/example/Drafts")
                  (mu4e-refile-folder . "/example/Archive")
                  (mu4e-sent-folder . "/example/Sent")
                  (mu4e-trash-folder . "/example/Trash")))
	))

(setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
(setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;


;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epg-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)

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
	       ((string-match "maximilian.brosius@uni-hamburg.de" from) "uhhmail"))))
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

(provide 'a-mu4e)
