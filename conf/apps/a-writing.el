(use-package academic-phrases)

(use-package powerthesaurus)


(use-package lsp-grammarly
  :hook ((text-mode mu4e-compose-mode) . (lambda ()
					   (require 'lsp-grammarly)
					   (lsp))))


(setq lsp-language-id-configuration
      (append '((mu4e-compose-mode . "plaintext")) lsp-language-id-configuration))

(setq lsp-grammarly-active-modes
      (add-to-list 'lsp-grammarly-active-modes 'mu4e-compose-mode))

;; (setq lsp-grammarly-server "grammarly-languageserver")

;; (lsp-register-client
;;  (make-lsp-client
;;   :new-connection (lsp-stdio-connection '("grammarly-languageserver" "--stdio"))
;;   :major-modes '(mu4e-compose-mode text-mode)
;;   :server-id 'grammarly-ls
;;   :priority -1))


(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-grammarly--server-command)
  :initialization-options
  `((clientId . ,lsp-grammarly-client-id)
    (name . "Grammarly"))
  :major-modes lsp-grammarly-active-modes
  :priority -1
  :add-on? t
  :server-id 'grammarly-ls
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'grammarly-ls callback error-callback))
  :after-open-fn #'lsp-grammarly--init
  :async-request-handlers
  (ht ("$/showError" #'lsp-grammarly--show-error)
      ("$/updateDocumentState" #'lsp-grammarly--update-document-state))))



;; (setq auth-source-debug t)

(use-package chatgpt-arcana
  :straight (:host github :repo "CarlQLange/ChatGPT-Arcana.el" :files ("*.el"))
  :init (setq chatgpt-arcana-api-key (auth-source-pick-first-password
				      :host "chat.openai.com"))
  :config
  (use-package all-the-icons
    :config
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(chatgpt-arcana-chat-mode all-the-icons-octicon "comment-discussion" :height 1.0 :v-adjust -0.1 :face all-the-icons-purple))))

(provide 'a-writing)
