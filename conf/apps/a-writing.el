(use-package academic-phrases)

(use-package powerthesaurus)


(use-package lsp-grammarly
  :hook (text-mode . (lambda ()
		       (require 'lsp-grammarly)
		       (lsp))))

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
