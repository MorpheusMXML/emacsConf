;; WEB
(use-package web-mode
  :straight t
  ;; :mode (("\\.html?\\'" . web-mode)
  ;;        ("\\.css\\'"   . web-mode)
  ;;        ("\\.jsx?\\'"  . web-mode)
  ;;        ("\\.tsx?\\'"  . web-mode)
  ;; 	 ("\\.js?\\'"   . web-mode)
  ;;        ("\\.json\\'"  . web-mode))
  )


;; EMMET
(use-package emmet-mode
  :straight t
  ;; :hook ((html-mode       . emmet-mode)
  ;;        (css-mode        . emmet-mode)
  ;;        (js-mode         . emmet-mode)
  ;;        (js-jsx-mode     . emmet-mode)
  ;;        (typescript-mode . emmet-mode)
  ;;        (web-mode        . emmet-mode))
  )
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)


(mabr-leader
  :states 'normal
  :definer 'minor-mode
  :keymaps 'emmet-mode
  "SPC e" '((lambda() (interactive) (evil-append 1) (emmet-expand-line nil))
            :which-key "expand emmet"))

;; TAILWINDCSS
(use-package lsp-tailwindcss)

(provide 'l-web)
