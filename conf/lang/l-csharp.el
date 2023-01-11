;; C# // UNITY Setup

(setenv "FrameworkPathOverride" "/Library/Frameworks/Mono.framework/Versions/Current")

(use-package csharp-mode
  :ensure t
  :init
  (defun my/csharp-mode-hook ()
    (setq-local lsp-auto-guess-root t)
    (lsp))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook))


;; Unity Packages: ---------------------------------------------------------------------------------------
;; (straight-use-package
;;  '(unity :type git :host github :repo "elizagamedev/unity.el"))
;; (add-hook 'after-init-hook #'unity-mode)
(provide 'l-csharp)
