;;---------------------------------------------------------------------------------------------------- 
;; company: autocompletion
(use-package company)

(setq company-idle-delay 0.0
      company-tooltip-limit 5
      company-minimum-prefix-length 3
      company-echo-delay 0
      company-auto-complete nil
      company-show-numbers t)
(add-hook 'prog-mode-hook 'company-mode)

(general-def
  :states '(normal insert)
  :keymaps 'company-mode-map
  "C-<return>" 'company-search-candidates)

;; COMPANY-PRESCIENT: better autocomplete suggestions based on past selections
(use-package company-prescient)
;; hooks
(add-hook 'company-mode-hook 'company-prescient-mode)

;; ---------------------------------------------------------------------------------------------------
;; YASNIPPET: Code Snippets

(use-package yasnippet)
(use-package yasnippet-snippets)

(require 'yasnippet)
(yas-reload-all)
;; hooks
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
;; (add-hook 'yas-minor-mode-hook 'yas-reload-all)

;;----------------------------------------------------------------------------------------------------
;; PROJECTILE: Project Management
(use-package projectile)
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(mabr-leader
  :states '(normal visual)
  :keymaps 'override
  "p" '(:ignore t :which-key "Projectile")
  "p f" 'projectile-find-file
  "p s" 'projectile-switch-project
  "p i" 'projectile-install-project
  "p c" 'projectile-compile-project
  "p R" 'projectile-run-project
  "p t" 'projectile-test-project
  "p r" 'counsel-projectile-rg
  )

;; ---------------------------------------------------------------------------------------------------
;; FLYCHECK: on the fly syntax checking
(use-package flycheck)
(setq flycheck-highlighting-mode 'symbols) ;; full symbols not cols
;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
(add-hook 'prog-mode-hook #'flycheck-mode)

;; Keybindings
(general-def :states '(normal visual) :keymaps 'prog-mode-map
  "g F" 'dired-at-point
  "g ! n" 'flycheck-next-error
  "g ! p" 'flycheck-previous-error
  "g ! l" 'flycheck-list-errors
  )
;;----------------------------------------------------------------------------------------------------
;; MAGIT - Mighty Git Interface
(use-package magit)



;; JSON
(use-package json-mode)
(push '("\\.\\(json\\|imp\\)\\'" . json-mode) auto-mode-alist)
(add-hook 'json-mode-hook
          (lambda () (json-pretty-print-buffer)))

;; CSV
(use-package csv-mode)
(push '("\\.\\(csv\\|xls\\)\\'" . csv-mode) auto-mode-alist)

;; YAML
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq evil-shift-width yaml-indent-offset)))


;; RAINBOW
(use-package rainbow-mode)

;; EDITOR-Config
(use-package editorconfig
   :straight t
   :config
  (editorconfig-mode 1))

(provide 'programming)
