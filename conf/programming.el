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
(use-package yasnippet-snippets
  :straight t
  :defer t)

(require 'yasnippet)
(yas-reload-all)
;; hooks
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
;; (add-hook 'yas-minor-mode-hook 'yas-reload-all)

;;----------------------------------------------------------------------------------------------------
;; PROJECTILE: Project Management
(use-package projectile
  :straight t
  :defer t
  :config
  (projectile-mode))

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

;; LSP: language server in emacs
;; In case shit breaks -> goto github page - search for stable lable - reset to commit xxxxxx
(use-package lsp-mode)
(setq lsp-eldoc-render-all nil
      read-process-output-max 1048576
      lsp-idle-delay 0.500
      lsp-prefer-capf t
      lsp-enable-indentation nil
      lsp-headerline-breadcrumb-enable-diagnostics nil)
;; The lsp-prefer-capf variable did not work anymore and probably another company backend
;; was the source of the weird flycheck/make errors which arised with any lsp-backend for c++
;; This fixes the issue by changing the company backend to the one recommended by the lsp team
(add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf))))

(use-package lsp-ui)
(with-eval-after-load 'lsp (require 'lsp-ui-flycheck))
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(setq lsp-enable-symbol-highlighting t
      lsp-lens-enable nil ;; ref count
      lsp-ui-doc-enable t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-max-height 30
      lsp-ui-doc-max-width 200
      lsp-ui-peek-enable nil
      lsp-ui-flycheck-enable t
      lsp-ui-sideline-enable nil)

;; Keybindings
(general-def
  :states '(normal)
  :keymaps 'lsp-ui-doc-frame-mode-map
  "TAB" 'lsp-ui-doc-unfocus-frame)

(general-def
  :states '(normal)
  :keymaps 'lsp-ui-doc-mode-map
  "TAB" 'lsp-ui-doc-focus-frame)

(mabr-leader
  :states 'normal
  :keymaps 'lsp-mode-map
  "SPC l i" 'lsp-organize-imports
  "SPC l c" 'lsp-describe-session
  "SPC l r" 'lsp-restart-workspace
  "SPC g d" 'lsp-find-definition
  "SPC g D" 'lsp-find-declaration
  "SPC g t" 'lsp-goto-type-definition
  "SPC r" 'lsp-rename
  "SPC d" 'lsp-ui-doc-toggle
  "SPC i" 'imenu
  "SPC p i" 'ivy-imenu-anywhere)

;; DAP: Debug Adapter Protocol is a wire protocol for communication
;;      between client and Debug Server
(use-package dap-mode)
(dap-auto-configure-mode)

;; DIRENV: works by invoking direnv to obtain the environment for the current file, then
;; updating the emacs variables process-environment and exec-path
(use-package direnv)
(advice-add 'lsp :before #'direnv-update-environment)

;; OTHER SMALL LANGUAGES: with nearly no setup
;; JS
;; (straight-use-package 'js2-mode)
;; (push '("\\.\\(js\\)\\'" . js2-mode) auto-mode-alist)
;; ;; sudo npm i -g javascript-typescript-langserver
;; (add-hook 'js2-mode-hook #'lsp-deferred)

;; STRUCTURE LANGUAGES: mostly highlighting and indent
;; CADDY
(use-package caddyfile-mode)
(push '("\\(Caddyfile\\|caddy.conf\\)\\'" . caddyfile-mode) auto-mode-alist)
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
