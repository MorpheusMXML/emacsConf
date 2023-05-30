;;----------------------------------------------------------------------------------------------------
;; company: autocompletion
(use-package company)

(setq company-idle-delay 0.0
      company-tooltip-limit 15
      company-minimum-prefix-length 1
      company-echo-delay 0
      company-auto-complete nil
      company-show-numbers 0)
(add-hook 'prog-mode-hook 'company-mode)

(use-package company-box
  :hook (company-mode . company-box-mode))

(general-def
  :states '(normal insert)
  :keymaps 'company-mode-map
  "C-SPC" 'company-search-candidates)

;; COMPANY-PRESCIENT: better autocomplete suggestions based on past selections
(use-package company-prescient)
;; hooks
(add-hook 'company-mode-hook 'company-prescient-mode)

;; ---------------------------------------------------------------------------------------------------
;; YASNIPPET: Code Snippets

(use-package yasnippet)
(yas-global-mode t)
(use-package yasnippet-snippets
  :straight t
  :defer t)

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
(use-package lsp-mode
  :straight t
  :hook ((js-mode
	  js-jsx-mode
	  python-mode
	  LaTex-mode
	  LaTeX-math-mode))
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

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

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)
(lsp-treemacs-sync-mode 1)

(use-package lsp-ivy)

(use-package platformio-mode
  :straight t)

(use-package arduino-mode
  :straight t)

(use-package company-arduino
  :straight t)

;; FIC: Highlight TODO/FIXME/etc. in code comments
(straight-use-package 'fic-mode)
(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
(add-hook 'prog-mode-hook 'fic-mode)

;; Additional languages with more config
(push (concat user-emacs-directory "conf/lang") load-path)
;; load languages if required

;; WORKAROUND TODO FIXME
;; since AUCTEX sets Local vars in the .tex files it also sets the Mode to latex
;; this somehow causes issues with the auto mode list call loading the l-latex config....
(require 'l-latex)

;; (push '("\\.rs\\'" . (lambda () (require 'l-rust) (rust-mode))) auto-mode-alist)
(push '("\\.go\\'" . (lambda () (require 'l-go) (go-mode))) auto-mode-alist)
;; (push '("\\.\\([ch]pp\\|cc\\)\\'" . (lambda () (require 'l-cc) (c++-mode))) auto-mode-alist)
;; (push '("\\.[ch]\\'" . (lambda () (require 'l-cc) (c-mode))) auto-mode-alist)
(push '("\\.tex\\'" . (lambda () (require 'l-latex) (LaTeX-mode))) auto-mode-alist)
(push '("\\.el\\'" . (lambda () (require 'l-elisp) (emacs-lisp-mode))) auto-mode-alist)
;; (push '("\\.js\\'" . (lambda () (require 'lsp-typescript) (javascript-mode))) auto-mode-alist)
(push '("\\.py\\'" . (lambda () (require 'l-python) (python-mode))) auto-mode-alist)
;; (push '("python[0-9.]*" . (lambda () (require 'l-python) (python-mode))) interpreter-mode-alist)
(push '("\\.cs\\'" . (lambda () (require 'l-csharp) (csharp-mode))) auto-mode-alist)
;; (push '("\\.java\\'" . (lambda () (require 'l-java) (java-mode))) auto-mode-alist)
(push '("\\.\\(tsx?\\|jsx?\\)\\'" . (lambda () (require 'l-typescript) (web-tide-mode))) auto-mode-alist)
(push '("\\.\\(xml\\|html\\|php\\|css\\)\\'" . (lambda () (require 'l-web) (web-mode))) auto-mode-alist)


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
  "SPC l" '(:ignore t :which-key "LSP Server")
  "SPC l i" 'lsp-organize-imports
  "SPC l c" 'lsp-describe-session
  "SPC l r" 'lsp-restart-workspace
  "SPC g" '(:ignore t :which-key "go to")
  "SPC g d" 'lsp-find-definition
  "SPC g D" 'lsp-find-declaration
  "SPC g t" 'lsp-goto-type-definition
  "SPC r" 'lsp-rename
  "SPC d" 'lsp-ui-doc-toggle
  "SPC i" 'imenu
  "SPC t" 'lsp-treemacs-errors-list
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
;; (use-package js2-mode)
;; (push '("\\.\\(js\\)\\'" . js2-mode) auto-mode-alist)
;; ;; sudo npm i -g javascript-typescript-langserver
;; (add-hook 'js2-mode-hook #'lsp-deferred)

;; JAVASCRIPT RJSX Mode
;; (use-package rjsx-mode
;;   :straight t
;;   :mode "\\.js\\'")



;; STRUCTURE LANGUAGES: mostly highlighting and indent
;; ;; CADDY
;; (use-package caddyfile-mode)
;; (push '("\\(Caddyfile\\|caddy.conf\\)\\'" . caddyfile-mode) auto-mode-alist)

(use-package sqlite3)

;; ;;----------------------------------------------------------------------------------------------------
;; MAGIT - Mighty Git Interface
(use-package magit)

(setq auth-sources '("~/.auth/.authinfo.gpg"))

(use-package forge
  :after magit
  :init
  (setq forge-add-default-bindings nil)
  :config
  (add-to-list 'forge-alist '("git.informatik.uni-hamburg.de" "git.informatik.uni-hamburg.de/api/v4" "git.informatik.uni-hamburg.de" forge-gitlab-repository))
  (add-to-list 'forge-alist '("uni" "git.informatik.uni-hamburg.de/api/v4" "git.informatik.uni-hamburg.de" forge-gitlab-repository))
  (add-to-list 'forge-alist '("github.com" "api.github.com" "github.com" forge-github-repository))
  (add-to-list 'forge-alist '("gh" "api.github.com" "github.com" forge-github-repository)))


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
(add-hook 'prog-mode-hook #'rainbow-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))

;; EDITOR-Config
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;; IDENTATION
;; Aggressive Indent
(use-package aggressive-indent)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)

(use-package format-all)
(add-hook 'prog-mode-hook #'format-all-mode)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)

(mabr-leader
  :states '(normal visual)
  :keymaps 'magit-mode-map
  "SPC p" '(:ignore t :which-key "Forge Pull")
  "SPC p p" '(forge-pull :which-key "Pull")
  "SPC p n" '(forge-pull-notifications :which-key "Pull Notifications")

  "SPC s" '(forge-post-submit :which-key "Submit Post")

  "SPC x" '(forge-post-cancel :which-key "Cancel Post")

  "SPC c" '(:ignore t :which-key "Create...")
  "SPC c i" '(forge-create-issue :which-key "Create Issue")
  "SPC c r" '(forge-create-pullreq-from-issue :which-key "Create PullReq")
  "SPC c c" '(forge-create-post :which-key "Create Comment")

  "SPC e" '(:ignore t :which-key "Edit...")
  "SPC e p" '(forge-edit-post :which-key "Edit Post")
  "SPC e t" '(forge-edit-topic-title :which-key "Edit Title")
  "SPC e l" '(forge-edit-topic-labels :which-key "Edit Labels")
  "SPC e s" '(forge-edit-topic-state :which-key "Edit State")
  "SPC e a" '(forge-edit-topic-assignees :which-key "Edit Assignees")
  "SPC e n" '(forge-edit-topic-note :which-key "Edit Note")
  "SPC e d" '(forge-delete-comment :which-key "Delete Comment")
  )

(mabr-leader
  :states '(normal visual)
  :keymaps 'forge-post-mode-map
  ;; "SPC s" '(forge-post-submit :which-key "Submit Post")
  "SPC x" '(forge-post-cancel :which-key "Cancel Post")
  )

(provide 'programming)
