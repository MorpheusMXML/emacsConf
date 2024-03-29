;; ;; install straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;; You will most likely need to adjust this font size for your system!
;; (defvar runemacs/default-font-size 145)

;; ;;Install Use-Package
;; (straight-use-package 'use-package)

;; (use-package straight
;;   :custom (straight-use-package-by-default t))

;; (setq inhibit-startup-message t)
;; ;; set keys for Apple keyboard, for emacs in OS X
;; (setq mac-command-modifier 'meta) ; make cmd key do Meta
;; (setq mac-option-modifier nil) ; make opt key do Super
;; (setq mac-control-modifier 'control) ; make Control key do Control
;; (setq ns-function-modifier 'hyper);; set keys for Apple keyboard, for emacs in OS X

;; (scroll-bar-mode -1)        ; Disable visible scrollbar
;; (tool-bar-mode -1)          ; Disable the toolbar
;; (tooltip-mode -1)           ; Disable tooltips
;; (set-fringe-mode 10)        ; Give some breathing room

;; ;; Set up the visible bell
;; (setq visible-bell t)

;; (set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; ;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ;; Initialize package sources
;; ;(require 'package)

;; ;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;; ;                         ("org" . "https://orgmode.org/elpa/")
;; ;                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; ;(package-initialize)
;; ;(unless package-archive-contents
;; ; (package-refresh-contents))

;; ;; Initialize use-package on non-Linux platforms
;; ;(unless (package-installed-p 'use-package)
;; ;   (package-install 'use-package))

;; ;(require 'use-package)
;; ;(setq use-package-always-ensure t)

;; (column-number-mode)
;; (global-display-line-numbers-mode t)

;; ;; Disable line numbers for some modes
;; (dolist (mode '(org-mode-hook
;;                 term-mode-hook))
;;                ; shell-mode-hook
;;                ; eshell-mode-hook
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (use-package command-log-mode)

;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)         :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode 1))
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15))) 


;; (use-package doom-themes
;;   :init (load-theme 'doom-dark+ t))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package which-key
;;   :init (which-key-mode)
;;   :diminish which-key-mode
;;   :config
;;   (setq which-key-idle-delay 1))



;; (use-package counsel
;;   :bind (("M-x" . counsel-M-x)
;; 	 ("C-x b" . counsel-ibuffer)
;; 	 ("C-x C-f" . counsel-find-file)
;; 	 :map minibuffer-local-map
;; 	 ("C-r" . 'counsel-minibuffer-history)))

;; (use-package ivy-rich
;;   :straight t
;;   :after counsel
;;   :init
;;   (ivy-rich-mode 1))

;; (use-package helpful
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

;; (mac-auto-operator-composition-mode)

;; (use-package undo-fu)
;; (use-package undo-fu-session)
;; (use-package vundo)

;; ;; EVIL:
;; ;; depends on: goto-chg, undo-tree

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-undo-system 'undo-fu)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-C-i-jump nil)
;;   :config
;;   (evil-mode 1)
;;   ;use C-g to quit Insert Mode like ESCape
;;   (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;;   ;use C-h to Delete Char like Backspace in Insert mode
;;   (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
;;   ;use j and k to jump to next Visual line if line is split and not jump to actual Line
;;   (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;;   (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; ;; https://github.com/emacs-evil/evil/issues/1288
;; ;; Credit goes to: https://github.com/nnicandro
;; ;; Fix for the broken org-src-tab-acts-natively functionality
;; ;; Tab in fact does nothing in src blocks if evil is enabled
;; (defun evil-org-insert-state-in-edit-buffer (fun &rest args)
;;   "Bind `evil-default-state' to `insert' before calling FUN with ARGS."
;;   (let ((evil-default-state 'insert)
;;         ;; Force insert state
;;         evil-emacs-state-modes
;;         evil-normal-state-modes
;;         evil-motion-state-modes
;;         evil-visual-state-modes
;;         evil-operator-state-modes
;;         evil-replace-state-modes)
;;     (apply fun args)
;;     (evil-refresh-cursor)))

;; (advice-add 'org-babel-do-key-sequence-in-edit-buffer
;;             :around #'evil-org-insert-state-in-edit-buffer)


;; (use-package general)
;; (general-create-definer mabr-leader
;;   :prefix "<SPC>"
;;   :non-normal-prefix "C-SPC")

;; ;; EMACS-WIDE BINDINGS
;; (mabr-leader
;;   :states '(normal visual emacs)
;;   :keymaps 'override

;;   "SPC" '(:ignore t :which-key "Minormode Binds")
;;   "SPC p" 'fill-paragraph
;;   "SPC SPC" 'counsel-m-x
   
;;   "a" 'ace-select-window
;;   "d" 'dired
;;   "D" 'dired-other-window
;;   "f" 'counsel-find-file
;;   "F" 'phga/find-file-sudo
;;   "g" 'magit-status
;;   "h" 'evil-avy-goto-line
;;   "i" 'counsel-imenu
;;   "t" '(hydra-text-scale/body :which-key "Scale Text")
;;   "y" 'counsel-yank-pop

;;   "8" 'insert-char

;;   "/" 'swiper
;;   ";" 'evilnc-comment-or-uncomment-lines
;;   "." 'save-buffer
;;  ;")" 'evil-forward-section-end
;;  ;"(" 'evil-backward-section-begin
  
;;  ;; window stuff
;;   "w" '(:ignore t :which-key "Window")
;;   "w w" 'other-window
;;   "w v" 'split-window-right
;;   "w h" 'split-window-below
;;   "w f" 'toggle-maximize-buffer
;;   "w L" 'windsize-right
;;   "w H" 'windsize-left
;;   "w J" 'windsize-down
;;   "w K" 'windsize-up

;;   ;; switch stuff
;;   "s" '(:ignore t :which-key "Switch")
;;   "s b" 'switch-to-buffer
;;   "s n" 'switch-to-next-buffer
;;   "s p" 'switch-to-prev-buffer
;;   "s s" 'counsel-switch-buffer
;;   "s w" 'ace-swap-window

;;   ;; kill stuff
;;   "k" '(:ignore t :which-key "Kill")
;;   "k b" 'kill-buffer
;;   "k w" 'delete-window
;;   "k W" 'kill-buffer-and-window
;;   "k o b" 'kill-some-buffers
;;   "k o w" 'delete-other-windows
;;   "k q q" 'kill-emacs
;;   "k k" 'kill-current-buffer

;;   "?" '(:ignore t :which-key "Describe")
;;   "? v" 'describe-variable
;;   "? f" 'describe-function
;;   "? c" 'describe-char
;;   "? m" 'describe-mode
;;   "? k" 'describe-key
;;   "? F" 'describe-face
;;   )

;; (use-package hydra)
;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale Text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finish" :exit t))

;; (use-package evil-nerd-commenter)	
;; ;;Define M-; as standard Keys for comm/uncomm Lines or Blocks
;; (evilnc-default-hotkeys)

;; (use-package drag-stuff)
;; (require 'drag-stuff)
;; (add-hook 'prog-mode-hook 'drag-stuff-mode)
;; ;; (setq drag-stuff-modifier 'shift)
;; (drag-stuff-define-keys)

;; ;; electric-pair: auto-balance brackets
;; (electric-pair-mode t)
;; ;; electric-indent-mode is a bit buggy in src blocks...  unfortunately
;; ;; it adds a bug, where the cursor jumps to the start of the src block
;; ;; after inserting a nested block of any sort {}, [], () and pressing
;; ;; ret this is triggered by: org-return -> newline-and-indent adding
;; ;; an advice did not work, since the point must not only be recovered
;; ;; but also increased by the level of indention... also, it seemed
;; ;; like another function is responsible for the reset since i added
;; ;; the advie to org-babel-do-key-sequence-in-edit-buffer (probably
;; ;; newline-and-indent)

;; ;; company: autocompletion
;; (use-package company)

;; (setq company-idle-delay 0.0
;;       company-tooltip-limit 5
;;       company-minimum-prefix-length 3
;;       company-echo-delay 0
;;       company-auto-complete nil
;;       company-show-numbers t)
;; (add-hook 'prog-mode-hook 'company-mode)

;; ;; keybindings
;; ;; all rights belong to my beloved friend phga <3 du würschtal
;; ;(defun mabr/keymap-company ()
;; ;  (interactive)
;; ;  (general-def
;; ;	  :keymaps 'company-active-map
;; ;	  "<tab>" 'company-complete-selection
;; ;    "<return>" nil
;; ;    "ret" nil))

;; (general-def
;;   :states '(normal insert)
;;   :keymaps 'company-mode-map
;;   "M-RET" 'company-search-candidates)


;; ;; ACE-WINDOW: jump between windows faster
;; (straight-use-package 'ace-window)
;; (setq aw-scope 'frame
;;       aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

;; (use-package projectile
;;   :ensure t
;;   :diminish projectile-mode
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("s-p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/Documents/Dev")
;;     (setq projectile-project-search-path '("~/Documents/Dev")))
;;   (setq projectile-switch-project-action #'projectile-dired)
;;   (projectile-mode +1))


;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

;; ;(add-hook 'company-mode-hook 'mabr/keymap-company)

;; (use-package magit)

;; ;; (use-package evil-collection
;; ;;   :after evil
;; ;;   :ensure t
;; ;;   :config
;; ;;   (evil-collection-init))

;; ;; EVIL-COLLECTION: improved evil support for multiple packages
;; (straight-use-package 'evil-collection)
;; ;; variables
;; (setq evil-collection-setup-minibuffer t
;;       evil-collection-mode-list
;;       '(ibuffer help calc nov man calendar ivy minibuffer dired debug
;;         doc-view arc-mode magit vterm))
;; ;; start mode
;; (evil-collection-init)

;; ;; Code to replace exec-path-from-shell
;; ;; Need to create file in $HOME/.emacs.d/.local/env
;; ;; use this command to create the file  `printenv > $HOME/.emacs.d/.local/env'
;; (defconst my-local-dir (concat user-emacs-directory ".local/"))

;; (defconst my-env-file (concat my-local-dir "env"))

;; (defun my-load-envvars-file (file &optional noerror)
;;   "Read and set envvars from FILE.
;; If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
;; unreadable. Returns the names of envvars that were changed."
;;   (if (not (file-readable-p file))
;;       (unless noerror
;;         (signal 'file-error (list "Couldn't read envvar file" file)))
;;     (let (envvars environment)
;;       (with-temp-buffer
;;         (save-excursion
;;           (insert "\n")
;;           (insert-file-contents file))
;;         (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
;;           (push (match-string 1) envvars)
;;           (push (buffer-substring
;;                  (match-beginning 1)
;;                  (1- (or (save-excursion
;;                            (when (re-search-forward "^\\([^= ]+\\)=" nil t)
;;                              (line-beginning-position)))
;;                          (point-max))))
;;                 environment)))
;;       (when environment
;;         (setq process-environment
;;               (append (nreverse environment) process-environment)
;;               exec-path
;;               (if (member "PATH" envvars)
;;                   (append (split-string (getenv "PATH") path-separator t)
;;                           (list exec-directory))
;;                 exec-path)
;;               shell-file-name
;;               (if (member "SHELL" envvars)
;;                   (or (getenv "SHELL") shell-file-name)
;;                 shell-file-name))
;;         envvars))))

;; (when (and (or (display-graphic-p)
;;                (daemonp))
;;            (file-exists-p my-env-file))
;;   (my-load-envvars-file my-env-file))
;; ;;; Code to replace exec-path-from-shell

;; (setq epa-pinentry-mode 'loopback)


;; ;; Org Mode Configuration ------------------------------------------------------

;; (defun mabr/org-mode-setup ()
;;   (org-indent-mode)
;;   (variable-pitch-mode 1)
;;   (visual-line-mode 1))


;; (defun mabr/org-font-setup ()
;;   ;; Replace list hyphen with dot
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;   ;; Set faces for heading levels
;;   (dolist (face '((org-level-1 . 1.2)
;;                   (org-level-2 . 1.1)
;;                   (org-level-3 . 1.05)
;;                   (org-level-4 . 1.0)
;;                   (org-level-5 . 1.1)
;;                   (org-level-6 . 1.1)
;;                   (org-level-7 . 1.1)
;;                   (org-level-8 . 1.1)))
;;     (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))

;;   ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;;   (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;;   (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;;   (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;;   (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; (use-package org
;;   :hook (org-mode . mabr/org-mode-setup)
;;   :config
;;   (setq org-ellipsis " ▾")
;;   (mabr/org-font-setup))

;; (use-package org-bullets
;;   :after org
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (defun mabr/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 100
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . mabr/org-mode-visual-fill))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
;;  '(package-selected-packages
;;    '(keychain-environment exec-path-from-shell evil-collection magit counsel-projectile projectile drag-stuff evil-nerd-commenter hydra which-key use-package undo-tree spacemacs-theme rainbow-delimiters ivy-rich helpful general evil doom-themes doom-modeline counsel company command-log-mode ace-window)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


