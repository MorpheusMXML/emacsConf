
;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 145)


(setq inhibit-startup-message t)
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper);; set keys for Apple keyboard, for emacs in OS X

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook))
               ; shell-mode-hook
               ; eshell-mode-hook
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))) 


(use-package doom-themes
  :init (load-theme 'doom-dark+ t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))


(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(mac-auto-operator-composition-mode)

;; EVIL:
;; depends on: goto-chg, undo-tree

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ;use C-g to quit Insert Mode like ESCape
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;use C-h to Delete Char like Backspace in Insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;use j and k to jump to next Visual line if line is split and not jump to actual Line
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package general)
(general-create-definer mabr-leader
  :prefix "<SPC>"
  :non-normal-prefix "C-SPC")

;; EMACS-WIDE BINDINGS
(mabr-leader
  :states '(normal visual emacs)
  :keymaps 'override

  "SPC" '(:ignore t :which-key "Minormode Binds")
  "SPC p" 'fill-paragraph
  "SPC SPC" 'counsel-m-x
   
  "a" 'ace-select-window
  "d" 'dired
  "D" 'dired-other-window
  "f" 'counsel-find-file
  "F" 'phga/find-file-sudo
  "g" 'magit-status
  "h" 'evil-avy-goto-line
  "i" 'counsel-imenu
  "t" '(hydra-text-scale/body :which-key "Scale Text")
  "y" 'counsel-yank-pop

  "8" 'insert-char

  "/" 'swiper
  ";" 'evilnc-comment-or-uncomment-lines
  "." 'save-buffer
 ;")" 'evil-forward-section-end
 ;"(" 'evil-backward-section-begin
  
 ;; window stuff
  "w" '(:ignore t :which-key "Window")
  "w w" 'other-window
  "w v" 'split-window-right
  "w h" 'split-window-below
  "w f" 'toggle-maximize-buffer
  "w L" 'windsize-right
  "w H" 'windsize-left
  "w J" 'windsize-down
  "w K" 'windsize-up

  ;; switch stuff
  "s" '(:ignore t :which-key "Switch")
  "s b" 'switch-to-buffer
  "s n" 'switch-to-next-buffer
  "s p" 'switch-to-prev-buffer
  "s s" 'counsel-switch-buffer
  "s w" 'ace-swap-window

  ;; kill stuff
  "k" '(:ignore t :which-key "Kill")
  "k b" 'kill-buffer
  "k w" 'delete-window
  "k W" 'kill-buffer-and-window
  "k o b" 'kill-some-buffers
  "k o w" 'delete-other-windows
  "k q q" 'kill-emacs
  "k k" 'kill-current-buffer

  "?" '(:ignore t :which-key "Describe")
  "? v" 'describe-variable
  "? f" 'describe-function
  "? c" 'describe-char
  "? m" 'describe-mode
  "? k" 'describe-key
  "? F" 'describe-face
  )

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale Text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finish" :exit t))

(use-package evil-nerd-commenter)	
;;Define M-; as standard Keys for comm/uncomm Lines or Blocks
(evilnc-default-hotkeys)

(use-package drag-stuff)
(require 'drag-stuff)
(add-hook 'prog-mode-hook 'drag-stuff-mode)
;; (setq drag-stuff-modifier 'shift)
(drag-stuff-define-keys)

;; electric-pair: auto-balance brackets
(electric-pair-mode t)
;; electric-indent-mode is a bit buggy in src blocks...  unfortunately
;; it adds a bug, where the cursor jumps to the start of the src block
;; after inserting a nested block of any sort {}, [], () and pressing
;; ret this is triggered by: org-return -> newline-and-indent adding
;; an advice did not work, since the point must not only be recovered
;; but also increased by the level of indention... also, it seemed
;; like another function is responsible for the reset since i added
;; the advie to org-babel-do-key-sequence-in-edit-buffer (probably
;; newline-and-indent)

;; company: autocompletion
(use-package company)

(setq company-idle-delay 0.0
      company-tooltip-limit 5
      company-minimum-prefix-length 3
      company-echo-delay 0
      company-auto-complete nil
      company-show-numbers t)
(add-hook 'prog-mode-hook 'company-mode)

;; keybindings
;; all rights belong to my beloved friend phga <3 du würschtal
;(defun mabr/keymap-company ()
;  (interactive)
;  (general-def
;	  :keymaps 'company-active-map
;	  "<tab>" 'company-complete-selection
;    "<return>" nil
;    "ret" nil))

(general-def
  :states '(normal insert)
  :keymaps 'company-mode-map
  "M-RET" 'company-search-candidates)

(setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("s-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/Dev")
    (setq projectile-project-search-path '("~/Documents/Dev")))
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode +1))


(use-package counsel-projectile
  :config (counsel-projectile-mode))

;(add-hook 'company-mode-hook 'mabr/keymap-company)

(use-package magit)

(use-package evil-magit)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(package-selected-packages
   '(magit counsel-projectile projectile drag-stuff evil-nerd-commenter hydra which-key use-package undo-tree spacemacs-theme rainbow-delimiters ivy-rich helpful general evil doom-themes doom-modeline counsel company command-log-mode ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


