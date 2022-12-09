;; -----------------------------------------------------------------------------------------------------------
;; ORG-MODE:
;;Import ORG Mode as one of the First Pkgs in order to prevent other dependencies pulling outdated versions.
(use-package org)
(use-package exec-path-from-shell)
(exec-path-from-shell-copy-env "PYTHONPATH")
;; (exec-path-from-shell-copy-env "GPG_TTY")
(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" ))
  (add-to-list 'exec-path-from-shell-variables var))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; -----------------------------------------------------------------------------------------------------------
;;GENERAL: Keybindings
(use-package general)
(general-create-definer mabr-leader
  :prefix "<SPC>"
  :non-normal-prefix "C-SPC")

;; -----------------------------------------------------------------------------------------------------------
;; UNDO
(use-package undo-fu)
(use-package undo-fu-session)
(use-package vundo)

;; -----------------------------------------------------------------------------------------------------------
;; EVIL - MODE
;; dependencies: goto-chg, undo-tree
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  ;use j and k to jump to next Visual line if line is split and not jump to actual Line
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; https://github.com/emacs-evil/evil/issues/1288
;; Credit goes to: https://github.com/nnicandro
;; Fix for the broken org-src-tab-acts-natively functionality
;; Tab in fact does nothing in src blocks if evil is enabled
(defun evil-org-insert-state-in-edit-buffer (fun &rest args)
  "Bind `evil-default-state' to `insert' before calling FUN with ARGS."
  (let ((evil-default-state 'insert)
        ;; Force insert state
        evil-emacs-state-modes
        evil-normal-state-modes
        evil-motion-state-modes
        evil-visual-state-modes
        evil-operator-state-modes
        evil-replace-state-modes)
    (apply fun args)
    (evil-refresh-cursor)))

(advice-add 'org-babel-do-key-sequence-in-edit-buffer
            :around #'evil-org-insert-state-in-edit-buffer)

;; -----------------------------------------------------------------------------------------------------------
;; EVIL-NERD-COMMENTER
(use-package evil-nerd-commenter)	
;;Define M-; as standard Keys for comm/uncomm Lines or Blocks
(evilnc-default-hotkeys)

;; -----------------------------------------------------------------------------------------------------------
;; DRAG-STUFF - Drag Lines through file
(use-package drag-stuff)
(require 'drag-stuff)
(add-hook 'prog-mode-hook 'drag-stuff-mode)
;; (setq drag-stuff-modifier 'shift)
(drag-stuff-define-keys)


;; -----------------------------------------------------------------------------------------------------------
;; ELECTRIC-PAIRS & RAINBOW
;; electric-pair: auto-balance brackets
;; rainbow-del: colorize Brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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


;; -----------------------------------------------------------------------------------------------------------
;; EVIL-COLLECTION: improved evil support for multiple packages
(use-package evil-collection)
;; variables
(setq evil-collection-setup-minibuffer t
      evil-collection-mode-list
      '(ibuffer help calc nov man calendar ivy minibuffer dired debug
        doc-view arc-mode magit vterm))
;; start mode
(evil-collection-init)


;; -----------------------------------------------------------------------------------------------------------
;; IVY: better Popup Menus
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-j" . ivy-next-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; -----------------------------------------------------------------------------------------------------------
;; IVY-PRESCIENT: better suggestions for ivy
(use-package ivy-prescient)
;; start mode
(ivy-prescient-mode)

(use-package ivy-rich
  :straight t
  :after counsel
  :init
  (ivy-rich-mode 1))
;; -----------------------------------------------------------------------------------------------------------
;; SWIPER: / search
(use-package swiper)

;; -----------------------------------------------------------------------------------------------------------
;; WHICH-KEY: key hints in MiniBar
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; -----------------------------------------------------------------------------------------------------------
;; COUNSEL: Improved functions like find file etc.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; -----------------------------------------------------------------------------------------------------------
;; HELPFUL - Explanations for Functions
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; -----------------------------------------------------------------------------------------------------------
;; ACE-WINDOW: jump between windows faster
(use-package ace-window)
(setq aw-scope 'frame
      aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

;; MESSAGE BUFFER:
(evil-initial-state 'message-mode 'normal)


(provide 'corepkg)
