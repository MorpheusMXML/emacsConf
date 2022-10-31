;; install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 145)

;;Install Use-Package
(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

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

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook))
               ; shell-mode-hook
               ; eshell-mode-hook
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package command-log-mode)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15))) 


(use-package doom-themes
  :init (load-theme 'doom-dark+ t))

(mac-auto-operator-composition-mode)

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



;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.emacs.d/.local/env
;; use this command to create the file  `printenv > $HOME/.emacs.d/.local/env'
(defconst my-local-dir (concat user-emacs-directory ".local/"))

(defconst my-env-file (concat my-local-dir "env"))

(defun my-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (not (file-readable-p file))
      (unless noerror
        (signal 'file-error (list "Couldn't read envvar file" file)))
    (let (envvars environment)
      (with-temp-buffer
        (save-excursion
          (insert "\n")
          (insert-file-contents file))
        (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
          (push (match-string 1) envvars)
          (push (buffer-substring
                 (match-beginning 1)
                 (1- (or (save-excursion
                           (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                             (line-beginning-position)))
                         (point-max))))
                environment)))
      (when environment
        (setq process-environment
              (append (nreverse environment) process-environment)
              exec-path
              (if (member "PATH" envvars)
                  (append (split-string (getenv "PATH") path-separator t)
                          (list exec-directory))
                exec-path)
              shell-file-name
              (if (member "SHELL" envvars)
                  (or (getenv "SHELL") shell-file-name)
                shell-file-name))
        envvars))))

(when (and (or (display-graphic-p)
               (daemonp))
           (file-exists-p my-env-file))
  (my-load-envvars-file my-env-file))
;;; Code to replace exec-path-from-shell

(setq epa-pinentry-mode 'loopback)


;; Org Mode Configuration ------------------------------------------------------

(defun mabr/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun mabr/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun mabr/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mabr/org-mode-visual-fill))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(package-selected-packages
   '(keychain-environment exec-path-from-shell evil-collection magit counsel-projectile projectile drag-stuff evil-nerd-commenter hydra which-key use-package undo-tree spacemacs-theme rainbow-delimiters ivy-rich helpful general evil doom-themes doom-modeline counsel company command-log-mode ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


