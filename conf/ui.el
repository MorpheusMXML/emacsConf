;;UI Settings

;; You will most likely need to adjust this font size for your system!
(defvar mabr/default-font-size 147)

;; FULLSCREN ON STARTUP
;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))
;; This is a test.  
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t)
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'nil) ; make opt key not do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper);; set keys for Apple keyboard, for emacs in OS X

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 5)        ; Give some breathing room
(setq-default indicate-empty-lines t)
;; Set up the visible bell
(setq visible-bell t)
(setq vc-follow-symlinks t)


(set-face-attribute 'default nil :font "Fira Code Retina" :height mabr/default-font-size)

(column-number-mode)
(global-display-line-numbers-mode t)

(rainbow-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook))
               ; shell-mode-hook
               ; eshell-mode-hook
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package all-the-icons
  :straight t
  :defer t)

(use-package all-the-icons-dired
  :straight t
  :defer t)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
	   (doom-modeline-enable-word-count t))) 


;; (use-package doom-themes
;;   :init (load-theme 'doom-dark+ t))

(use-package shanty-themes
  :init (load-theme 'shanty-themes-dark))
(setq mabr/dark-theme 'shanty-themes-dark)
(setq mabr/light-theme 'shanty-themes-light)

(set-face-attribute 'dired-ignored nil :foreground "#546a7b")

(setq mabr/current-theme mabr/dark-theme)
(load-theme mabr/current-theme t)

(defun mabr/cycle-themes ()
  (interactive)
  "Cycle through my preferred dark and light theme"
  (if (eq mabr/current-theme mabr/dark-theme)
      (progn
        (disable-theme mabr/dark-theme)
        (load-theme mabr/light-theme t)
        (message "Loaded light theme")
        (setq mabr/current-theme mabr/light-theme))
    (progn
      (disable-theme mabr/light-theme)
      (load-theme mabr/dark-theme t)
      (message "Loaded dark theme")
      (setq mabr/current-theme mabr/dark-theme))))

(mac-auto-operator-composition-mode)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Ave Emacs, morituri te salutant")
;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/dashb/startup.gif")
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        ))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
(setq dashboard-week-agenda t)
  )


(provide 'ui)
