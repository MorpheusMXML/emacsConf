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
(display-time)



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

;; (mac-auto-operator-composition-mode)

(use-package dashboard
  :straight t
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

;; TREEMACS
(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-eldoc-display                   'simple
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-text-scale                      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)))


(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package beacon
;;   :ensure t
;;   :config
;;   (beacon-mode 1)
;;   (setq beacon-blink-delay 2.0)
;;   (setq beacon-blink-duration 1.6))

(use-package pulsar
  :init
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  :config
  (pulsar-global-mode 1))

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; (use-package centaur-tabs
;;   :load-path "~/.emacs.d/other/centaur-tabs"
;;   :config
;;   (setq centaur-tabs-style "bar"
;; 	centaur-tabs-height 32
;; 	centaur-tabs-set-icons t
;; 	centaur-tabs-gray-out-icons 'buffer
;; 	centaur-tabs-set-close-button nil

;; 	centaur-tabs-set-modified-marker t
;; 	centaur-tabs-show-navigation-buttons t
;; 	centaur-tabs-set-bar 'under
;; 	x-underline-at-descent-line t)
;;   (centaur-tabs-headline-match)
;;   ;; (setq centaur-tabs-gray-out-icons 'buffer)
;;   ;; (centaur-tabs-enable-buffer-reordering)
;;   ;; (setq centaur-tabs-adjust-buffer-order t)
;;   (centaur-tabs-mode t)
;;   (setq uniquify-separator "/")
;;   (setq uniquify-buffer-name-style 'forward)
;;   (defun centaur-tabs-buffer-groups ()
;;     "`centaur-tabs-buffer-groups' control buffers' group rules.

;;  Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;;  All buffer name start with * will group to \"Emacs\".
;;  Other buffer group by `centaur-tabs-get-group-name' with project name."
;;     (list
;;      (cond
;;       ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
;;       ;; "Remote")
;;       ((or (string-equal "*" (substring (buffer-name) 0 1))
;; 	   (memq major-mode '(magit-process-mode
;; 			      magit-status-mode
;; 			      magit-diff-mode
;; 			      magit-log-mode
;; 			      magit-file-mode
;; 			      magit-blob-mode
;; 			      magit-blame-mode
;; 			      )))
;;        "Emacs")
;;       ((derived-mode-p 'prog-mode)
;;        "Editing")
;;       ((derived-mode-p 'dired-mode)
;;        "Dired")
;;       ((memq major-mode '(helpful-mode
;; 			  help-mode))
;;        "Help")
;;       ((memq major-mode '(org-mode
;; 			  org-agenda-clockreport-mode
;; 			  org-src-mode
;; 			  org-agenda-mode
;; 			  org-beamer-mode
;; 			  org-indent-mode
;; 			  org-bullets-mode
;; 			  org-cdlatex-mode
;; 			  org-agenda-log-mode
;; 			  diary-mode))
;;        "OrgMode")
;;       (t
;;        (centaur-tabs-get-group-name (current-buffer))))))
;;   :hook
;;   (dashboard-mode . centaur-tabs-local-mode)
;;   (term-mode . centaur-tabs-local-mode)
;;   (calendar-mode . centaur-tabs-local-mode)
;;   (org-agenda-mode . centaur-tabs-local-mode)
;;   (helpful-mode . centaur-tabs-local-mode)
;;   :bind
;;   ;; ("C-<prior>" . centaur-tabs-backward)
;;   ;; ("C-<next>" . centaur-tabs-forward)
;;   ;; ("C-c t s" . centaur-tabs-counsel-switch-group)
;;   ;; ("C-c t p" . centaur-tabs-group-by-projectile-project)
;;   ;; ("C-c t g" . centaur-tabs-group-buffer-groups)
;;   (:map evil-normal-state-map
;; 	("g t" . centaur-tabs-forward)
;; 	("g T" . centaur-tabs-backward)))

(provide 'ui)
