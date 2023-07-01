(setq gc-cons-threshold most-positive-fixnum)
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

;;Install Use-Package
(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))


;; Using garbage magic hack.
;; (use-package gcmh
;;   :config
;;   (gcmh-mode 1))
;; ;; Setting garbage collection threshold
;; (setq gc-cons-threshold 402653184
;;       gc-cons-percentage 0.6)


;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(push (concat user-emacs-directory "conf") load-path)

;; (debug-watch 'TeX-command-extra-options)
(require 'corepkg)
(message "Done loading: corepkg.el")
;; (require 'essentials)
;; (message "Done loading: essentials.el")
(require 'keybindings)
(message "Done loading: keybindings.el")
(require 'programming)
(message "Done loading: programming.el")
(require 'addons)
(message "Done loading: addons.el")
(require 'ui)
(message "Done loading: ui.el")
(require 'utils)
(message "Done loading: utils.el")
(require 'functions)
(message "Done loading: functions.el")
(require 'variables)
(message "Done loading: variables.el")

;; start emacs-server if not running
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq large-file-warning-threshold 50000000)
;; Code to replace exec-path-from-shell
;; Need to create file in $HOME/.emacs.d/.local/env
;; use this command to create the file  `printenv > $HOME/.emacs.d/.local/env'
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
;;; Code to replace exec-path-from-shell

;; (use-package exec-path-from-shell
;;   :straight t
;;   :init
;;   (exec-path-from-shell-initialize))

(setq user-full-name "Maximilian Brosius")
(setq frame-title-format '("Allmighty Editor " emacs-version))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq initial-scratch-message "")
(setq visible-bell nil)
(setq epa-pinentry-mode 'loopback)
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq mac-pass-command-to-system nil)

;; set gc values back to smth reasonable
(setq gc-cons-threshold 104857600) ;; 100 MegaByte (LSP)
