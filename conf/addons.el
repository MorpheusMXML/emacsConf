(push (concat user-emacs-directory "conf/apps") load-path)

;; DIRED: file browser
(require 'a-dired)
;; MVTN: Easy Notes in ORGmode
(require 'a-mvtn)
(require 'a-shell)
;; ;; PDF-TOOLS
;; (require 'a-pdf)
;; ORGMODE 

;; ORG-MODE
;; hooks
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; (require 'a-orgmode)
(with-eval-after-load 'org (require 'a-orgmode))
;; PEEP-DIRED: preview files in dired
(use-package peep-dired)
;; variables
(setq peep-dired-cleanup-on-disable t
      peep-dired-enable-on-directories t)
;; hooks
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;; DICTCC: query dict.cc without leaving emacs
(use-package dictcc)

;; IBUFFER: UI to clean up buffers
;; variables
(setq ibuffer-saved-filter-groups (quote (("mabr"
                                           ("Dired" (mode . dired-mode))
                                           ("ORG" (mode . org-mode))
                                           ("LaTeX" (or (file-extension . "bib")
                                                        (file-extension . "tex")))
                                           ("Programming" (or (mode . c++-mode)
                                                              (mode . c-mode)
                                                              (mode . python-mode)
                                                              (mode . web-mode)
                                                              (mode . java-mode)
                                                              (mode . go-mode)
                                                              (mode . js2-mode)
                                                              (mode . emacs-lisp-mode)
                                                              (mode . sh-mode)))))))

;; hooks
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "mabr")))

(provide 'addons)
