(push (concat user-emacs-directory "conf/app") load-path)

;; DIRED: file browser
(require 'dired)
;; ;; PDF-TOOLS
;; (require 'a-pdf)


;; ORG-MODE
;; hooks
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-capture-mode-hook 'evil-insert-state)

(with-eval-after-load ' korg (require 'a-orgmode))
;; PEEP-DIRED: preview files in dired
(use-package peep-dired)
;; variables
(setq peep-dired-cleanup-on-disable t
      peep-dired-enable-on-directories t)
;; hooks
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;; DICTCC: query dict.cc without leaving emacs
(use-package dictcc)

(provide 'addons)
