;; Stolen from f1p and modified
;; REQUIRES: y -S typescript
(require 'l-web)

;; Tide - TS IDE inkl. JS IDE stuff TSServer
(defun mabr/setup-tide-mode ()
  "Setting Up Tide Function."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq tide-tsserver-executable "/opt/homebrew/bin/tsc"))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
  (add-hook 'typescript-mode-hook #'mabr/setup-tide-mode)

(use-package tide
  :straight t
  :after (company flycheck)
  :hook ((typescript-mode . prettier-mode)
	 (js-mode . prettier-mode))
)

;; PRETTIER JS Formatting
(use-package prettier
  :straight t
  :hook ((typescript-mode . prettier-mode)
	 (js-mode . prettier-mode))
  )


(defun fp/tide-mode-hook ()
  (setq-local flycheck-check-syntax-automatically
              '(save idle-change new-line mode-enabled))

  (defun fp/tide-jump-to-definition ()
    (interactive)
    (evil-set-jump)
    (tide-jump-to-definition))

  (company-mode 1)
  (flycheck-mode 1)
  (setq-local company-tooltip-align-annotations t))

(add-hook 'tide-mode-hook 'fp/tide-mode-hook)

(general-def
  :states 'normal
  :keymaps 'tide-mode-map
  "gd" 'fp/tide-jump-to-definition
  "gr" 'tide-references
  "SPC SPC r" 'tide-rename-symbol)

(general-def
  :states 'normal
  :keymaps 'tide-references-mode-map
  "q" 'quit-window
  "RET" 'tide-goto-line-reference
  "a" (lambda () (interactive)
        (tide-goto-line-reference)
        (pulse-momentary-highlight-one-line (point))
        (select-window (previous-window))))

(flycheck-add-next-checker 'javascript-eslint 'typescript-tide 'append)

;; ----------------------------------------------------------------------
;; web-tide-mode
;; ----------------------------------------------------------------------

(evil-define-operator evil-tide-format (beg end)
  "Format text with tide. See `evil-indent' for reference."
  :move-point nil
  :type line
  ;; these two movements mimic the behaviour of `evil-indent`. not sure if they
  ;; are useful, but consistency is always nice
  (goto-char beg)
  (evil-first-non-blank)
  (tide-format-region beg end))

(define-derived-mode web-tide-mode web-mode "WebTide"
  "Web mode with tide"
  (tide-setup))

(flycheck-add-mode 'javascript-tide 'web-tide-mode)

(general-def
  :states '(normal visual)
  :keymaps 'web-tide-mode-map
  "=" 'prettier-js)

;; This has to be set again here despite already being configured
;; because tide requires typescript-mode, which inserts itself
;; at the top of auto-mode alist.
(add-to-list 'auto-mode-alist '("\\.\\(tsx?\\)\\|\\(jsx?\\)\\|\\(js?\\)\\'" . web-tide-mode))

;; PRETTIER: Formats js code
;; Requires: prettier
(add-hook 'web-tide-mode-hook 'prettier-js-mode)

;; If we want to use the prettier in node modules (pkg: add-node-modules-path)
;; --no-semi, --use-tabs, --no-bracket-spacing: Not required bc default
;; (setq prettier-js-args
;;       '("--print-width=90"
;;         "--tab-width=2"
;;         "--single-quote"
;;         "--trailing-comma=es5"
;;         "--parser=babel" ;; used to be babylon (should be the standard anyway)
;;         "--jsx-single-quote"
;;         ))

(provide 'l-typescript)
