;; NEW BASH COMPLETION
;; Make sure to add:
;; export HISTCONTROL=ignoreboth
;; to the .bashrc that is sourced by shell-mode
;; TODO: both basically do the same but native-complete does not show aliases

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

(use-package native-complete)
(use-package bash-completion)
(with-eval-after-load 'shell
  (native-complete-setup-bash)
  (bash-completion-setup))

;; SHX: Advanced shell
(use-package shx)
(setq shx-max-output 1024
      shx-img-height 250
      shx-show-hints nil)
(shx-global-mode 1)

(defun shx-cmd-unkeep (_arg)
  "(SAFE) Remove a kept command from `shx-kept-commands'."
  (let ((unkeep (completing-read
                 "What to unkeep: "
                 (mapcar (lambda (el)
                           (car el)) shx-kept-commands))))
    (setq shx-kept-commands
          (delq (assoc unkeep shx-kept-commands) shx-kept-commands))
    (shx-insert "Removed " unkeep "from shx-kept-commands\n")))



(add-hook 'shell-mode-hook
  (lambda ()
    (define-key shell-mode-map (kbd "<M-up>") 'comint-previous-input)
    (define-key shell-mode-map (kbd "<M-down>") 'comint-next-input)
  )
  )


(add-hook 'term-mode-hook
  (lambda ()
    (define-key term-mode-map (kbd "<M-up>") 'comint-previous-input)
    (define-key term-mode-map (kbd "<M-down>") 'comint-next-input)
  )
  )

(provide 'a-shell)
