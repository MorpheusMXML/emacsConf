;; PYTHON
;; PYLS
;; sudo pip install python-lsp-server[all]
;; (setq lsp-pylsp-plugins-flake8-enabled nil
;;       lsp-pylsp-plugins-pylint-enabled nil
;;       lsp-pylsp-plugins-pyflakes-enabled t
;;       lsp-pylsp-plugins-autopep8-enabled nil
;;       lsp-pylsp-plugins-pydocstyle-enabled nil)

;; (add-hook 'python-mode-hook 'lsp)

;; PYRIGHT
(setq python-indent-guess-indent-offset-verbose nil
      lsp-pyright-auto-import-completions t
      lsp-pyright-typechecking-mode "on"
      python-shell-completion-native-enable nil)
;; sudo npm install -g pyright
;; y -S pyright
(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))
(setq org-babel-python-command "/usr/local/bin/python3")
;; (add-to-list 'org-src-lang-modes '("python" . py))

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (call-interactively #'lsp-workspace-restart)))
  (pyvenv-mode +1))

(provide 'l-python)
