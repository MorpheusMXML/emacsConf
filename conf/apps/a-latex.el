(use-package tex
  :straight auctex)

(use-package company-auctex
  :config
  (company-auctex-init))

(add-hook 'TeX-mode-hook 'flyspell-mode)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil) 		;dont ask to save if you want to compile with C-c C-c
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(provide 'a-latex)
