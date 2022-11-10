;;Utilities for Emacs  
;; Utilities

;; S _ Package for String manipulation
(use-package s
  :straight 
  :defer t)
;; Package for Map Functionality
(use-package dash
  :straight t
  :defer t)


(use-package evil-anzu
  :after evil
  :config (global-anzu-mode t))


(provide 'utils)
