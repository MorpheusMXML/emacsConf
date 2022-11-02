
;; Org Mode Configuration ------------------------------------------------------

;; There was a long discussion on reddit that this package slows
;; down the redisplay optimizations of emacs
;; https://old.reddit.com/r/emacs/comments/p9yjgq/turn_off_fontlock_under_cursor/ha6zor6/
;; ORG-APPEAR: shows emphasis symbols when required
(use-package org-appear)
(setq org-appear-autolinks t
      org-appear-autosubmarkers t
      org-appear-autoentities t
      org-appear-autokeywords t
      org-appear-inside-latex t)
(add-hook 'org-mode-hook 'org-appear-mode)

;; ORG-DOWNLOAD: insert screenshots on the fly
(use-package org-download)
(require 'org-download)

(defun mabr/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; EVIL-ORG: more org bindings
(use-package evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(with-eval-after-load 'org-mode
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar)))

(defun mabr/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun mabr/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mabr/org-mode-visual-fill))
(provide 'a-orgmode)
