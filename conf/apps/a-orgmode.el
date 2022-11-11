
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
(use-package org-download
  :after org
  :straight t
  :config
  (setq org-download-screenshot-method "screencapture -i %s")
  (setq-default org-download-image-dir "./img")
  (add-hook 'dired-mode-hook 'org-download-enable))
(require 'org-download)


;; (use-package org-bullets
;;   :after org
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("●" "○" "◉" "●" "○" "◉")))


;; EVIL-ORG: more org bindings
(use-package evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(with-eval-after-load 'org-mode
  (evil-org-set-key-theme '(navigation todo insert textobjects additional calendar)))


  ;; Set faces for heading levels
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.1)
  ;;                 (org-level-3 . 1.05)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.1)
  ;;                 (org-level-6 . 1.1)
  ;;                 (org-level-7 . 1.1)
  ;;                 (org-level-8 . 1.1)))
;;     (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))
 

(defun mabr/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mabr/org-mode-visual-fill))

(evil-set-initial-state 'org-agenda-mode 'normal)
;;-------------------------------------------------------------------------------------------------
;; ORG Settings
(setq org-duration-format 'h:mm

      ;; Agenda
      org-agenda-files '("~/org/Agenda/")
      org-agenda-restore-windows-after-quit t
      org-agenda-default-appointment-duration 30
      org-agenda-start-on-weekday nil
      ;; Build Agenda seperation with Week and Global TODOs
      ;; Build High Prio ord Today Section only with Prio A Items in Agenda
      org-agenda-custom-commands
      '(("c" "Get Shit Done View"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Prio Today:")))
          (agenda "")
          (alltodo ""
		   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline)))))))))
      ;;Export
      org-export-with-email t

      ;; Visuals
      org-pretty-entities t
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-ellipsis " ↷"
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-startup-with-latex-preview t
      org-image-actual-width nil

      ;; TODOS
      org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (type "NEXT(n)")
        (type "MEET(m)")
        (type "IDEA(i)"))
            org-todo-keyword-faces
      '(("TODO" :inherit org-todo :weight bold)
        ("DONE" :inherit org-done :weight bold)
        ("NEXT" :foreground "#A2DFED" :weight bold)
        ("MEET" :foreground "#B95F8A":weight bold)
        ("IDEA" :foreground "#FF7F11" :weight bold))
      org-use-fast-todo-selection t
      org-fontify-todo-headline nil
      org-fontify-done-headline nil)

;; ORG Agenda Filter Prio A from Global List Function
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

;; Function to implement Header Jumping in Agenda View
(defun air-org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header))

(defun air-org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (air--org-agenda-goto-header t))

(defun air--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

;; KEYBINDINGS
(general-def
  :states 'normal
  :keymaps 'org-mode-map
  ;; "TAB" 'org-cycle
  "=" 'mabr/format-buffer)

(mabr-leader
  :states 'normal
  :definer 'minor-mode
  :keymaps 'org-src-mode
  "'" 'org-edit-src-exit)

(mabr-leader
  :states '(normal visual)
  :keymaps 'org-mode-map
  "'" 'org-edit-src-code
  "SPC h" 'org-insert-heading
  "SPC p" 'fill-paragraph
  "SPC i" '(:ignore t :which-key "Insert")
  "SPC i s" 'org-download-screenshot
  "SPC i t" 'org-todo
  "SPC i l" 'org-insert-link
  "SPC s l" 'org-store-link
  "SPC t" '(:ignore t :which-key "Toggle")
  "SPC t i" 'org-toggle-inline-images

  "SPC t l" 'org-latex-preview
  "SPC c" '(:ignore t :which-key "Clock, Execute")
  "SPC c i" 'org-clock-in
  "SPC c o" 'org-clock-out
  "SPC c l" 'org-clock-in-last
  "SPC c c" 'org-babel-execute-src-block
  "SPC r" '(:ignore t :which-key "Remove")
  "SPC r r" 'org-babel-remove-result
  "SPC r a" (lambda () (interactive) (org-babel-remove-result-one-or-many t))
  "SPC o" 'org-open-at-point
  "SPC e" 'org-export-dispatch)

(general-def
  :states '(normal visual insert)
  :keymaps 'org-mode-map
  "C-k" 'org-previous-visible-heading
  "C-j" 'org-next-visible-heading)

(general-def
  :states '(normal visual insert)
  :keymaps 'org-agenda-mode-map
  "C-j" 'air-org-agenda-next-header
  "C-k" 'air-org-agenda-previous-header
  "C-t" 'org-agenda-todo)

(provide 'a-orgmode)
