;; (defvar mabr/git "~/git")
(defvar mabr/emacsconf "~/.emacs.d/conf")
;; (defvar mabr/dotfiles "~/")
(defvar mabr/orgfiles  "~/Documents/Dev/OrgFiles")


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; all rights belong to my beloved friend phga <3 du würschtal
;(defun mabr/keymap-company ()
;  (interactive)
;  (general-def
;	  :keymaps 'company-active-map
;	  "<tab>" 'company-complete-selection
;    "<return>" nil
;    "ret" nil))

;; EMACS-WIDE BINDINGS
(mabr-leader
  :states '(normal visual emacs)
  :keymaps 'override

  "SPC" '(:ignore t :which-key "Minormode Binds")
  "SPC p" 'fill-paragraph
  "SPC SPC" 'counsel-m-x
   
  "a" 'ace-select-window
  "d" 'dired
  "D" 'dired-other-window
  "f" 'counsel-find-file
  "F" 'phga/find-file-sudo
  "g" 'magit-status
  "h" 'evil-avy-goto-line
  "i" 'counsel-imenu
  "t" '(hydra-text-scale/body :which-key "Scale Text")
  "y" 'counsel-yank-pop

  "8" 'insert-char

  "/" 'swiper
  ";" 'evilnc-comment-or-uncomment-lines
  "." 'save-buffer
 ;")" 'evil-forward-section-end
 ;"(" 'evil-backward-section-begin
  
 ;; window stuff
  "w" '(:ignore t :which-key "Window")
  "w w" 'other-window
  "w v" 'split-window-right
  "w h" 'split-window-below
  "w f" 'toggle-maximize-buffer
  "w L" 'windsize-right
  "w H" 'windsize-left
  "w J" 'windsize-down
  "w K" 'windsize-up

  ;; switch stuff
  "s" '(:ignore t :which-key "Switch")
  "s b" 'switch-to-buffer
  "s n" 'switch-to-next-buffer
  "s p" 'switch-to-prev-buffer
  "s s" 'counsel-switch-buffer
  "s w" 'ace-swap-window

  ;; kill stuff
  "k" '(:ignore t :which-key "Kill")
  "k b" 'kill-buffer
  "k w" 'delete-window
  "k W" 'kill-buffer-and-window
  "k o b" 'kill-some-buffers
  "k o w" 'delete-other-windows
  "k q q" 'kill-emacs
  "k k" 'kill-current-buffer

  ;; edit stuff
  "e" '(:ignore t :which-key "Edit")
  "e c" '((lambda() (interactive) (dired phga/emacsconf)) :which-key "edit emacs config")
  ;; "e d" '((lambda() (interactive) (dired phga/dotfiles)) :which-key "edit dotfiles")
  "e o" '((lambda() (interactive) (dired phga/orgfiles)) :which-key "edit org files")
  ;; "e g" '((lambda() (interactive) (dired phga/git)) :which-key "edit git files")

  "e m" '(gnus :which-key "edit mail")

  "?" '(:ignore t :which-key "Describe")
  "? v" 'describe-variable
  "? f" 'describe-function
  "? c" 'describe-char
  "? m" 'describe-mode
  "? k" 'describe-key
  "? F" 'describe-face
  )

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale Text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finish" :exit t))

(general-def
  :states '(normal insert)
  :keymaps 'company-mode-map
  "M-RET" 'company-search-candidates)


(provide 'keybindings)
