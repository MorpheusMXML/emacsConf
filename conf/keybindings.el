;; (defvar mabr/git "~/git")
(defvar mabr/emacsconf "~/.emacs.d/conf")
;; (defvar mabr/dotfiles "~/")
(defvar mabr/orgfiles  "~/org")


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; all rights belong to my beloved friend mabr <3 du würschtal
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
  "SPC SPC" 'counsel-M-x
  
  "a" 'ace-select-window
  ;; "d" 'dired
  ;; "D" 'dired-other-window
  ;; "f" 'counsel-find-file
  ;; "F" 'mabr/find-file-sudo
  "g" 'magit-status
  "T" 'treemacs-select-window
  "j" 'evil-avy-goto-line
  "y" 'counsel-yank-pop
  "h" 'mabr/new-dashboard
  "r" 'ranger
  
  "8" 'insert-char

  "/" 'swiper
  ";" 'evilnc-comment-or-uncomment-lines
  "." 'save-buffer
					;")" 'evil-forward-section-end
					;"(" 'evil-backward-section-begin

  ;; Filte Stuff
  "f"     '(:ignore t :which-key "File Stuff")
  "f f"   '(find-file :which-key "Find file")
  "f r"   '(counsel-recentf :which-key "Recent files")
  "f s"   '(save-buffer :which-key "Save file")
  "f u"   '(mabr/find-file-sudo :which-key "Sudo find file")
  "f y"   '(mabr/show-and-copy-buffer-path :which-key "Yank file path")
  "f C"   '(copy-file :which-key "Copy file")
  "f D"   '(delete-file :which-key "Delete file")
  "f R"   '(rename-file :which-key "Rename file")
  
  ;; window stuff
  "w" '(:ignore t :which-key "Window")
  "w w" 'other-window
  ;; "w v" 'split-window-right
  ;; "w h" 'split-window-below
  "w m" 'toggle-maximize-buffer
  "w b" 'balance-windows
  "w L" 'windsize-right
  "w H" 'windsize-left
  "w J" 'windsize-down
  "w K" 'windsize-up
  ;; "w f" 'make-frame-on-monitor

  "w s" '(:ignore t :which-key "Split")
  "w s s" 'split-window-below
  "w s d" 'split-window-right
  "w s f" 'make-frame-on-monitor

  
  ;; Dired stuff
  "d" '(:ignore t :which-key "Dired")
  "d d" 'dired
  "d j" 'dired-jump
  "d f" 'dired-other-frame
  "d w" 'dired-other-window
  "d s" 'find-dired

  ;; switch stuff
  "s" '(:ignore t :which-key "Switch")
  "s b" 'switch-to-buffer
  "s n" 'switch-to-next-buffer
  "s p" 'switch-to-prev-buffer
  "s s" 'counsel-switch-buffer
  "s w" 'ace-swap-window
  "s f" 'other-frame

  ;; kill stuff
  "k" '(:ignore t :which-key "Kill")
  "k t" 'treemacs
  "k f" 'delete-frame
  "k b" 'kill-buffer
  "k w" 'delete-window
  "k W" 'kill-buffer-and-window
  "k o b" 'kill-some-buffers
  "k o w" 'delete-other-windows
  "k q q" 'kill-emacs
  "k k" 'kill-current-buffer

  ;; buffer stuff
  "b" '(:ignore t :which-key "Buffer (revert)")
  "b r" 'revert-buffer
  "b a r" 'auto-revert-mode

  ;; edit stuff
  "e" '(:ignore t :which-key "Edit")
  "e c" '((lambda() (interactive) (dired mabr/emacsconf)) :which-key "edit emacs config")
  "e o" '((lambda() (interactive) (dired mabr/orgfiles)) :which-key "edit org files")

  "m" '(:ignore t :which-key "Mail")
  "m m" 'mu4e
  "m c" 'mu4e-compose-new
  "m i" 'mabr/go-to-inbox 
  "m u" 'mu4e-update-mail-and-index

  "q" '(:ignore t :which-key "Quality :)")
  "q t" 'mabr/cycle-themes
  "q s" '(hydra-text-scale/body :which-key "Scale Text")

  "t" '(:ignore t :which-key "Tools")
  "t a" 'org-agenda
  "t d" 'dictcc
  "t c" 'calc
  "t m" 'man
  "t k" 'calendar
  "t b" 'ibuffer
  "t r" '((lambda () (interactive) (counsel-rg nil default-directory))
          :which-key "Ripgrep in current folder")
  "t R" '((lambda (regex) (interactive "MFile Search: ")
            (shell-command (format "find * -type f -name '%s'" regex)))
          :which-key "Search for files in current folder")
  "t t" '(mabr/run-terminal-here :which-key "Alacritty in current dir")
  "t s" 'mabr/new-shell
  "t v" 'mabr/new-vterm
  "t u" 'vundo
  "t c" 'org-capture
  "t i" '(counsel-imenu :which-key "iMenu") 

  "i" '(:ignore t :which-key "Insert")
  "i t" '(mabr/insert-current-timestamp :which-key "Insert Timestamp")
  "i p" '(academic-phrases :which-key "Lookup Academic Phrases")
  "i d" '(powerthesaurus-lookup-definitions-dwim :which-key "Definitions")
  "i s" '(powerthesaurus-lookup-synonyms-dwim :which-key "Synonyms")
  "i a" '(powerthesaurus-lookup-dwim :which-key "Alternative Word")
  
  "c" '(:ignore t :which-key "Modes")
  "c s" 'flyspell-mode
  
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
