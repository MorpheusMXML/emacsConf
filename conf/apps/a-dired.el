;; DIRED: Filebrowser
(use-package dired-narrow)

(setq dired-listing-switches "-lFahvgG --group-directories-first"
      dired-dwim-target t
      dired-ls-F-marks-symlinks t)


;; PEEP-DIRED: preview files in dired
;; (Use-package peep-dired
;;   :after dired
;;   :config
;;   (setq peep-dired-cleanup-on-disable t)
;;   (setq peep-dired-cleanup-eagerly t)
;;   (setq peep-dired-enable-on-directories t))

;; (use-package nav)

(use-package ranger
  :config
  (setq ranger-show-hidden t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-cleanup-on-disable t))

(use-package osx-trash)
(when (eq system-type 'darwin)
  (osx-trash-setup))
(setq delete-by-moving-to-trash t)


;; (use-package diredc)

;; Keybindings
(general-def
  :states 'normal
  :keymaps 'dired-mode-map
  "f" 'dired-narrow
  "p" 'ranger
  "E" 'sudired
  "d" 'epa-dired-do-decrypt
  "e" 'epa-dired-do-encrypt
  "P" (lambda() (interactive) (start-process "SHOW PICS" "*NOMACS*" "nomacs"
                                             (file-name-directory (dired-get-filename))))
  "h" (lambda() (interactive) (find-alternate-file ".."))
  "l" (lambda() (interactive) (dired-find-alternate-file)))

;; (general-def
;;   :states 'normal
;;   :keymaps 'peep-dired-mode-map
;;   "J" 'peep-dired-scroll-page-down
;;   "K" 'peep-dired-scroll-page-up
;;   "j" 'peep-dired-next-file
;;   "k" 'peep-dired-prev-file)

;; file-LAUNCH: Launch apps depending on file extension
(use-package dired-launch)
(setq dired-launch-command '("xdg-open"))
(setf dired-launch-extensions-map
      '(;; xml files with bpmn in it
        ("bpmn" ("camunda-modeler"))
        ("xopp" ("xournalpp"))))

(general-def
  :states 'normal
  :keymaps 'dired-mode-map
  "W" 'dired-launch-command)

(provide 'a-dired)
