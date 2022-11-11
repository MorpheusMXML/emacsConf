;; DIRED: Filebrowser
(use-package dired-narrow)

(setq dired-listing-switches "-lFahv --group-directories-first"
      dired-dwim-target t
      delete-by-moving-to-trash t
      dired-ls-F-marks-symlinks t)

;; Keybindings
(general-def
  :states 'normal
  :keymaps 'dired-mode-map
  "f" 'dired-narrow
  "p" 'peep-dired
  "E" 'sudired
  "P" (lambda() (interactive) (start-process "SHOW PICS" "*NOMACS*" "nomacs"
                                             (file-name-directory (dired-get-filename))))
  "h" (lambda() (interactive) (find-alternate-file ".."))
  "l" (lambda() (interactive) (dired-find-alternate-file)))

(general-def
  :states 'normal
  :keymaps 'peep-dired-mode-map
  "J" 'peep-dired-scroll-page-down
  "K" 'peep-dired-scroll-page-up
  "j" 'peep-dired-next-file
  "k" 'peep-dired-prev-DIRED)

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