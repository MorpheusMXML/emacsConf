(use-package vterm)
(defun mabr/new-vterm ()
  (interactive)
  "Creates a new vterm in the current window and renames in with a random name"
  (let* ((curr-window (get-buffer-window (current-buffer)))
         (adjectives '("last" "worthless" "labored" "sordid" "puzzled"
                       "tremendous" "clammy" "present" "outrageous" "raspy"
                       "nimble" "rampant" "sour" "angry" "colorful" "early"
                       "alert" "quixotic" "husky" "shrill" "disturbed" "damaged"
                       "breakable" "painful" "gainful" "delicate" "unruly"
                       "huge" "spooky" "incompetent" "dependent" "long"
                       "skillful" "troubled" "longing" "second-hand" "dazzling"
                       "wary" "resonant" "guarded" "abiding" "crowded" "eatable"
                       "grandiose" "alike" "unsuitable" "shut" "ambitious"))
         (vterm-name (generate-new-buffer-name
                      (concat "*" (nth (random (length adjectives))
                                       adjectives) "-vterm*"))))
    (vterm vterm-name)))

;; Otherwise vterm instantly exits
(setq vterm-tramp-shells '()
      vterm-environment '("TERM_FROM_EMACS=true"))
(add-to-list 'vterm-tramp-shells '("ssh" "/bin/bash"))
(add-to-list 'vterm-tramp-shells '("sudo" "/bin/bash"))

(defun mabr/vterm--update-cursor (fn &rest args)
  (vterm-goto-char (point)))

;; Make append work as expected
(advice-add 'vterm-send-key :before 'mabr/vterm--update-cursor)

(general-def
  :states '(normal insert visual)
  :keymaps 'vterm-mode-map
  "M-p" 'vterm-send-up
  "M-n" 'vterm-send-down
)

(provide 'a-vterm)
