;;Custom Functions

 (defun mabr/insert-current-timestamp ()
  (interactive)
  (insert (format-time-string " %Y-%m-%dT%H:%M" (current-time))))  

(defun mabr/find-file-sudo ()
  (interactive)
  (let ((file (expand-file-name (buffer-file-name))))
    (if (string-match "^/sudo:" file)
        (user-error "File already opened with sudo")
      (find-file (concat "/sudo::" file)))))

;;; https://gist.github.com/mads-hartmann/3402786
(defun toggle-maximize-buffer ()
  "Maxmize current buffer, when called again, restore previous buffer setup"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun mabr/format-buffer()
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun count-lines-words-region (start end)
  "Print number of lines words and characters in the region."
  (interactive "r")
  (message "Region has %d lines, %d words, %d characters"
 	   (count-lines start end)
           (count-words-region start end)
           (- end start)))

;; Remap Open Dashboard
(defun mabr/new-dashboard ()
  "Jump to the dashboard buffer, if doesn't exists create one."
  (interactive)
  (switch-to-buffer dashboard-buffer-name)
  (dashboard-mode)
  (dashboard-insert-startupify-lists)
  (dashboard-refresh-buffer))

(defun mabr/show-and-copy-buffer-path ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))
(defun mabr/show-buffer-path-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))


;; Prefer Vertical over Horizontal Splits if sensible...
(defcustom split-window-below nil
  "If non-nil, vertical splits produce new windows below."
  :group 'windows
  :type 'boolean)

(defcustom split-window-right nil
  "If non-nil, horizontal splits produce new windows to the right."
  :group 'windows
  :type 'boolean)

(fmakunbound #'split-window-sensibly)

(defun split-window-sensibly
    (&optional window)
  (setq window (or window (selected-window)))
  (or (and (window-splittable-p window t)
           ;; Split window horizontally.
           (split-window window nil (if split-window-right 'right 'left)))
      (and (window-splittable-p window)
           ;; Split window vertically.
           (split-window window nil (if split-window-below 'above 'below)))
      (and (eq window (frame-root-window (window-frame window)))
           (not (window-minibuffer-p window))
           ;; If WINDOW is the only window on its frame and is not the
           ;; minibuffer window, try to split it horizontally disregarding the
           ;; value of `split-width-threshold'.
           (let ((split-width-threshold 0))
             (when (window-splittable-p window t)
               (split-window window nil (if split-window-right
                                            'left
                                          'right)))))))

(setq-default split-height-threshold  4
	      split-width-threshold   160) ; the reasonable limit for horizontal splits

(setq split-window-right 1)

(provide 'functions)
