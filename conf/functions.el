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

(provide 'functions)
