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

;; Remap Open Dashboard
(defun mabr/new-dashboard ()
  "Jump to the dashboard buffer, if doesn't exists create one."
  (interactive)
  (switch-to-buffer dashboard-buffer-name)
  (dashboard-mode)
  (dashboard-insert-startupify-lists)
  (dashboard-refresh-buffer))

(provide 'functions)
