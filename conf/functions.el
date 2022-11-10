;;Custom Functions

 (defun mabr/insert-current-timestamp ()
  (interactive)
  (insert (format-time-string " %Y-%m-%dT%H:%M" (current-time))))  

(provide 'functions)
