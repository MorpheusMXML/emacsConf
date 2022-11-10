;; Custom set Variables


(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

(defalias 'yes-or-no-p 'y-or-n-p)

 ;; save unsaved buffers before killing frame
(add-hook 'delete-frame-functions 'save-some-buffers)
(add-hook 'kill-emacs-hook 'save-some-buffers)

(provide 'variables)
