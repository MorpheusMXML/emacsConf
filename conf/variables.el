;; Custom set Variables


(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/"))

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(setq make-backup-files nil
      create-lockfiles nil)

(defalias 'yes-or-no-p 'y-or-n-p)

 ;; save unsaved buffers before killing frame
(add-hook 'delete-frame-functions 'save-some-buffers)
(add-hook 'kill-emacs-hook 'save-some-buffers)

(provide 'variables)
