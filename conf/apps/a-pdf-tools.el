; This goes into your emacs config file
(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))

;; this only has to be executed for the installation and can be removed/commented afterwards
;; I recommend commenting it out so that it can be found easily when reinstalling
;; (setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
;; (pdf-tools-install)

(provide 'a-pdf-tools)
;;; a-pdf-tools ends here.
