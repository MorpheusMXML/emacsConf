(use-package tex
  :straight auctex)

(use-package company-auctex
  :config
  (company-auctex-init))

;;(use-package lsp-latex)
(setq lsp-tex-server 'digestif)
;; Load Auxtex
(load "auctex.el" nil t t)

;; Some mode to be hooked
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTex-math-mode-hook 'lsp)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(with-eval-after-load "bibtex"
  (add-hook 'bibtex-mode-hook 'lsp))
(with-eval-after-load "biber"
  (add-hook 'biber-mode-hook 'lsp))
;; hooking up and setting reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-toc-split-windows-horizontally t
      reftex-toc-split-windows-fraction 0.62)
(setq reftex-cite-format 'natbib
      reftex-sort-bibtex-matches 'reverse-year)

;; for parentheses
(setq LaTeX-electric-left-right-brace 1)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)
;; For emacs to know where is pdflatex
(setenv "PATH"
	(concat
	 "/Library/TeX/Distributions/Programs/texbin" ":" (getenv "PATH")))

;; Make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
;; (add-hook 'LaTeX-mode-hook (lambda ()
;;   (push
;;     '("latexmk" "latexmk -pvc -pdf %s" TeX-run-TeX nil t
;;       :help "Run latexmk on file")
;;     TeX-command-list)))
;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(use-package auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
;; To invoke Skim using shift-command-click
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "<M-s-mouse-1>") #'TeX-view)))

;; Error handling
(setq TeX-display-help nil)

;; Clean things up
(eval-after-load 'latex
  '(setq LaTeX-clean-intermediate-suffixes
     (append LaTeX-clean-intermediate-suffixes (list "\\.fdb_latexmk" "\\.rel" "\\.tex~"))))


(add-hook 'TeX-mode-hook 'flyspell-mode)
(setq-default TeX-master nil)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("org-plain-latex"
		 "\\documentclass{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; variables
(setq TeX-auto-save t
;; Always parse the file upon opening
      TeX-parse-self t
      TeX-save-query nil
      TeX-view-program-selection '((output-pdf "PDF Viewer"))
      ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
      TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))
      TeX-source-correlate-start-server t
      TeX-source-correlate-method 'synctex
      ;; Choose pdflatex
      TeX-PDF-mode t
      TeX-DVI-via-PDFTeX t
      LaTeX-item-indent 0
      ispell-dictionary "german")

(defun mabr/revert-latex-document-buffer (pdf)
  (let* ((parts (split-string pdf "/"))
         (fname (car (last parts 1)))
         (pl (nbutlast parts 1))
         (dir (string-join pl "/"))
         (file (concat dir "/auto/" fname)))
    (message (concat "Reverting file: " file))
    (TeX-revert-document-buffer file)))

(add-hook 'TeX-after-compilation-finished-functions #'mabr/revert-latex-document-buffer)

;; keybindings
(mabr-leader
  :states 'normal
  :keymaps 'LaTeX-mode-map
  "SPC c" (lambda () (interactive) (save-buffer) (TeX-command "LaTeX" 'TeX-master-file))
  "SPC b" (lambda () (interactive) (save-buffer) (TeX-command "Biber" 'TeX-master-file))
  "SPC p" 'fill-paragraph
  "=" 'align-current
  )

(provide 'l-tex)
