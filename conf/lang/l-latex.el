(message "Loading LaTeX Settings...")

(use-package tex
  :straight auctex
  :ensure t)
(load "auctex.el" nil t t)


(use-package company-auctex
  :config
  (company-auctex-init)
  :ensure t)

(setq lsp-tex-server 'digestif)


;; Some mode to be hooked
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTex-mode-hook 'lsp)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'drag-stuff-mode)
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

(use-package auctex-latexmk
  :ensure t
  :init
  (auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (add-hook 'TeX-mode-hook #'(lambda () (setq TeX-command-default "LatexMk")))
  (add-hook 'LaTeX-mode-hook
            (lambda () (local-set-key (kbd "<M-s-mouse-1>") #'TeX-view))))

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
      ;; TeX-view-program-selection '((output-pdf "PDF Viewer"))
      ;; TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      ;; use Skim as default pdf viewer
      ;; Skim's displayline is used for forward search (from .tex to .pdf)
      ;; option -b highlights the current line; option -g opens Skim in the background  
      ;; TeX-view-program-list
      ;; '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))

      ;; PDF_Tools as PDF Viewer for Latex compiled Files
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF-tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t
      TeX-source-correlate-method 'synctex
      TeX-source-correlate-mode t
      ;; Choose pdflatex
      TeX-PDF-mode t
      TeX-DVI-via-PDFTeX t
      LaTeX-item-indent 0
      ispell-dictionary "english")

(defun mabr/revert-latex-document-buffer (pdf)
  (let* ((parts (split-string pdf "/"))
         (fname (car (last parts 1)))
         (pl (nbutlast parts 1))
         (dir (string-join pl "/"))
         (file (concat dir "/auto/" fname)))
    (message (concat "Reverting file: " file))
    (TeX-revert-document-buffer file)))

(add-hook 'after-save-hook (lambda ()
			     (when (eq major-mode 'LaTeX-mode)
			       (TeX-command-run-all))))

;; (add-hook 'TeX-after-compilation-finished-functions #'mabr/revert-latex-document-buffer)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; keybindings
(mabr-leader
  :states 'normal
  :keymaps 'LaTeX-mode-map
  "SPC c" '((lambda () (interactive) (save-buffer) (TeX-command "LatexMk" 'TeX-master-file)) :which-key "Compile")
  "SPC b" '((lambda () (interactive) (save-buffer) (TeX-command "Biber" 'TeX-master-file)) :which-key "Biber")
  "SPC v" '(TeX-view :which-key "View PDF")
  "SPC p" 'fill-paragraph
  "=" 'align-current
  )

(provide 'l-latex)
