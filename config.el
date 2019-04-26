(defvar nandu-image-background-color nil "If not `nil', set the ImageMagick :background parameter of images in overlays to this color.")
(defvar nandu-post-result-lines 2 "The number of empty lines between then end of a src block or results and a new src block inserted via nandu-shift-return.")
(defvar nandu-layer-directory (file-name-directory (symbol-file 'nandu-shift-return)) "The directory containing all Nandu files.")
(defvar nandu-styles-directory (expand-file-name "stylelib" nandu-layer-directory) "The directory containing style sheets for matplotlib.")

;; there is a bug in org currently with this - it just can't be set
;; https://github.com/syl20bnr/spacemacs/issues/9748
;; (setq org-default-notes-file "~/Dropbox/org/random.org")
(with-eval-after-load 'org
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files (mapcar #'(lambda (f) (expand-file-name f org-directory))
                                 '("work.org" "personal.org" "refile-beorg.org" "random.org")))
  (setq org-refile-targets #'(mapcar (lambda (f) (((expand-file-name f org-directory) :maxlevel . 4)))
        '(org-agenda-files "linux.org" "betaplane.org")))
  (setq org-refile-use-outline-path 'file)
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-startup-with-latex-preview t)
  (org-babel-do-load-languages 'org-babel-load-languages '((ipython . t)))
  ;;  (setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
  (evil-define-key 'normal org-mode-map
    (kbd "DEL") 'nandu-babel-delete)
  ;; (advice-add 'org-display-inline-remove-overlay :before 'nandu-display-image-remove-overlay)
  ;; (advice-add 'ob-ipython--create-process :after 'nandu-create-process)
  ;; (add-hook 'org-after-todo-state-change-hook (lambda () (message "%s" org-state)))
  ;; (font-lock-add-keywords 'org-mode '(nandu-display-func-src))
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-export-use-babel nil) ;; don't execute src blocks when exporting org file
  ;; </org-ref
  ;; https://github.com/jkitchin/org-ref#configuration
  ;; manual: https://github.com/jkitchin/org-ref/blob/master/org-ref.org
  ;; https://www.reddit.com/r/emacs/comments/8wr0e0/alternative_to_orgref/
  (setq org-ref-default-bibliography '("~/Dropbox/work/zotero_all.bib"))
  (setq org-ref-cite-completion-library 'org-ref-ivy-cite)
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -pdf -f -outdir=%o %f"))
  ;; org-ref/>
  (advice-add 'org-babel-execute:ipython :around 'nandu-babel-execute:ipython)
  (advice-add 'ob-ipython--get-python :override 'nandu--get-python)
  (advice-add 'ob-ipython--process-response :override 'nandu--process-response)
  (advice-add 'ob-ipython--render :override 'nandu--render)
  (advice-add 'ob-ipython--create-process :after 'nandu-create-ipython-process)
  )

(with-eval-after-load 'company
	(add-to-list 'company-backends 'company-ob-ipython)
  )

;; for use with elpy
(setq python-shell-interpreter "jupyter-console")
(setq python-shell-interpreter-interactive-arg "")

;; don't prompt me to confirm everytime I want to evaluate a block
;; this is a possible security risk if I open an org-mode file from someone else
;; and the code gets exevuted automatically
(setq org-confirm-babel-evaluate nil)
;; (setq org-confirm-shell-link-function nil) ; doesn't seem to apply

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(add-hook 'org-mode-hook 'nandu-org-mode-hook)
(add-hook 'org-export-before-processing-hook 'nandu-org-export-before-processing-hook)
(add-hook 'org-font-lock-set-keywords-hook 'nandu-font-lock-set-keywords-hook)
(add-hook 'after-init-hook 'nandu-after-init-hook)
