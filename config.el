(with-eval-after-load 'org
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files
        '("~/Dropbox/org/work.org"
          "~/Dropbox/org/personal.org"))
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 4)
          ("~/Dropbox/org/random.org" :maxlevel . 4)))
  (setq org-refile-use-outline-path 'file)
  (setq org-preview-latex-default-process 'dvisvgm)
  (org-babel-do-load-languages 'org-babel-load-languages '((ipython . t)))
  ;;  (setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
  (setq org-image-actual-width 600) ;; needs maybe seperate setting by computer
  (evil-define-key 'normal org-mode-map
    (kbd "DEL") 'nandu-babel-delete)
  )

(with-eval-after-load 'company
	(add-to-list 'company-backends 'company-ob-ipython)
  )

;; apparently locate-library only works after loading of elpy
;; the form `(... ,expr) means expr with comma before will be evaluated
;; before inserting into the list
(with-eval-after-load 'elpy
  (setq yas-snippet-dirs `(,(file-name-as-directory (expand-file-name "snippets" (file-name-directory (symbol-file 'nandu-babel-after-execute-hook))))
                           ,(file-name-as-directory (expand-file-name "snippets" (file-name-directory (locate-library "elpy")))))))

(setq python-shell-interpreter "jupyter-console")

;; don't prompt me to confirm everytime I want to evaluate a block
;; this is a possible security risk if I open an org-mode file from someone else
;; and the code gets exevuted automatically
(setq org-confirm-babel-evaluate nil)
;; (setq org-confirm-shell-link-function nil) ; doesn't seem to apply

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(add-hook 'org-mode-hook 'nandu-set-ob-ipython-directory)

(add-hook 'org-font-lock-set-keywords-hook 'nandu-font-lock-set-keywords-hook)
;; (remove-hook 'org-font-lock-set-keywords-hook 'nandu-font-lock-set-keywords-hook)

(add-hook 'after-init-hook (lambda ()
                             (yas-global-mode t)
                             (global-company-mode t)))
