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
  )

;; https://github.com/gregsexton/ob-ipython
;; https://vxlabs.com/tag/ob-ipython/

;; this is because ob-ipython uses python-shell-interpreter in order to
;; find the "python" command, whereas for elpy I need to set it to "jupyter"
;; ob-ipython currently uses this only in this particular function
;; one might have to change it if that changes
(with-eval-after-load 'ob-ipython
	(defun ob-ipython--get-python ()
      "python3"
      ))

(with-eval-after-load 'company
  (global-company-mode t)
	(add-to-list 'company-backends 'company-ob-ipython)
  )

;; apparently locate-library only works after loading of elpy
;; the form `(... ,expr) means expr with comma before will be evaluated
;; before inserting into the list
(with-eval-after-load 'elpy
  (setq yas-snippet-dirs `("~/Dropbox/spacemacs/snippets"
                           ,(concat (file-name-directory (locate-library "elpy")) "snippets"))))

(with-eval-after-load 'yasnippet
  (yas-global-mode 1))

(setq python-shell-interpreter "jupyter-console")

;; don't prompt me to confirm everytime I want to evaluate a block
(setq org-confirm-babel-evaluate nil)

;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; emacs isn't really executing any of the shell init files
;; I need this for some of my python code currently
(setenv "CEZANNE_CONFIG" (shell-command-to-string "echo $CEZANNE_CONFIG"))
