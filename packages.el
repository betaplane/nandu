(defconst nandu-packages
  '(
    (python :excluded t)
    zotxt
    sphinx-doc
    ob-ipython
    ivy-bibtex
    org-ref
    )
  )

(defun nandu/init-zotxt ()
  (use-package zotxt :defer t))

(defun nandu/init-sphinx-doc ()
  (use-package sphinx-doc :defer t))

(defun nandu/init-ob-ipython ()
  (use-package ob-ipython :defer t))

(defun nandu/init-ivy-bibtex ()
  (use-package ivy-bibtex :defer t)
  (setq bibtex-completion-bibliography (directory-files "~/Dropbox/work/" t ".*\.bib"))
  (setq bibtex-completion-pdf-field "File")
  (setq ivy-re-builders-alist '((ivy-bibtex . ivy--regex-ignore-order)
                                (t . ivy--regex-plus))))

(defun nandu/init-org-ref ()
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (use-package org-ref :defer t)
  (setq org-ref-default-bibliography bibtex-completion-bibliography))
