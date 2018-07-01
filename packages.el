(defconst nandu-packages
  '(
    (python :excluded t)
    zotxt
    sphinx-doc
    ob-ipython
    )
  )

(defun nandu/init-zotxt ()
  (use-package zotxt :defer t)
  )

(defun nandu/init-sphinx-doc()
  (use-package sphinx-doc :defer t)
  )

(defun nandu/init-ob-ipython()
  (use-package ob-ipython :defer t)
  )
