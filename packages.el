(defconst nandu-packages
  '(zotxt
    sphinx-doc
    (python :excluded t)
    )
  )

(defun nandu/init-zotxt ()
  (use-package zotxt :defer t)
  )

(defun nandu/init-sphinx-doc()
  (use-package sphinx-doc :defer t)
  )
