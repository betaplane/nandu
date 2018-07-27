;; http://spacemacs.org/doc/DOCUMENTATION.html#binding-keys

(spacemacs/declare-prefix "o" "my own (nandu) menu")
(spacemacs/declare-prefix "of" "org files")
(spacemacs/declare-prefix "oz" "org-zotxt")
(spacemacs/declare-prefix "os" "sphinx-doc")

;; this actually gives the functions names that appear in the ivy-interface
(defmacro nandu-open-keys (key fname)
  (let ((f (or (and (file-directory-p fname) (intern "nandu-open:org-directory"))
               (intern (concat "nandu-open:" (file-name-nondirectory fname))))))
    `(progn
      (defun ,f ()
        (interactive)
        (if (file-directory-p ,fname)
            (dired ,fname)
          (find-file ,fname)))
      (spacemacs/set-leader-keys ,key ',f))))

(nandu-open-keys "ofa" "~/Dropbox/org/")
(nandu-open-keys "ofp" "~/Dropbox/org/personal.org")
(nandu-open-keys "ofw" "~/Dropbox/org/work.org")
(nandu-open-keys "ofr" "~/Dropbox/org/random.org")
(nandu-open-keys "ofl" "~/Dropbox/org/linux.org")
(nandu-open-keys "ofb" "~/Dropbox/org/refile-beorg.org")

(defun nandu-insert-zotero ()
  (interactive)
  (when (not org-zotxt-mode) (org-zotxt-mode t))
  (call-interactively 'org-zotxt-insert-reference-link))

(spacemacs/set-leader-keys "ozi" 'nandu-insert-zotero)

(defun nandu-sphinx-doc ()
  (interactive)
  (when (not sphinx-doc-mode) (sphinx-doc-mode t))
  (call-interactively 'sphinx-doc))

(spacemacs/set-leader-keys "osi" 'nandu-insert-zotero)
