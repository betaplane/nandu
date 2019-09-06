;; http://spacemacs.org/doc/DOCUMENTATION.html#binding-keys

(spacemacs/declare-prefix "o" "my own (nandu) menu")
(spacemacs/declare-prefix "of" "org files")
(spacemacs/declare-prefix "oz" "org-zotxt")
(spacemacs/declare-prefix "os" "sphinx-doc")

;; this actually gives the functions names that appear in the ivy-interface
(defmacro nandu-open-keys (key fname &optional name)
  (let ((f (or (and name (intern (concat "nandu-open:" name)))
               (intern (concat "nandu-open:" (file-name-nondirectory fname))))))
    `(progn
      (defun ,f ()
        (interactive)
        (if (file-directory-p ,fname)
            (dired ,fname)
          (find-file ,fname)))
      (spacemacs/set-leader-keys ,key ',f))))

(nandu-open-keys "ofa" "~/Dropbox/org/" "org-dir")
(nandu-open-keys "ofp" "~/Dropbox/org/personal.org")
(nandu-open-keys "ofw" "~/Dropbox/org/work.org")
(nandu-open-keys "ofr" "~/Dropbox/org/random.org")
(nandu-open-keys "ofc" "~/Dropbox/org/comp.org")
(nandu-open-keys "ofl" "~/Dropbox/org/climate.org")
(nandu-open-keys "ofb" "~/Dropbox/org/refile-beorg.org")
(nandu-open-keys "ofn" nandu-layer-directory "nandu-dir")
(nandu-open-keys "ofy" (file-name-as-directory (concat nandu-layer-directory "snippets")) "yas-dir")
(if (equal "condor" (system-name))
    (nandu-open-keys "off" "~/Documents/code/python/" "python")
  (nandu-open-keys "off" "~/Documents/cezanne/python/" "python"))

(defun nandu-insert-zotero ()
  (interactive)
  (when (not org-zotxt-mode) (org-zotxt-mode t))
  (call-interactively 'org-zotxt-insert-reference-link))

(spacemacs/set-leader-keys "ozi" 'nandu-insert-zotero)

(defun nandu-sphinx-doc ()
  (interactive)
  (when (not sphinx-doc-mode) (sphinx-doc-mode t))
  (call-interactively 'sphinx-doc))

(spacemacs/set-leader-keys "osi" 'nandu-sphinx-doc)
