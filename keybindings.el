;; http://spacemacs.org/doc/DOCUMENTATION.html#binding-keys

(spacemacs/declare-prefix "o" "my own (nandu) menu")
(spacemacs/declare-prefix "oo" "org files")

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

(nandu-open-keys "ooa" "~/Dropbox/org/")
(nandu-open-keys "oop" "~/Dropbox/org/personal.org")
(nandu-open-keys "oow" "~/Dropbox/org/work.org")
(nandu-open-keys "oor" "~/Dropbox/org/random.org")
(nandu-open-keys "ool" "~/Dropbox/org/linux.org")
(nandu-open-keys "oob" "~/Dropbox/org/refile-beorg.org")

