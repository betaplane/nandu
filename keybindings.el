;; http://spacemacs.org/doc/DOCUMENTATION.html#binding-keys

(spacemacs/declare-prefix "o" "my own (nandu) menu")
(spacemacs/declare-prefix "oo" "org files")

;; this actually gives the functions names that appear in the ivy-interface
(defmacro nandu-open-keys (key fname)
  (let ((f (intern (concat "nandu-open:" (file-name-nondirectory fname)))))
    `(progn
      (defun ,f ()
        (interactive)
        (find-file ,fname))
      (spacemacs/set-leader-keys ,key ',f))))

(nandu-open-keys "oop" "~/Dropbox/org/personal.org")
(nandu-open-keys "oow" "~/Dropbox/org/work.org")
(nandu-open-keys "oor" "~/Dropbox/org/random.org")
