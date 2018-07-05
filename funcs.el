(defun switch-pyvenv (env)
  (interactive "spyvenv: ")
  (let ((lenv (split-string env)))
    (if (equal (car lenv) "conda")
        (let ((env (pop (cdr lenv))))
          (pyvenv-activate (expand-file-name env conda-envs-directory))
          (setq-local python-shell-interpreter-args
                      (concat "--simple-prompt --kernel " env))
          )
      (progn
        (pyvenv-activate (expand-file-name env non-conda-envs-directory))
        (setq-local python-shell-interpreter-args
                    (concat "--kernel " env))
        )))
  )

;; https://github.com/gregsexton/ob-ipython
;; https://vxlabs.com/tag/ob-ipython/

(with-eval-after-load 'ob-ipython
  ;; this is because ob-ipython uses python-shell-interpreter in order to
  ;; find the "python" command, whereas for elpy I need to set it to "jupyter"
  ;; ob-ipython currently uses this only in this particular function
  ;; one might have to change it if that changes
	(defun ob-ipython--get-python () "python3")

  (defun nandu-set-ob-ipython-directory ()
    (let ((encl (file-name-directory (buffer-file-name)))
          (res_dir (car (get 'ob-ipython-resources-dir 'standard-value))))
      (setq-local ob-ipython-resources-dir
                  (file-name-as-directory
                   (expand-file-name
                    (file-name-base (buffer-file-name)) (concat encl res_dir))))))

  ;; this overrides the rendering of output in ob-ipython,
  ;; in particular to allow both png and pdf files to be saved,
  ;; but only the png to be included in the org file
  (defun ob-ipython--render (file-or-nil values)
    (message "NANDUPY")
    (let* ((res_dir (buffer-local-value 'ob-ipython-resources-dir (current-buffer)))
          (file-base (or file-or-nil (make-temp-name res_dir)))
          (org (lambda (value) value))
          (pdf (lambda (value)
                 (let ((file (s-concat file-base ".pdf")))
                   (ob-ipython--write-base64-string file value))))
          (png (lambda (value)
                 (let ((file (s-concat file-base ".png")))
                   (ob-ipython--write-base64-string file value)
                   (format "[[file:%s]]" file))))
          (svg (lambda (value)
                 (let ((file (s-concat file-base ".svg")))
                   (ob-ipython--write-string-to-file file value)
                   (format "[[file:%s]]" file))))
          (txt (lambda (value)
                 (let ((lines (s-lines value)))
                   (if (cdr lines)
                       (->> lines
                            (-map 's-trim)
                            (s-join "\n  ")
                            (s-concat "  ")
                            (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"))
                     (s-concat ": " (car lines)))))))
        (-when-let (val (cdr (assoc 'application/pdf values))) (funcall pdf val))
        (or (-when-let (val (cdr (assoc 'text/org values))) (funcall org val))
            (-when-let (val (cdr (assoc 'image/png values))) (funcall png val))
            (-when-let (val (cdr (assoc 'image/svg+xml values))) (funcall svg val))
            (-when-let (val (cdr (assoc 'text/plain values))) (funcall txt val)))))
    )

(defun nandu--ob-ipython-shift-return (&optional yas)
  (when (or nandu-shift-return yas)
    (let* ((pos (point-at-bol))
           (el1 (org-element-at-point))
           (el2 nil)
           (src (car el1))
           (pl (pop (cdr el1)))
           (blank (plist-get pl :post-blank)))
      (cond ((string= src "src-block")
             (setq pos (plist-get pl :end))
             (setq blank (- (count-lines 1 pos) blank (count-lines 1 (point-at-bol))))
             (save-excursion
               (goto-char pos)
               (setq el2 (org-element-at-point))
               (when (string= (car el2) "drawer")
                 (setq pos (plist-get (pop (cdr el2)) :end))))))
      (cond ((and
              (not (string= (car el2) "drawer"))
              (string= src "src-block")
              (> blank 0) ;; not beyond the #+end_src
              (string-match-p "[:alnum:]" (plist-get pl :value))) ;; there's code in the src_block
             (setq-local nandu-shift-return t)
             (org-babel-execute-src-block))
            (t
             (goto-char pos)
             (setq-local nandu-shift-return nil)
             (yas-expand-snippet (yas-lookup-snippet "ob-ipython source block")))))))


(defun nandu-org-after-execute-hook ()
  (org-display-inline-images t t)
  (nandu--ob-ipython-shift-return))
