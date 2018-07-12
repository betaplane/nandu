(defun switch-pyvenv (env)
  (interactive "spyvenv: ")
  (let ((lenv (split-string env)))
    (if (equal (car lenv) "conda")
        (let ((env (pop (cdr lenv))))
          (pyvenv-activate (expand-file-name env conda-envs-directory))
          (setq python-shell-interpreter-args
                      (concat "--simple-prompt --kernel " env))
          )
      (progn
        (pyvenv-activate (expand-file-name env non-conda-envs-directory))
        (setq python-shell-interpreter-args
                    (concat "--kernel " env))
        )))
  )

;; https://github.com/gregsexton/ob-ipython
;; https://vxlabs.com/tag/ob-ipython/

;; function overrides
(with-eval-after-load 'ob-ipython
  ;; this is because ob-ipython uses python-shell-interpreter in order to
  ;; find the "python" command, whereas for elpy I need to set it to "jupyter"
  ;; ob-ipython currently uses this only in this particular function
  ;; one might have to change it if that changes
	(defun ob-ipython--get-python () "python3")

  ;; this overrides the rendering of output in ob-ipython,
  ;; in particular to allow both png and pdf files to be saved,
  ;; but only the png to be included in the org file
  (defun ob-ipython--render (file-or-nil values)
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

;; experimental
(defun nandu-run-ob-ipython-startup (&optional ob-ipy-args)
  (let ((args '((:session . nil)))
        (file (expand-file-name "startup.py" (file-name-directory load-file-name))))
    (org-babel-execute:ipython (format "a=%s" file) args)))


;; FUNCTIONS (FOR KEY BINDINGS / SNIPPETS)
;; =======================================

(defun nandu--ob-ipython-shift-return ()
  (let* ((pos (point-at-bol))
         (el (org-element-at-point))
         (blank (org-element-property :post-blank el)))
    (cond ((org-babel-when-in-src-block)
           (setq pos (org-element-property :end el))
           (setq blank (- (count-lines 1 pos) blank (count-lines 1 (point-at-bol))))
           (when (org-babel-where-is-src-block-result)
             (save-excursion
               (goto-char pos)
               (goto-char (org-element-property :contents-end (org-element-at-point)))
               (forward-line (1+ nandu-post-result-lines))
               (setq pos (point-at-bol)))))
          ((string= "drawer" (org-element-type el))
           (save-excursion
             (goto-char (org-element-property :contents-end el))
             (forward-line (1+ nandu-post-result-lines))
             (setq pos (point-at-bol)))))
    (cond ((and
            (org-babel-when-in-src-block)
            (not (org-babel-where-is-src-block-result))
            (> blank 0) ;; not beyond the #+end_src
            (string-match-p "[[:alnum:]]" (org-element-property :value el)) ;; there's code in the src_block
            )
           (org-babel-execute-src-block))
          (t
           (goto-char pos)
           (yas-expand-snippet (yas-lookup-snippet "ob-ipython source block"))))))

;; obsolete
(defun nandu-babel--delete-result-file ()
  (save-excursion
    (goto-char (org-element-property :end (org-element-at-point)))
    (when-let* ((el (org-element-at-point))
           (begin (org-element-property :contents-begin el))
           (end (org-element-property :contents-end el)))
      (goto-char begin)
      (while (< (point) end)
        (let ((ctxt (org-element-context)))
          (when (string= "file" (org-element-property :type ctxt))
            (let ((file (file-name-sans-extension (org-element-property :path ctxt))))
              (dolist (f (file-expand-wildcards (concat file "*")))
                (delete-file f t)
                (message "File %s deleted [nandu-babel]" f))
              )))
        (forward-line 1)
        ))))

(defun nandu-babel-delete ()
  (interactive)
  (cond ((org-babel-when-in-src-block)
         (when (org-babel-where-is-src-block-result)
           ;; (nandu-babel--delete-result-file)
           (org-babel-remove-result))
         (let ((el (org-element-at-point)))
           (delete-region (org-element-property :begin el) (org-element-property :end el)))
        (goto-char (org-babel-previous-src-block)))
        ((string= "drawer" (org-element-type (org-element-at-point)))
         (goto-char (org-babel-previous-src-block))
         ;; (nandu-babel--delete-result-file)
         (org-babel-remove-result)
         )))


;; FONT LOCK MODE
;; ==============

;; This is how font-lock-mode appears to work with functions
(defun nandu-font-lock-caption (limit)
  (let ((case-fold-search t))
    (while (re-search-forward "^[ \t]*#\\+caption:\\([^\n]*\\)$" limit t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        ;; (remove-text-properties beg end '(font-lock-fontified t))
        ;; (add-text-properties beg end '(face (:background "Blue1")))
        (save-excursion
          (goto-char beg)
          (org-do-emphasis-faces end)) ;; this just runs the emphasis function again
        ))))

;; adds a :background color attribute to inlined images and an overlay extending
;; over the whole line with the same background color as the image
;; works as a font-lock-keyword, but gets only added if nandu-image-background-color is not nil
(defun nandu-image-overlay (limit)
  (while (< (point) limit)
    (let ((imov (get-char-property-and-overlay (point) 'org-image-overlay))
          (nanov (get-char-property-and-overlay (point) 'nandu-image-overlay)))
      (when (car imov)
        (let* ((ov (cdr imov))
               (im (overlay-get ov 'display)))
          (setf (image-property im :background) nandu-image-background-color))
        (when (not (car nanov))
          (let* ((ov (cdr imov))
                 (nanov (make-overlay (overlay-start ov)
                                 (save-excursion
                                   (goto-char (overlay-end ov))
                                   (forward-line 1)
                                   (point)))))
          (overlay-put nanov 'evaporate t)
          (overlay-put nanov 'nandu-image-overlay t)
          (overlay-put nanov 'face `(:background ,nandu-image-background-color))))))
    (forward-line 1)))


;; ADVICE
;; ======

;; this function gets advised :befor org-display-image-remove-overlay, which is called by
;; modification-hooks for the image overlay
;; it causes image files to be deleted if the image overlay is removed
(defun nandu-display-image-remove-overlay (ov after _beg _end &optional _len)
  (let ((inhibit-modification-hooks t))
    (when (and ov after)
      (let* ((im (overlay-get ov 'display))
             (file (file-name-sans-extension (image-property im :file))))
        (dolist (f (file-expand-wildcards (concat file "*")))
          (delete-file f t)
          (message "File %s deleted [nandu-babel]" f)))
        )))


;; HOOKS
;; =====

(defun nandu-babel-after-execute-hook ()
  (remove-hook 'org-babel-after-execute-hook 'nandu-babel-after-execute-hook)
  (nandu--ob-ipython-shift-return))

(defun nandu-org-mode-hook ()
  (let ((encl (file-name-directory (buffer-file-name)))
        (res_dir (car (get 'ob-ipython-resources-dir 'standard-value)))
        (nandu_dir (file-name-directory (symbol-file 'nandu-org-mode-hook))))
    (setq-local ob-ipython-resources-dir
                (file-name-as-directory
                 (expand-file-name
                  (file-name-base (buffer-file-name)) (concat encl res_dir))))
  (setenv "PYTHONSTARTUP" (expand-file-name "startup.py" nandu_dir))))

;; I don't manage to make it work when I prepend (add-to-list) the 'keyword' function
(defun nandu-font-lock-set-keywords-hook ()
  ;; (setq-local org-font-lock-extra-keywords (append org-font-lock-extra-keywords '(("avail" 0 '(:background "Blue1") t))))
  ;; (setq-local org-font-lock-extra-keywords (remove '(org-fontify-meta-lines-and-blocks) org-font-lock-extra-keywords))
  (setq org-font-lock-extra-keywords (nconc org-font-lock-extra-keywords '((nandu-font-lock-caption))))
  (when nandu-image-background-color
    (setq org-font-lock-extra-keywords (nconc org-font-lock-extra-keywords '((nandu-image-overlay)))))
  ;; (add-to-list 'org-font-lock-extra-keywords '(nandu-test-font))
  )

(defun nandu-after-init-hook ()
  (yas-global-mode t)
  (global-company-mode t))
