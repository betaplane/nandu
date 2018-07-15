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

;; experimental, see nandu-create-process
(defun nandu-run-ipython-startup ()
  (interactive)
  (let* ((args '((:session . nil)))
        (nandu_dir (file-name-directory (symbol-file 'nandu-run-ipython-startup)))
        (cmd (format "import sys; sys.path.insert(0, '%s'); import nandu; sys.path.pop(0); nandu.get_file_name = lambda: nandufile; nandu.resources_dir = '%s'" nandu_dir ob-ipython-resources-dir)))
    (org-babel-execute:ipython cmd args))
  (add-hook 'org-ctrl-c-ctrl-c-hook 'nandu-ctrl-c-ctrl-c-hook))


;; FUNCTIONS (FOR KEY BINDINGS / SNIPPETS)
;; =======================================

(defun nandu--ob-ipython-shift-return (&optional ctrl-c)
  (catch :ctrl
    (let* ((pos (point-at-bol))
           (el (org-element-at-point))
           (blank (org-element-property :post-blank el)))
      (when (string= "src-block" (org-element-type el))
        (goto-char (org-element-property :end el))
        (setq blank (- (count-lines 1 (point-at-bol)) blank (count-lines 1 pos))))
      (let ((el (org-element-at-point))
            (case-fold-search t))
        (cond ((progn
                 (end-of-line)
                 (re-search-backward "^[ \t]*#\\+results:" (org-element-property :begin el) t))
               (goto-char (org-element-property :end el))
               (let* ((blank (org-element-property :post-blank el))
                      (fat (- nandu-post-result-lines blank))
                 (forward-line (min 0 fat))
                 (newline (max 0 fat))))
              ((and ctrl-c (> blank 0))
               (goto-char pos)
               (org-ctrl-c-ctrl-c)
               (nandu--ob-ipython-shift-return)
               (throw :ctrl t)))))
    (yas-expand-snippet (yas-lookup-snippet "ob-ipython source block"))))


(defun nandu-ctrl-c-ctrl-c-hook ()
  (message "%s" (org-element-context))
  )


(defun nandu-babel-delete ()
  (interactive)
  (let ((el (org-element-at-point)))
    (cond ((string= "src-block" (org-element-type el))
           (if (org-babel-where-is-src-block-result)
               (org-babel-remove-result)
             (delete-region (org-element-property :begin el) (org-element-property :end el))
             (goto-char (org-babel-previous-src-block))))
          ((progn
             (end-of-line)
             (re-search-backward "^[ \t]*#\\+results:" (org-element-property :begin el) t))
           (goto-char (org-babel-previous-src-block))
           (org-babel-remove-result)))))

  (defun nandu-babel-after-save ()
    (let ((files (directory-files ob-ipython-resources-dir nil "^[^\\.]")))
      (org-with-wide-buffer
       (org-element-map (org-element-parse-buffer) 'link
         (lambda (el)
           (when (string= 'file (org-element-property :type el))
             (let ((file (file-name-base (org-element-property :path el))))
               (setq files (-remove (lambda (f) (string-match-p file f)) files))
               )))))
      (dolist (f files)
        (delete-file (expand-file-name f ob-ipython-resources-dir) t)
        (message "File %s deleted [nandu-babel-before-save]" (file-name-nondirectory f)))
      ))

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

;; advice-after ob-ipython--create-process
(defun nandu-create-process (name cmd)
  (message "NANDU create-process name %s, cmd %s" name cmd)
  (nandu-run-ipython-startup))


;; HOOKS
;; =====
;; https://orgmode.org/worg/doc.html
;; NOTE: add-hook does not add a hook multiple times and remove-hook does nothing if the hook is not present

(defun nandu-babel-after-execute-hook ()
  (remove-hook 'org-babel-after-execute-hook 'nandu-babel-after-execute-hook)
  (nandu--ob-ipython-shift-return))

;; gets called quite frequently in org-mode (not only at start)
;; therefore the condition-case
(defun nandu-org-mode-hook ()
  (condition-case err
      (let ((encl (file-name-directory (buffer-file-name)))
            (res_dir (car (get 'ob-ipython-resources-dir 'standard-value))))
        (setq-local ob-ipython-resources-dir
                    (file-name-as-directory
                     (expand-file-name
                      (file-name-base (buffer-file-name)) (concat encl res_dir)))))
    (error (message "error [nandu-org-mode-hook]: %s" (error-message-string err))))
  (add-hook 'after-save-hook 'nandu-babel-after-save t t)
  (message "NANDO org mode hook"))

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
  (global-company-mode t)
  (global-visual-line-mode t))
