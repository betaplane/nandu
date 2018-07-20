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

  ;; removed the extra line with # Out[...], because
  ;; 1) it interferes with the way I try to detect #+RESULTS:
  ;; 2) it seems useless, in particular since it doesn't correspond to an Out variable on the Python side
  (defun ob-ipython--process-response (ret file result-type)
    (let ((result (cdr (assoc :result ret)))
          (output (cdr (assoc :output ret))))
      (if (eq result-type 'output)
          output
        (ob-ipython--output output nil)
        (s-join "\n" (->> (-map (-partial 'ob-ipython--render file)
                                 (list (cdr (assoc :value result))
                                       (cdr (assoc :display result))))
                           (remove-if-not nil))))))

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
        (cmd (format "import sys; sys.path.insert(0, '%s'); import nandu; sys.path.pop(0); nandu.buffer_file_name = lambda: buffer_file_name; nandu.savefig = lambda: savefig; nandu.resources_dir = '%s'" nandu_dir ob-ipython-resources-dir)))
    (org-babel-execute:ipython cmd args))
  (add-hook 'org-ctrl-c-ctrl-c-hook 'nandu-ctrl-c-ctrl-c-hook))


(defun nandu-append-figure ()
  "\
Execute a matplotlib.pyplot.savefig on the current figure in the `ob-ipython-resources-dir' directory and append the link/overlay to the current src block. The file will be named according to the :savefig header param, or randomly if that isn't present.

:savefig should either be given a full filename with extension (e. g. \"image.png\"), or just the extension (e. g. \"png\"), in which case a random name is created also.

The :results header is handed over to `org-babel-insert-result'.

An optional :style header can give the name of a style with which to print the figure to hardcopy."
  (interactive)
  (let* ((o-args '((:session . nil)))
         (info (or (nth 2 (org-babel-get-src-block-info))
                   (org-babel-parse-header-arguments
                    (org-element-property :end-header (org-element-at-point)))))
         (res_dir (expand-file-name (file-name-sans-extension (buffer-name)) ob-ipython-resources-dir))
         (ext "png")
         (path nil)
         (result-params '("replace" "raw")))
    (catch :im_format
      (when-let ((file_name (alist-get :savefig info)))
        (setq ext (split-string file_name "\\."))
        (if (cdr ext)
            (progn
              (setq path (expand-file-name file_name res_dir))
              (throw :im_format t))
          (setq ext (car ext)))) ;; when-let
      (setq path (concat (make-temp-name (file-name-as-directory res_dir)) "." ext))) ;; end catch
    (let ((cmd (format "plt.gcf().savefig('%s'); plt.close()" path)))
      (when-let* ((style (alist-get :style info))
                  (mplstyle (or (car (directory-files nandu-mpl-styles-directory t style))
                                style)))
        (setq cmd (format "with plt.style.context('%s'):\n\t%s" mplstyle cmd)))
      (org-babel-execute:ipython cmd o-args))
    (when-let ((results (alist-get :results info)))
      (setq result-params (cl-union (split-string results) result-params)))
    (org-babel-insert-result (format "[[file:%s]]" path) result-params)
    (org-display-inline-images)))


;; possibly to parse header arguments and hand to python by expanding the source
;; this will be necessary to have a possible change to the resources_dir if multiple different org files share the same kernel
(defun nandu-ctrl-c-ctrl-c-hook ()
  (let ((info (org-babel-get-src-block-info)))
    (when (string= "ipython" (car info))
      (let* ((body (nth 1 info))
             (var (format "buffer_file_name = '%s'\n" (buffer-file-name))))
        (when (not (string-match-p var body))
          (org-babel-update-block-body (concat var body)))))))
;; ob-ipython->org-babel-execute:ipython override and set var in params
;; possibly with org-babel-merge-params


;; FUNCTIONS (FOR KEY BINDINGS / SNIPPETS)
;; =======================================

(defun nandu-shift-return (&optional ctrl-c)
  "\
Supposed behavior: 1) in src block without (:results . \"silent\") in :result-params
                   execute src block, then append new empty one after results
                   2) in src block with (:results . \"silent\") in :result-params
                   only execute
                   3) after src block
                   only insert
                   4) on src block with existing #+RESULTS: or on #+RESULTS: block
                   only insert, after results

Variable of interest: `nandu-post-result-lines' has the number of empty lines to leave after src blocks or results."
  (catch :ctrl
    (let* ((pos (point-at-bol))
           (el (org-element-at-point))
           (blank (org-element-property :post-blank el))
           (fat (- nandu-post-result-lines blank)))
      (when (member (org-element-type el) '(src-block, babel-call))
        (goto-char (org-element-property :end el))
        (setq blank (- (count-lines 1 (point-at-bol)) blank (count-lines 1 pos))))
      (let ((el (org-element-at-point)))
        (cond ((let ((case-fold-search t))
                 (end-of-line)
                 (re-search-backward "^[ \t]*#\\+results:" (org-element-property :begin el) t))
               (goto-char (org-element-property :end el))
               (setq blank (org-element-property :post-blank el))
               (setq fat (- nandu-post-result-lines blank)))
              ((and ctrl-c (> blank 0))
               (goto-char pos)
               (org-ctrl-c-ctrl-c)
               (when (not
                      (member "silent"
                                  (alist-get :result-params (nth 2 (org-babel-get-src-block-info)))))
                 (nandu-shift-return))
               (throw :ctrl t))))
      (forward-line (min 0 fat))
      (newline (max 0 fat)))
    (yas-expand-snippet (yas-lookup-snippet "org-babel source block"))))


(defun nandu-babel-delete ()
  "\
Supposed behavior: 1) on results paragraph
                   remove results
                   2) on src block:
                      a) if results present
                      remove results
                      b) if no results present
                      remove src block"
  (interactive)
  (let ((el (org-element-at-point))
        (elements '(src-block babel-call)))
    (cond ((member (org-element-type el) elements)
           (if (org-babel-where-is-src-block-result)
               (org-babel-remove-result)
             (delete-region (org-element-property :begin el) (org-element-property :end el))
             (goto-char (org-babel-previous-src-block))))
          ((let ((case-fold-search t))
             (end-of-line)
             (re-search-backward "^[ \t]*#\\+results:" (org-element-property :begin el) t))
           (backward-char)
           (let ((el (org-element-at-point)))
            (when (member (org-element-type el) elements)
              (goto-char (org-element-property :begin el))
              (org-babel-remove-result)))))))

(defun nandu-babel-after-save ()
  (let ((res_dir (expand-file-name (file-name-sans-extension (buffer-name)) ob-ipython-resources-dir)))
    (when (file-directory-p res_dir)
      (let ((files (directory-files res_dir nil "^[^\\.]")))
        (org-with-wide-buffer
         (org-element-map (org-element-parse-buffer) 'link
           (lambda (el)
             (when (string= 'file (org-element-property :type el))
               (let ((file (file-name-base (org-element-property :path el))))
                 (setq files (-remove (lambda (f) (string-match-p file f)) files))
                 )))))
        (dolist (f files)
          (delete-file (expand-file-name f res_dir) t)
          (message "File %s deleted [nandu-babel-before-save]" (file-name-nondirectory f)))
    ))))

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

(defun nandu-display-func-src (limit)
  (when (re-search-forward "\\$ff(\\(.*\\))" limit)
    (let* ((func (intern (match-string 1)))
           (el (org-element-context))
           (beg (org-element-property :begin el))
           (end (org-element-property :end el))
           (ovtext nil))
      (save-window-excursion
        (find-function func)
        (set-mark (point))
        (forward-sexp)
        (setq ovtext (buffer-substring (mark) (point)))
        (kill-buffer-if-not-modified))
      (put-text-property beg end 'display (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE" ovtext)))))


;; ADVICE
;; ======

;; this function gets advised :before org-display-image-remove-overlay, which is called by
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

;; currently not used - maybe necessary for async calls...
(defun nandu-babel-after-execute-hook ()
  (remove-hook 'org-babel-after-execute-hook 'nandu-babel-after-execute-hook)
  (nandu-shift-return))

;; gets called quite frequently in org-mode (not only at start)
;; therefore the condition-case
(defun nandu-org-mode-hook ()
  ;; (condition-case err
  ;;     (let ((encl (file-name-directory (buffer-file-name)))
  ;;           (res_dir (car (get 'ob-ipython-resources-dir 'standard-value))))
  ;;       (setq-local ob-ipython-resources-dir
  ;;                   (file-name-as-directory
  ;;                    (expand-file-name
  ;;                     (file-name-base (buffer-file-name)) (concat encl res_dir)))))
  ;;   (error (message "error [nandu-org-mode-hook]: %s" (error-message-string err))))
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


(defun nandu-org-walker (el)
    (when (string= "headline" (car el))
      (let ((children (mapcar 'nandu-org-walker (nthcdr 2 el))))
        (or (remove nil children) ;; because of this, needs mapcar not mapc!
            (cons (org-element-property :end el) (org-element-property :raw-value el))))))

(defun nandu-insert-into-leaves (re)
  "Insert text at the end of each leave of an org-mode subtree.

Use: 1) Mark a region of text to be copied. The region can contain the special symbol ~{}~ to be replaced with the text of the heading under which it is being inserted
     2) When prompted, enter search string for which all headings only will be searched.

The text, with expansion of the headline if applicable, will be inserted at the :end of each paragraph corresponding to a 'leave' heading, i. e. one without any subheadings under it.
"
  (interactive "ssearch string: ")
  (let* ((text (buffer-substring-no-properties (point) (mark)))
         (ast (org-element-map (org-element-parse-buffer) 'headline
                (lambda (el)
                  (let ((val (org-element-property :raw-value el)))
                    (when (string-match-p re val) el)))))
         (walk (mapcar 'nandu-org-walker ast)))
    (dolist (el (--sort (> (car it) (car other)) (-flatten walk)))
      (let* ((text (replace-regexp-in-string "[^\\]{}" (cdr el) text))
             (text (replace-regexp-in-string "\\\\{\\\\}" "{}" text)))
        (save-excursion
          (goto-char (car el))
          (insert text))))))
