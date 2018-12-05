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
;; (with-eval-after-load 'ob-ipython
(when nil
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


;; "PUBLIC" FUNCTIONS (FOR KEY BINDINGS / SNIPPETS)
;; =======================================

(defun nandu-shift-return (&optional ctrl-c language)
  "\
Supposed behavior: 1) in src block without (:results . \"silent\") in :result-params
                   execute src block, then append new empty one after results
                   2) in src block with (:results . \"silent\") in :result-params
                   only execute
                   3) after src block or *if a region is selected*
                   only insert
                   4) on src block with existing #+RESULTS: or on #+RESULTS: block
                   only insert, after results

Variable of interest: `nandu-post-result-lines' has the number of empty lines to leave after src blocks or results."
  (catch :ctrl
    (let* ((pos (point-at-bol))
           (el (org-element-at-point))
           (blank (org-element-property :post-blank el))
           (fat (- nandu-post-result-lines blank))
           (language (or (org-element-property :language el) language)))
      (if (member (org-element-type el) '(src-block, babel-call))
          (progn
            (goto-char (org-element-property :end el))
            (setq blank (- (count-lines 1 (point-at-bol)) blank (count-lines 1 pos))))
        (when (not (string= "" yas-selected-text))
          (yas-expand-snippet (yas-lookup-snippet "org-babel source block"))
          (throw :ctrl t)))
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
                 (nandu-shift-return nil language))
               (throw :ctrl t))))
      (forward-line (min 0 fat))
      (newline (max 0 fat)))
    (if language
        (yas-expand-snippet
         (format "#+begin_src %s :results raw :session\n$0\n#+end_src\n" language))
    (yas-expand-snippet (yas-lookup-snippet "org-babel source block")))))

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
  (let ((res_dir (file-name-as-directory (expand-file-name (file-name-sans-extension (buffer-name)) ob-ipython-resources-dir))))
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


;; "PRIVATE" FUNCTIONS (USED INTERNALLY)
;; =====================================

(defun nandu--make-file-name (savefig)
  (let ((path nil)
        (ext "png")
        (res-dir (file-name-as-directory (expand-file-name (file-name-sans-extension (buffer-name)) ob-ipython-resources-dir))))
    (when (not (file-directory-p res-dir))
      (make-directory res-dir t))
    (catch :im_format
      (when savefig
        (setq ext (split-string savefig "\\."))
        (if (cdr ext)
            (progn
              (setq path (concat res-dir savefig))
              (throw :im_format t))
          (setq ext (car ext))))
      (setq path (concat (make-temp-name res-dir) "." ext)))
    path))

(defun nandu-append-figure-manually ()
  "\
Print a figure created in a src-block to a file and append a file link as #+RESULTS line.

The files are saved in ob-ipython-resources-dir/<buffer-name-sans-extension>. The optional :savefig directive can either contain a file name *with* extension (which will be used), or just the extension (e.g. `png') to create a random file name. If no :savefig is give, a random file name with the default extension (`png') is created."

  (interactive)
  (let* ((info (nth 2 (org-babel-get-src-block-info)))
        (path (nandu--make-file-name (alist-get :savefig info)))
        (savefig (file-name-nondirectory path))
        (result-params (split-string (alist-get :results info))))
    (org-babel-execute:ipython nil (append info `((:savefig . ,savefig))))
    (org-babel-insert-result (format "[[%s]]" path) result-params)
    (org-babel-insert-header-arg "savefig" savefig)
    (org-display-inline-images)))

(defun nandu-export-figure ()
  "\
Execute a src-block containing matplotlib instructions and save an .eps file with standard (`plt.style.use(\'default\')') styling.
"

  (interactive)
  (let* ((info (org-babel-get-src-block-info))
         (body (nth 1 info))
         (params (nth 2 info)))
    (org-babel-execute:ipython body (append params '((:nandu))))))

(defun nandu--existing-results-file-name ()
  (if-let ((beg (org-babel-where-is-src-block-result)))
    (save-excursion
      (goto-char beg)
      (let* ((el (org-element-at-point))
             (end (org-element-property :end el)))
        (condition-case nil
            (progn 
              (re-search-forward "\\[\\[file:\\(.*?\\)\]\]" end)
              (file-name-nondirectory (match-string-no-properties 1)))
          (error nil))))
    nil))


;; ADVICE
;; ======

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advice-combinators.html

;; this function gets advised :before org-display-image-remove-overlay, which is called by
;; modification-hooks for the image overlay
;; it causes image files to be deleted if the image overlay is removed
(defun nandu-create-ipython-process (name cmd)
  (let ((info (nth 2 (org-babel-get-src-block-info)))
        (runpy (concat nandu-layer-directory "/nandu_ipython_startup.py"))
        (styles (concat nandu-styles-directory "/nandu_dark.mplstyle")))
    (org-babel-execute:ipython
     (format
      "import runpy; runpy.run_path('%s', init_globals={'nandu_style_sheet':'%s'})" runpy styles) info)))

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

;; The following functions :override the ones provided by ob-ipython
;; their names are the same with 'ob-ipython-' replaced by 'nandu-'
;; -----------------------------------------------------------------

;; this is because ob-ipython uses python-shell-interpreter in order to
;; find the "python" command, whereas for elpy I need to set it to "jupyter"
;; ob-ipython currently uses this only in this particular function
;; one might have to change it if that changes
(defun nandu--get-python () "python3")

;; removed the extra line with # Out[...], because
;; 1) it interferes with the way I try to detect #+RESULTS:
;; 2) it seems useless, in particular since it doesn't correspond to an Out variable on the Python side
(defun nandu--process-response (ret file result-type)
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
(defun nandu--render (file-or-nil values)
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

(defun nandu-babel-execute:ipython (func body params)
  "\
Advice around org-babel-execute:ipython to deal with 1) figure export and 2) figure styling. See also \[nandu-append-figure-manually] and \[nandu-export-figure].

Supposed behavior: 1) If there is no :savefig directive in the header:
                      Just execute the src-block. If the src-block produces a figure, it can be appended afterwards with \[nandu-append-figure-manually].
                   2) If there is a :savefig directive:
                      Execute the src-block, automatically save the figure which is created by the code, and append the figure as file link in a #+RESULTS directive.
                   3) If, in addition to :savefig, :exports is specified as either `results' or `both':
                      The code block is evaluated first with a standard styling to produce an .eps figure in ./obipy-exports/<buffer-name-sans-extension>/ and second with either the currently active styling or the one given by the :style header, to produce a figure in ./obipy-resources/<bufer-name-sans-extension>/. Without a :savefig header, an :exports directive has no effect as far as this function is concerned, unless a #+RESULTS: paragraph is already appended and contains a file link to an existing file (in which case the base file name is used to produce the .eps file in ./obipy-exports).

An optional :style header refers to a matplotlib style sheet file and will be applied to any src-block. In addition to regular matplotlib locations, style sheets will be looked for in `nandu-styles-directory'.

The :savefig header can either be a filename with extension (default file format is .png), or just the extension without dot (e.g. `png'), in which case a filename is created at random but the extension is set to the one given.

One possible usage scenario without the :savefig directive is:
1) Execute src-block that produces a figure.
2) Execute \[nandu-append-figure-manually], which will create an arbitrary file name and insert a :savefig directive (if several are inserted, only the last one is in effect).
3) If desired, execute \[nandu-export-figure], which will create an .eps file with standard styling in ./obipy-exports, of the same base file name as the one produced in step 2)."

  (let ((fig (lambda (style path)
               (let ((result nil))
                 (when style
                   (funcall func
                            (format "nandu_ctxt=plt.style.context('%s'); nandu_ctxt.__enter__()" style)
                            params))
                 (when body
                   (setq result (funcall func body params)))
                 (when path
                   (funcall func (format "plt.gcf().savefig('%s', bbox_inches='tight'); plt.close()" path) params))
                 (when style
                   (funcall func "nandu_ctxt.__exit__(None, None, None); del nandu_ctxt" params))
                 (message "src-block executed with arguments %s %s" style path)
                 result)))
        (style (alist-get :style params))
        (savefig (if-let ((savef (alist-get :savefig params)))
                     (nandu--make-file-name savef)
                   nil)))

    (when style
      (setq style (or (car (directory-files nandu-styles-directory t style)) style)))

    (let ((exports (downcase (alist-get :exports params)))
          (existing-file (nandu--existing-results-file-name)))
      (when (and (or (string= exports "results") (string= exports "both") (assoc :nandu params))
                 (or existing-file savefig))
        (let ((exports-dir (file-name-as-directory
                            (concat "./obipy-exports/" (file-name-sans-extension (buffer-name)))))
              (file_name (or existing-file (file-name-nondirectory savefig))))
          (when (not (file-directory-p exports-dir))
            (make-directory exports-dir t))
          (let ((path (concat exports-dir (file-name-sans-extension file_name) ".eps")))
            (funcall fig "default" path))))

      (when (not (assoc :nandu params))
        (let ((result (funcall fig style savefig)))
          (or (and savefig (format "[[%s]]" savefig)) result))))))

;; -------------------------------
;; end :override advised functions

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
  (global-company-mode t)
  (global-visual-line-mode t)
  ;; cursor color clashes with farmhouse theme
  ;; gets reset if set earlier in the loading process
  (setq evil-insert-state-cursor '(bar "magenta"))
  ;; apparently locate-library only works after loading of elpy
  ;; the form `(... ,expr) means expr with comma before will be evaluated
  ;; before inserting into the list
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" (file-name-directory (symbol-file 'nandu-org-mode-hook))))
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" (file-name-directory (locate-library "elpy"))))
  ;; needs to be enabled *after* adding to yas-snippet-dirs
  (yas-global-mode t))

;; possibly to parse header arguments and hand to python by expanding the source
;; this will be necessary to have a possible change to the resources_dir if multiple different org files share the same kernel
(defun nandu-ctrl-c-ctrl-c-hook ()
  (let ((info (org-babel-get-src-block-info)))
    (when (string= "ipython" (car info))
      (let* ((body (nth 1 info))
             (var (format "buffer_file_name = '%s'\n" (buffer-file-name))))
        (when (not (string-match-p var body))
          (org-babel-update-block-body (concat var body)))))))


;; Org-mode functions
;; ==================

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

(defun nandu-link-jupyter2org ()
  "Replace Jupyter link markup with org markup (for 1 match following point)."
  (interactive)
  (re-search-forward "\\[\\([^\]]+\\)](\\([^)]+\\))" nil t 1)
  (replace-match "[[\\2][\\1]]"))

;; Jekyll
;; ======

(defun nandu-export-html ()
  (interactive)
  (let* ((jekyll-dir nil)
         (el (org-element-at-point))
         (head (org-get-heading t t t t))
         (title (replace-regexp-in-string
                 "\\[\\[[^\]]+\\]\\[\\([^\]]+\\)\\]\\]" "\\1" head))
         (fname (replace-regexp-in-string "[:=\(\)\?]" "" title))
         (fname (replace-regexp-in-string "[[:blank:]]" "-" title))
         (time (format-time-string "%Y-%m-%d"))
         (tags (org-get-tags))
         (categories nil))
    ;; CATEGORIES
    ;; everything between levels 2 and the headline of the post is a "category"
    ;; level 1 is any heading
    (save-excursion
      (while (< 2 (org-element-property :level (org-element-at-point)))
        (org-up-heading-safe)
        (setq categories (cons (org-get-heading t t t t) categories))))
    ;; #+JEKYLL_DIR: should set the directory to which to export
    (org-element-map (org-element-parse-buffer 'greater-element) 'keyword
      (lambda (el)
        (when (string= "JEKYLL_DIR" (upcase (org-element-property :key el)))
          (setq jekyll-dir (org-element-property :value el)))))
    (if-let* ((ts (org-element-property :scheduled el))
                (year (org-element-property :year-start ts))
                (month (org-element-property :month-start ts))
                (day (org-element-property :day-start ts)))
        (setq time (format "%d-%02d-%02d" year month day))
      (org-schedule nil time))
    (org-html-export-as-html nil t nil t)
    (with-current-buffer "*Org HTML Export*"
      (beginning-of-buffer)
      (insert "---\n")
      (insert (format "title: %s\n" title))
      (insert "layout: post\n")
      (when categories
        (insert (format "categories: [%s]\n" (string-join categories ", "))))
      (when (not (string= "" (car tags)))
        (insert (format "tags: [%s]\n" (string-join tags ", "))))
      (insert "---\n")
      (write-file (expand-file-name (format "%s-%s.html" time fname) jekyll-dir)))))

;; All Exports (including html for Jekyll)
;; =======================================
(defun nandu-org-export-before-processing-hook (backend)
  ;; pre-processes a #+DEFUN keyword by looking for the function and replacing the line in the buffer
  (when (org-export-derived-backend-p backend 'html)
    (org-element-map (org-element-parse-buffer 'element) 'keyword
      (lambda (el)
        (let ((key (upcase (org-element-property :key el))))
          (when (string= key "DEFUN")
            (let* ((v (org-element-property :value el))
                   (beg (org-element-property :begin el))
                   (end (org-element-property :end el))
                   (l (split-string v))
                   (text nil)
                   (ff (find-function-noselect (intern (car l)))))
              (save-current-buffer
                (set-buffer (car ff))
                (forward-sexp)
                (setq text (buffer-substring-no-properties (cdr ff) (point))))
              (save-excursion
                (delete-region beg end)
                (goto-char beg)
                (insert "#+begin_export html\n")
                (insert (format "{%% highlight %s %%}{%% raw %%}" (nth 1 l)))
                (insert text)
                (insert "{% endraw %}{% endhighlight %}\n")
                (insert "\n#+end_export\n"))))))))

  ;; when org-export-use-babel is nil, src-block header args are not parsed
  ;; this deletes all src blocks, starting from the back (so that the parsed :begin elements remain correct)
  (when (or (org-export-derived-backend-p backend 'beamer)
            (org-export-derived-backend-p backend 'latex))
    (let ((src (org-element-map (org-element-parse-buffer 'element) 'src-block
                 (lambda (el)
                   (condition-case nil
                       (when-let* ((info (nth 2 (org-babel-get-src-block-info nil el)))
                                   (exp (downcase (alist-get :exports info)))
                                   (check (or (string= "none" exp) (string= "results" exp)))
                                   (beg (org-element-property :begin el))
                                   (end (org-element-property :end el)))
                         `(,beg . ,end))
                     (error nil))))))
      (dolist (el (--sort (> (car it) (car other)) src))
        (delete-region (car el) (cdr el)))))

  ;; this replaces images in the ./obipy-resources folder with .eps ones in ./obipy-exports
  (when (org-export-derived-backend-p backend 'latex)
    (let ((exports-dir (file-name-as-directory (concat "./obipy-exports/" (file-name-sans-extension (buffer-name))))))
      (when (file-directory-p exports-dir)
        (let ((links (org-element-map (org-element-parse-buffer) 'link
                       (lambda (el)
                         (when (string= 'file (org-element-property :type el))
                           (let ((link-file (org-element-property :path el))
                                 (beg (org-element-property :begin el))
                                 (end (org-element-property :end el)))
                             (list beg end link-file)))))))
          (dolist (el (--sort (> (car it) (car other)) links))
            (save-excursion
              (message "replace file %s" (concat exports-dir (file-name-base (nth 2 el)) ".eps"))
              (delete-region (car el) (nth 1 el))
              (goto-char (car el))
              (insert (format "[[file:%s]]" (concat exports-dir (file-name-base (nth 2 el)) ".eps"))))))))))

