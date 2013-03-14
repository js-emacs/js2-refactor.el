(require 'cl)
(require 'dash)

;; Turn parameter into local var in local function

(defun js2r-localize-parameter ()
  (interactive)
  (js2r--guard)
  (if (js2-name-node-p (js2-node-at-point))
      (js2r--localize-parameter-pull)
    (js2r--localize-parameter-push)))

(defun js2r--localize-parameter-push ()
  (let* ((node (js2-node-at-point))
         (arg-node (or (js2r--closest-node-where 'js2r--parent-is-call-node node)
                       (error "Place cursor on argument to localize.")))
         (call-node (js2-node-parent arg-node))
         (value (js2-node-string arg-node))
         (target (js2-call-node-target call-node))
         (fn (if (js2-name-node-p target)
                 (js2r--local-fn-from-name-node target)
               (error "Can only localize parameter for local functions.")))
         (usages (js2r--function-usages fn))
         (index (car (--keep (when (eq arg-node it) it-index)
                             (js2-call-node-args call-node))))
         (name (js2-name-node-name (nth index (js2-function-node-params fn)))))
    (js2r--localize-parameter fn usages index name value)))

(defun js2r--localize-parameter-pull ()
  (let* ((name-node (js2-node-at-point))
         (name (if (js2-name-node-p name-node)
                   (js2-name-node-name name-node)
                 (error "Place cursor on parameter to localize.")))
         (fn (or (js2r--closest-node-where 'js2r--is-local-function name-node)
                 (error "Can only localize parameter in local functions.")))
         (index (or (js2r--param-index-for name fn)
                    (error "%S isn't a parameter to this function." name)))
         (usages (js2r--function-usages fn))
         (examples (--map (js2r--argument index it) usages))
         (value (js2r--choose-one "Value: " examples)))
    (js2r--localize-parameter fn usages index name value)))

(defun js2r--localize-parameter (fn usages index name value)
  (save-excursion
    (js2r--goto-fn-body-beg fn)
    (save-excursion
      (--each usages (js2r--remove-argument-at-index index it)))
    (newline-and-indent)
    (insert "var " name " = " value ";")
    (js2r--remove-parameter-at-index index fn)))

(defun js2r--parent-is-call-node (node)
  (js2-call-node-p (js2-node-parent node)))

(defun js2r--local-fn-from-name-node (name-node)
  (js2-node-parent
   (--first (js2-function-node-p (js2-node-parent it))
            (js2r--local-usages-of-name-node name-node))))

(defun js2r--param-index-for (name fn)
  (car (--keep (when (equal name (js2-name-node-name it)) it-index)
               (js2-function-node-params fn))))

(defun js2r--argument (index call-node)
  (js2-node-string (nth index (js2-call-node-args call-node))))

(defun js2r--remove-parameter-at-index (index fn)
  (js2r--delete-node-in-params (nth index (js2-function-node-params fn))))

(defun js2r--remove-argument-at-index (index call-node)
  (js2r--delete-node-in-params (nth index (js2-call-node-args call-node))))

(defun js2r--delete-node-in-params (node)
  (goto-char (js2-node-abs-pos node))
  (delete-char (js2-node-len node))
  (if (and (looking-back "(")
           (looking-at ", "))
      (delete-char 2)
    (when (looking-back ", ")
      (delete-char -2))))

(defun js2r--choose-one (prompt options)
  (when examples
    (if (cdr examples)
        (completing-read prompt examples)
      (car examples))))

;; Introduce parameter in local function

(defun js2r-introduce-parameter (beg end)
  (interactive "r")
  (js2r--guard)
  (unless (use-region-p)
    (error "Mark the expressions to introduce as parameter first."))
  (let ((fn (js2r--closest-node-where 'js2r--is-local-function (js2-node-at-point))))
    (unless fn
      (error "Can only introduce parameter in local functions."))
    (save-excursion
      (let ((name (read-string "Parameter name: "))
            (val (buffer-substring beg end))
            (usages (js2r--function-usages fn)))
        (goto-char beg)
        (save-excursion
          (-each usages (-partial 'js2r--add-parameter val)))
        (delete-char (- end beg))
        (insert name)
        (js2r--add-parameter name fn)
        (query-replace val name nil (js2-node-abs-pos fn) (js2r--fn-body-end fn))))))

(defun js2r--function-usages (fn)
  (-map 'js2-node-parent (js2r--function-usages-name-nodes fn)))

(defun js2r--function-usages-name-nodes (fn)
  (let ((name-node (or (js2-function-node-name fn)
                       (js2-var-init-node-target (js2-node-parent fn)))))
    (remove name-node (js2r--local-usages-of-name-node name-node))))

(defun js2r--add-parameter (name node)
  (save-excursion
    (js2r--goto-closing-paren node)
    (unless (looking-back "(")
      (insert ", "))
    (insert name)))

(defun js2r--goto-closing-paren (node)
  (goto-char (js2-node-abs-pos node))
  (search-forward "(")
  (forward-char -1)
  (forward-list)
  (forward-char -1))

(defun js2r--goto-fn-body-beg (fn)
  (goto-char (js2-node-abs-pos fn))
  (search-forward "{"))

(defun js2r--fn-body-end (fn)
  (save-excursion
    (js2r--goto-fn-body-beg fn)
    (forward-char -1)
    (forward-list)
    (point)))

(defun js2r--is-local-function (node)
  (or (js2r--is-var-function-expression node)
      (js2r--is-function-declaration node)))

(defun js2r--is-method (node)
  (and (js2-function-node-p node)
       (js2-object-prop-node-p (js2-node-parent node))))

(defun js2r--is-var-function-expression (node)
  (and (js2-function-node-p node)
       (js2-var-init-node-p (js2-node-parent node))))

(defun js2r--is-assigned-function-expression (node)
  (and (js2-function-node-p node)
       (js2-assign-node-p (js2-node-parent node))))

(defun js2r--is-function-declaration (node)
  (let ((parent (js2-node-parent node)))
    (and (js2-function-node-p node)
         (not (js2-assign-node-p parent))
         (not (js2-var-init-node-p parent))
         (not (js2-object-prop-node-p parent)))))

;; Change from a list of arguments to a parameter object

(defun js2r-arguments-to-object ()
  (interactive)
  (js2r--guard)
  (js2r--guard-yas)
  (unless (and (looking-at "(")
               (js2-call-node-p (js2-node-at-point)))
    (error "Place point right before the opening paren in the call."))
  (let ((args (js2-call-node-args (js2-node-at-point))))
    (when (null args)
      (error "No arguments to convert."))
    (let (arg key result)
      (--dotimes (length args)
        (setq arg (nth it args))
        (setq key (if (js2-name-node-p arg)
                      (js2-name-node-name arg)
                    "key"))
        (setq result
              (concat result
                      (format "    ${%d:%s}: %s,\n"
                              (1+ it)
                              key
                              (buffer-substring (js2-node-abs-pos arg)
                                                (js2-node-abs-end arg)))
                      )))
      (yas/expand-snippet (concat "({\n" (substring result 0 -2) "\n})")
                          (point)
                          (save-excursion (forward-list) (point))))))

;; Extract Function and Extract Method

(defun js2r-extract-function (name)
  (interactive "sName of new function: ")
  (js2r--extract-fn
   name
   #'(lambda ()
       (unless (js2r--looking-at-function-declaration)
         (goto-char (js2-node-abs-pos (js2r--closest 'js2-expr-stmt-node-p)))))
   "%s(%s);"
   "function %s(%s) {\n%s\n}\n\n"))

(defun js2r-extract-method (name)
  (interactive "sName of new method: ")
  (js2r--extract-fn
   name
   #'(lambda ()
       (goto-char (js2-node-abs-pos (js2r--closest 'js2-object-prop-node-p))))
   "this.%s(%s);"
   "%s: function (%s) {\n%s\n},\n\n"))

(defun js2r--extract-fn (name goto-position call-template function-template)
  (js2r--guard)
  (unless (use-region-p)
    (error "Mark the expressions to extract first."))
  (save-excursion
    (let* ((parent (js2r--first-common-ancestor-in-region (region-beginning) (region-end)))
           (block (js2r--closest-node-where 'js2-block-node-p parent))
           (fn (js2r--closest-node-where 'js2-function-node-p block))
           (exprs (js2r--marked-expressions-in-block block))
           (vars (-mapcat 'js2r--name-node-decendants exprs))
           (local (--filter (js2r--local-to-fn-p fn it) vars))
           (names (-distinct (-map 'js2-name-node-name local)))
           (declared-in-exprs (-map 'js2r--var-init-node-target-name (-mapcat 'js2r--var-init-node-decendants exprs)))
           (outside-exprs (-difference (js2-block-node-kids block) exprs))
           (outside-var-uses (-map 'js2-name-node-name (-mapcat 'js2r--name-node-decendants outside-exprs)))
           (declared-in-but-used-outside (-intersection declared-in-exprs outside-var-uses))
           (export-var (car declared-in-but-used-outside))
           (params (-difference names declared-in-exprs))
           (params-string (mapconcat 'identity (reverse params) ", "))
           (first (car exprs))
           (last (car (last exprs)))
           (beg (js2-node-abs-pos (car exprs)))
           (end (js2-node-abs-end last))
           (contents (buffer-substring beg end)))
      (goto-char beg)
      (delete-region beg end)
      (when (js2-return-node-p last)
        (insert "return "))
      (when export-var
        (setq contents (concat contents "\nreturn " export-var ";"))
        (insert "var " export-var " = "))
      (insert (format call-template name params-string))
      (goto-char (js2-node-abs-pos fn))
      (funcall goto-position)
      (let ((start (point)))
        (insert (format function-template name params-string contents))
        (indent-region start (1+ (point)))))))

(defun js2r--var-init-node-target-name (node)
  (js2-name-node-name
   (js2-var-init-node-target node)))

(defun js2r--function-around-region ()
  (or
   (js2r--closest-node-where 'js2-function-node-p
                             (js2r--first-common-ancestor-in-region
                              (region-beginning)
                              (region-end)))
   (error "This only works when you mark stuff inside a function")))

(defun js2r--marked-expressions-in-block (fn)
  (-select 'js2r--node-is-marked (js2-block-node-kids fn)))

(defun js2r--node-is-marked (node)
  (and
   (<= (region-beginning) (js2-node-abs-end node))
   (>= (region-end) (js2-node-abs-pos node))))

(defun js2r--name-node-decendants (node)
  (-select 'js2-name-node-p (js2r--decendants node)))

(defun js2r--var-init-node-decendants (node)
  (-select 'js2-var-init-node-p (js2r--decendants node)))

(defun js2r--decendants (node)
  (let (vars)
    (js2-visit-ast node
                   '(lambda (node end-p)
                      (unless end-p
                        (setq vars (cons node vars)))))
    vars))

(defun js2r--local-to-fn-p (fn name-node)
  (let* ((name (js2-name-node-name name-node))
         (scope (js2-node-get-enclosing-scope name-node))
         (scope (js2-get-defining-scope scope name)))
    (eq fn scope)))

;; Toggle between function name() {} and var name = function ();

(defun js2r-toggle-function-expression-and-declaration ()
  (interactive)
  (save-excursion
    (js2r--find-closest-function)
    (cond
     ((js2r--looking-at-var-function-expression) (js2r--transform-function-expression-to-declaration))
     ((js2r--looking-at-function-declaration) (js2r--transform-function-declaration-to-expression))
     (t (error "Can only toggle between function declarations and free standing function expressions.")))))

(defun js2r--find-closest-function ()
  (end-of-line)
  (word-search-backward "function")
  (while (er--point-inside-string-p)
    (word-search-backward "function")))

(defun js2r--looking-at-method ()
  (and (looking-at "function")
       (looking-back ": ?")))

(defun js2r--looking-at-function-declaration ()
  (and (looking-at "function")
       (looking-back "^ *")))

(defun js2r--looking-at-var-function-expression ()
  (and (looking-at "function")
       (looking-back "^ *var [a-z_$]+ = ")))

(defun js2r--transform-function-expression-to-declaration ()
  (when (js2r--looking-at-var-function-expression)
    (delete-char 9)
    (forward-list)
    (forward-list)
    (delete-char 1)
    (backward-list)
    (backward-list)
    (delete-backward-char 3)
    (back-to-indentation)
    (delete-char 4)
    (insert "function ")))

(defun js2r--transform-function-declaration-to-expression ()
  (when (js2r--looking-at-function-declaration)
    (delete-char 9)
    (insert "var ")
    (search-forward "(")
    (backward-char 1)
    (insert " = function ")
    (forward-list)
    (forward-list)
    (insert ";")))

(provide 'js2r-functions)
