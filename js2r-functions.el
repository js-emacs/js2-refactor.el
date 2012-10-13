;; Convert from regular arguments to object literal of named arguments.
;; Requires yasnippets

(require 'cl)
(require 'bang)

(defun js2r-arguments-to-object ()
  (interactive)
  (js2r--guard)
  (unless (fboundp 'yas/expand-snippet)
    (error "js2r-arguments-to-object requires yasnippet to run."))
  (unless (and (looking-at "(")
               (js2-call-node-p (js2-node-at-point)))
    (error "Place point right before the opening paren in the call."))
  (let ((args (js2-call-node-args (js2-node-at-point))))
    (when (null args)
      (error "No arguments to convert."))
    (let (arg key result)
      (dotimes (i (length args) result)
        (setq arg (nth i args))
        (setq key (if (js2-name-node-p arg)
                      (js2-name-node-name arg)
                    "key"))
        (setq result
              (concat result
                      (format "    ${%d:%s}: %s,\n"
                              (1+ i)
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
           (vars (!mapcat 'js2r--name-node-decendants exprs))
           (local (!!filter (js2r--local-to-fn-p fn it) vars))
           (names (!distinct (!map 'js2-name-node-name local)))
           (declared-in-exprs (!map 'js2r--var-init-node-target-name (!mapcat 'js2r--var-init-node-decendants exprs)))
           (outside-exprs (!difference (js2-block-node-kids block) exprs))
           (outside-var-uses (!map 'js2-name-node-name (!mapcat 'js2r--name-node-decendants outside-exprs)))
           (declared-in-but-used-outside (!intersection declared-in-exprs outside-var-uses))
           (export-var (car declared-in-but-used-outside))
           (params (!difference names declared-in-exprs))
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
  (remove-if-not 'js2r--node-is-marked (js2-block-node-kids fn)))

(defun js2r--node-is-marked (node)
  (and
   (<= (region-beginning) (js2-node-abs-end node))
   (>= (region-end) (js2-node-abs-pos node))))

(defun js2r--name-node-decendants (node)
  (remove-if-not 'js2-name-node-p (js2r--decendants node)))

(defun js2r--var-init-node-decendants (node)
  (remove-if-not 'js2-var-init-node-p (js2r--decendants node)))

(defun js2r--decendants (node)
  (lexical-let (vars)
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
