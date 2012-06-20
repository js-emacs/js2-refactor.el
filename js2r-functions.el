(require 'yasnippet)

;; Convert from regular arguments to object literal of named arguments.
;; Requires yasnippets

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


;; Extract function

(defun js2r-extract-function ()
  (interactive)
  (js2r--guard)
  (unless (fboundp 'yas/expand-snippet)
    (error "js2r-extract-function requires yasnippet to run."))
  (let ((contents (buffer-substring (point) (mark)))
        (containing-func (js2r--closest 'js2-function-node-p)))
    (deactivate-mark)
    (yas/expand-snippet (concat
                         "function ${1:name}() {\n"
                         "  " contents "\n"
                         "}\n"
                         "\n"
                         (js2r--string-replace contents "$1();$0"
                                               (js2-node-string containing-func)))
                        (js2-node-abs-pos containing-func)
                        (js2-node-abs-end containing-func))
    ))

;; for å finne names: må finne en common root for alle expressions i region.
;; så bruke js2-visit-ast på den, men filtrere på expressions som er innenfor region.
;; det er alle navnene. så finne containing scope for de.
;; sammenligne med containing-func
;; fjerne de som defineres i region?

;; må oppføre seg ganske annerledes for statements og expressions?
;;  expressions skal i hvert fall returne

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
