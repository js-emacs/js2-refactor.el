(require 'dash)

(defun js2r--nesting-node-p (node)
  (or (js2-function-node-p node)
      (js2-if-node-p node)
      (js2-for-node-p node)
      (js2-while-node-p node)))

(defun js2r--balanced-node-p (node)
  (or (js2-array-node-p node)
      (js2-object-node-p node)
      (js2-string-node-p node)))

(defun js2r--standalone-node-p (node)
  (or (js2-stmt-node-p node)
      (and (js2-function-node-p node)
           (eq 'FUNCTION_STATEMENT (js2-function-node-form node)))))

(defun js2r-kill ()
  "Kill a line like `kill-line` but tries to respect the AST.
Falls back to `kill-line` if the buffer has parse errors."
  (interactive)
  (let ((node (js2-node-at-point)))
    (cond 
     ((js2-comment-node-p node) (kill-line))
     ((js2r--balanced-node-p node) (js2r--kill-line-in-balanced-exp))
     (t (js2r--kill-line-in-sexp)))))

(defun js2r--kill-line-in-sexp ()
  "Kill a line, but respecting the closest sexp, delimited with
  \")}]\"."
  (condition-case error
      (let* ((beg (point))
             (end (save-excursion
                    (up-list)
                    (forward-char -1)
                    (point))))
        (if (js2-same-line end)
            (kill-region beg end)
          (kill-line)))
    (scan-error 
     (message "Unbalanced parentheses. Killing the line.")
     (kill-line))))

(defun js2r--kill-line-in-balanced-exp ()
  "Kill a line, but respecting the closest balanced node (an
array, literal object or string node)."
  (if js2-parsed-errors 
      (progn
        (message "Buffer has parse errors. Killing the line.")
        (kill-line))
    (let* ((node (js2-node-at-point))
           (beg (point))
           (end (and node (1- (js2-node-abs-end node))))) 
      (if (and node (js2-same-line end))
          (kill-region beg end)
        (kill-line)))))

(defun js2r-forward-slurp ()
  (interactive)
  (js2r--guard)
  (let* ((nesting (js2r--closest 'js2r--nesting-node-p))
         (standalone (if (js2r--standalone-node-p nesting)
                         nesting
                       (js2-node-parent-stmt nesting)))
         (next-sibling (js2-node-next-sibling standalone))
         (beg (js2-node-abs-pos next-sibling))
         (end (1+ (js2-node-abs-end next-sibling))) ;; include whitespace after statement
         (text (buffer-substring beg end)))
    (save-excursion
      (delete-region beg end)
      (goto-char (js2-node-abs-end nesting))
      (forward-char -1)
      (when (looking-back "{ *") (newline))
      (setq beg (point))
      (insert text)
      (indent-region beg end))))

(defun js2r-forward-barf ()
  (interactive)
  (js2r--guard)
  (let* ((nesting (js2r--closest 'js2r--nesting-node-p))
         (standalone (if (js2r--standalone-node-p nesting)
                         nesting
                       (js2-node-parent-stmt nesting)))
         (standalone-end (js2-node-abs-end standalone))
         (last-child (car (last (if (js2-if-node-p nesting)
                                    (js2-scope-kids (js2r--closest 'js2-scope-p))
                                  (js2r--node-kids nesting)))))
         (last-child-beg (save-excursion
                           (goto-char (js2-node-abs-pos last-child))
                           (skip-syntax-backward " ")
                           (while (looking-back "\n") (backward-char))
                           (point)))
         (last-child-end (js2-node-abs-end last-child))
         (text (buffer-substring last-child-beg last-child-end)))
    (save-excursion
      (js2r--execute-changes
       (list
        (list :beg last-child-beg :end last-child-end :contents "")
        (list :beg standalone-end :end standalone-end :contents text))))))

(provide 'js2r-paredit)

;;; js2r-paredit.el ends here
