(require 'dash)

(defun js2r--nesting-node-p (node)
  (or (js2-function-node-p node)
      (js2-if-node-p node)
      (js2-for-node-p node)
      (js2-while-node-p node)))

(defun js2r--standalone-node-p (node)
  (or (js2-stmt-node-p node)
      (and (js2-function-node-p node)
           (eq 'FUNCTION_STATEMENT (js2-function-node-form node)))))

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
