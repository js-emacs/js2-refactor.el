(require 'dash)

(defun js2r--slurpable-node-p (node)
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
  (let* ((slurpable (js2r--closest 'js2r--slurpable-node-p))
         (standalone (if (js2r--standalone-node-p slurpable)
                   slurpable
                 (js2-node-parent-stmt slurpable)))
         (next-sibling (js2-node-next-sibling standalone))
         (beg (js2-node-abs-pos next-sibling))
         (end (1+ (js2-node-abs-end next-sibling))) ;; include whitespace after statement
         (text (buffer-substring beg end)))
    (save-excursion
      (delete-region beg end)
      (goto-char (js2-node-abs-end slurpable))
      (forward-char -1)
      (when (looking-back "{ *") (newline))
      (setq beg (point))
      (insert text)
      (indent-region beg end))))

(provide 'js2r-paredit)

;;; js2r-paredit.el ends here
