(defun js2r--guard ()
  (when js2-parsed-errors
    (error "Can't refactor while buffer has parse errors.")))

(defun js2r--closest-node-where (p node)
  (if (or (null node)
          (apply p node nil))
      node
    (js2r--closest-node-where p (js2-node-parent node))))

(defun js2r--closest (p)
  (js2r--closest-node-where p (js2-node-at-point)))

(defun js2r--goto-and-delete-node (node)
  (goto-char (js2-node-abs-pos node))
  (delete-char (js2-node-len node)))

(provide 'js2-refactor-core)
