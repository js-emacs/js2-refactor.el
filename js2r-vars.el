(require 'mark-multiple)

;; Rename variable

(defun js2r--name-node-at-point ()
  (let ((current-node (js2-node-at-point)))
    (unless (js2-name-node-p current-node)
      (setq current-node (js2-node-at-point (- (point) 1))))
    (if (not (and current-node (js2-name-node-p current-node)))
        (error "Point is not on an identifier.")
      current-node)))


(defun js2-rename-var ()
  "Renames the variable on point and all occurrences in its lexical scope."
  (interactive)
  (let* ((current-node (js2r--name-node-at-point))
         (name (js2-name-node-name current-node))
         (scope (js2-node-get-enclosing-scope current-node))
         (scope (js2-get-defining-scope scope name))
         (current-start (js2-node-abs-pos current-node))
         (current-end (+ current-start (js2-node-len current-node))))
    (push-mark current-end)
    (goto-char current-start)
    (activate-mark)
    (mm/create-master current-start current-end)
    (js2-with-unmodifying-text-property-changes
      (js2-visit-ast
       scope
       (lambda (node end-p)
         (when (and (not end-p)
                    (not (eq node current-node))
                    (js2-name-node-p node)
                    (string= name (js2-name-node-name node)))
           (let* ((start (js2-node-abs-pos node))
                  (end (+ start (js2-node-len node))))
             (mm/add-mirror start end)))
         t)))))


;; Extract variable

(defun js2r--start-of-parent-stmt ()
  (js2-node-abs-pos (js2-node-parent-stmt (js2-node-at-point))))

(defun js2r--object-literal-key-behind (pos)
  (save-excursion
    (goto-char pos)
    (when (looking-back "\\sw: ?")
      (backward-char 2)
      (js2-name-node-name (js2r--name-node-at-point)))))

(defun js2-extract-variable (start end)
  (interactive "r")
  (let ((deactivate-mark nil)
        (expression (buffer-substring start end))
        (varpos (make-marker))
        (name (or (js2r--object-literal-key-behind start) "name"))
        beg)
    (delete-region start end)
    (set-marker varpos (point))
    (insert name)
    (goto-char (js2r--start-of-parent-stmt))
    (insert "var ")
    (setq beg (point))
    (insert name)
    (insert (concat " = " expression ";\n"))
    (when (string-match-p "^function " expression)
      (insert "\n"))
    (goto-char varpos)
    (indent-region beg (point))
    (push-mark (+ (length name) varpos) t t)
    (mm/create-master varpos (+ (length name) varpos))
    (mm/add-mirror beg (+ (length name) beg))))

;; todo: mark-multiple should switch to multiple-cursors after first change
;;       also: always delete everything, not rely on region to do that.

(provide 'js2r-vars)
