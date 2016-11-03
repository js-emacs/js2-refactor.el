;;; js2r-formatting.el --- Private helper functions for formatting

;; Copyright (C) 2012-2014 Magnar Sveen
;; Copyright (C) 2015-2016 Magnar Sveen and Nicolas Petton

;; Author: Magnar Sveen <magnars@gmail.com>,
;;         Nicolas Petton <nicolas@petton.fr>
;; Keywords: conveniences

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


(defun js2r--ensure-newline ()
  (if (and (not (looking-at "\s*\n"))
           (not (looking-back "\n\s*")))
      (newline-and-indent)))

(defun js2r--ensure-just-one-space ()
  (interactive)
  (while (or (looking-at "\s*\n")
             (looking-back "\n\s*"))
    (when (looking-at "\n")
      (delete-char 1))
    (when (looking-back "\n\s")
      (backward-char)
      (delete-char -1))
    (just-one-space))
  (just-one-space))

(defmacro js2r--create-bracketed-whitespace-traverser
  (name ws-fix-func looking-at-start-func
	goto-closest-start-func subexpr-str)
  "Build a function to expand or contract a given type of
   bracketed expression, i.e., function body, object literal, or
   array (any of which may be nested).
   Parameters:
       name:                    name of the function to be built
       ws-fix-func:             function to adjust whitespace at point
       looking-at-start-func:   returns t if point is at
                                    the start of the bracketed
                                    thing we want to act on
       goto-closest-start-func: moves point if necessary
                                    until looking-at-start-func
                                    is true
       subexpr-str:             literal delimiter of parts of the
                                    thing to be expanded or contracted"
  `(defun ,name ()
     (interactive)
     (save-excursion
       (if (not ,looking-at-start-func)
           ,goto-closest-start-func)
       (let ((end (make-marker)))
         (set-marker end (save-excursion
                           (forward-list)
                           (point)))
         (forward-char)
         ,ws-fix-func
         (while (< (point) end)
           (while (js2r--point-inside-string-p)
             (forward-char))
           (when (looking-at ,subexpr-str)
             (forward-char)
             (unless (js2-comment-node-p (js2-node-at-point))
              ,ws-fix-func))
           (if (looking-at "\\s(")
               (forward-list)
             (forward-char)))
         (backward-char)
         ,ws-fix-func))))

(defun js2r--looking-at-object-start ()
  (and (looking-at "{")
       (not (looking-back ")[\s\n]*"))))

(defun js2r--goto-closest-object-start ()
  (while (not (js2r--looking-at-object-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on an object")
      (goto-char (nth 1 (syntax-ppss))))))

(js2r--create-bracketed-whitespace-traverser js2r-expand-object
					     (js2r--ensure-newline)
					     (js2r--looking-at-object-start)
					     (js2r--goto-closest-object-start)
					     ",")

(js2r--create-bracketed-whitespace-traverser js2r-contract-object
					     (js2r--ensure-just-one-space)
					     (js2r--looking-at-object-start)
					     (js2r--goto-closest-object-start)
					     ",")

(defun js2r--looking-at-array-start ()
  (looking-at "\\["))

(defun js2r--goto-closest-array-start ()
  (while (not (js2r--looking-at-array-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on an array")
      (goto-char (nth 1 (syntax-ppss))))))

(js2r--create-bracketed-whitespace-traverser js2r-expand-array
					     (js2r--ensure-newline)
					     (js2r--looking-at-array-start)
					     (js2r--goto-closest-array-start)
					     ",")

(js2r--create-bracketed-whitespace-traverser js2r-contract-array
					     (js2r--ensure-just-one-space)
					     (js2r--looking-at-array-start)
					     (js2r--goto-closest-array-start)
					     ",")

(defun js2r--looking-at-function-start ()
  (or
   (and (looking-at "{")
        (looking-back
         ;; This horrible-looking regexp is actually pretty simple.  It
         ;; matches "function <optional_name> (<optional_parameters,...>)"
         ;; allowing for whitespace.  TODO: support Unicode in function and
         ;; parameter names.
         (concat "function[\s\n]*"
                 "\\\([a-zA-Z_$][a-zA-Z_$0-9]*[\s\n]*\\\)?"
                 "\(\\\([a-zA-Z_$][a-zA-Z_$0-9]*"
                 "[\s\n]*,[\s\n]*\\\)*[\s\n]*"
                 "\\\([a-zA-Z_$][a-zA-Z_$0-9]*[\s\n]*\\\)*"
                 "[\s\n]*\)[\s\n]*")))
   ;; arrow functions
   (and (looking-at "{")
        (looking-back "=>[\s\n]*")
        (not (js2r--point-inside-string-p)))))

(defun js2r--goto-closest-function-start ()
  (while (not (js2r--looking-at-function-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on a function body")
      (goto-char (nth 1 (syntax-ppss))))))

(js2r--create-bracketed-whitespace-traverser js2r-expand-function
					     (js2r--ensure-newline)
					     (js2r--looking-at-function-start)
					     (js2r--goto-closest-function-start)
					     ";")

;; TODO: It'd be great if js2r-contract-function could recognize
;; newlines that are implied statement terminators and insert
;; semicolons correctly, but that would probably mean not using the
;; same macro as the other "contract" function definitions.
(js2r--create-bracketed-whitespace-traverser js2r-contract-function
					     (js2r--ensure-just-one-space)
					     (js2r--looking-at-function-start)
					     (js2r--goto-closest-function-start)
					     ";")

(defun js2r--looking-at-call-start ()
  (looking-at "("))

(defun js2r--goto-closest-call-start ()
  (while (not (js2r--looking-at-call-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on a call")
      (goto-char (nth 1 (syntax-ppss))))))

(js2r--create-bracketed-whitespace-traverser js2r-expand-call-args
               (js2r--ensure-newline)
               (js2r--looking-at-call-start)
               (js2r--goto-closest-call-start)
               ",")

(js2r--create-bracketed-whitespace-traverser js2r-contract-call-args
               (js2r--ensure-just-one-space)
               (js2r--looking-at-call-start)
               (js2r--goto-closest-call-start)
               ",")

(defun js2-comments-between (start end &optional comments-list)
  "Return comment nodes between START and END, nil if not found.
START and END are absolute positions in current buffer.
Pass COMMENTS-LIST when no AST available."
  (let ((ast js2-mode-ast)
        (comments nil)
        c-start c-end)
    (setq comments-list (or comments-list
                            (and ast (js2-ast-root-comments ast))))
    (nreverse
      (dolist (comment comments-list comments)
        (setq c-start (js2-node-abs-pos comment)
              c-end (1- (+ c-start (js2-node-len comment))))
        (unless (or (< c-end start)
                    (> c-start end))
          (push comment comments))))))

(defun js2r--node-is-list (node)
  (or
   (js2-function-node-p node)
   (js2-block-node-p node)
   (js2-array-node-p node)
   (js2-object-node-p node)
   (js2-new-node-p node)
   (js2-call-node-p node)))

(defun js2r--node-child-list (target-node &optional recursive)
  "Get all child nodes for target-node, test with (js2r--node-is-list), recursively when recursive is t."
  (let ((result nil)
        (nodes nil)
        (children (cond
                   ((js2-function-node-p target-node)
                    (js2-block-node-kids (js2-function-node-body target-node)))
                   ((js2-block-node-p target-node)
                    (js2-block-node-kids target-node))
                   ((js2-array-node-p target-node)
                    (js2-array-node-elems target-node))
                   ((js2-object-node-p target-node)
                    (js2-object-node-elems target-node))
                   ((js2-call-node-p target-node)
                    (js2-call-node-args target-node))
                   ((js2-new-node-p target-node)
                    (js2-new-node-args target-node))
                   (t
                    nil)))
        (is-object (js2-object-node-p target-node))
        check-node node-parts)
    (dolist (node children)
      ;; node-parts is ALIST: ((node parent) (node parent))
      (setq node-parts nil)
      (setq check-node node)
      ;; get object parts (left, right)
      (if (and is-object (js2-object-prop-node-p node))
          (progn
            (setq check-node (js2-object-prop-node-right node))
            (push (list (js2-object-prop-node-left node) target-node) node-parts)
            (push (list check-node target-node) node-parts))
        (push (list node target-node) node-parts))
      (if (js2r--node-is-list check-node)
          (setq result
                (nconc result
                       (append node-parts (when recursive (js2r--node-child-list check-node recursive)))))
        (setq nodes (append nodes node-parts))))
    (nconc result (nreverse nodes))))

(defun js2r--expand-contract-node-at-point (&optional is-contract is-recursive)
  (save-excursion
    (let* ((node (js2-node-at-point))
           (target (if (and (not (js2-comment-node-p node))
                            (not (js2r--node-is-list node)))
                       (setq node (js2-node-parent node))
                     node))
           (target-start (js2-node-abs-pos target))
           (target-end (js2-node-abs-end target))
           (pos-list (list
                      (list :pos (1+ target-start) :node target :parent target)
                      (list :pos (1- target-end) :node target :parent target)))
           (sum-space 0)
           child-nodes
           pos-start pos-end
           asi node parent start end)
      (when (js2r--node-is-list target)
        (setq child-nodes (js2r--node-child-list target is-recursive))
        (dolist (N child-nodes)
          ;; N is (node parent)
          (setq node (car N))
          (setq start (js2-node-abs-pos node))
          (setq end (js2-node-abs-end node))
          (push (list :pos start :node node :role 'start :parent (cadr N)) pos-list)
          (push (list :pos end :node node :role 'end :parent (cadr N)) pos-list)
          (when (and is-recursive (js2r--node-is-list node))
            (push (list :pos (1+ start) :node node :role 'list-start :parent node) pos-list)
            (push (list :pos (1- end) :node node :role 'list-end :parent node) pos-list)
            ))
        (setq pos-list (sort pos-list '(lambda(a b) (< (plist-get a :pos) (plist-get b :pos)))))
        (dotimes (i (length pos-list))
          (setq start (nth i pos-list))
          (incf i)
          (setq end (nth i pos-list))
          (setq pos-start (plist-get start :pos))
          (setq pos-end (plist-get end :pos))
          (setq node (plist-get start :node))
          (setq parent (plist-get start :parent))
          ;; automatic semicolon insertion (ASI) sperated list, maybe omit ";"
          (setq asi (js2-block-node-p parent))
          (unless (js2-comments-between pos-start pos-end)
            ;; remove start part space
            (goto-char (- pos-start sum-space))
            (while (looking-at "[\n\s]")
              (delete-char 1)
              (incf sum-space))
            ;; remove end part space
            (goto-char (- pos-end sum-space))
            (while (looking-back "[\n\s]")
              (delete-char -1)
              (incf sum-space))
            ;; add ";" for ASI if it omitted
            (when (and is-contract asi (not (js2-block-node-p node)) (not (= (char-before) ?\;)))
              (insert ";")
              (decf sum-space))

            (if is-contract
                (insert " ")
              (if (and
                    ;; don't append newline for object prop name
                    ;; node-type 39 is "js2-name-node"
                   (= 39 (js2-node-type node))
                   (js2-object-node-p parent))
                  (insert " ")
                (newline)))
            (decf sum-space)
            ))
        (when (not is-contract)
          (js2-indent-region target-start (- target-end sum-space)))
        ))))

(defun js2r-expand-node-at-point (args)
  "Expand bracketed list according to node type at point."
  (interactive "P")
  (js2r--expand-contract-node-at-point nil args))

(defun js2r-contract-node-at-point (args)
  "Contract bracketed list according to node type at point."
  (interactive "P")
  (js2r--expand-contract-node-at-point t args))

(provide 'js2r-formatting)
;;; js2-formatting.el ends here
