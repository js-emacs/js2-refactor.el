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

(defun js2r--comments-between (start end &optional comments-list)
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
    (let* ((current-node (js2-node-at-point))
            (target  (progn
                       ;; skip comment if point is on
                       (when (js2-comment-node-p current-node)
                         (setq current-node (js2-node-at-point (1+ (js2-node-abs-end current-node)))))
                       (while (not (js2r--node-is-list current-node))
                         (setq current-node (js2-node-parent current-node)))
                       current-node))
            (target-start (js2-node-abs-pos target))
            (target-end (js2-node-abs-end target))
            (pos-list (list
                        (list :pos target-start :node target :parent target)
                        (list :pos (1- target-end) :node target :parent target)))
            (newline (js2r--buffer-newline-char))
            child-nodes
            changes content
            pos-start pos-end
            asi node parent start end)
      ;; build pos-list for list node
      (when (js2r--node-is-list target)
        (setq child-nodes (js2r--node-child-list target is-recursive))
        (dolist (N child-nodes)
          ;; N is (node parent)
          (setq node (car N))
          (setq start (js2-node-abs-pos node))
          (setq end (js2-node-abs-end node))
          (push (list :pos (if (js2-new-node-p node)
                                 ;; new-node's start pos just before new
                               (progn (goto-char start) (backward-word) (point))
                             start) :node node :parent (cadr N)) pos-list)
          (push (list :pos end :node node :parent (cadr N)) pos-list)
          (when (and is-recursive (js2r--node-is-list node))
            (push (list :pos (1+ start) :node node :parent node) pos-list)
            (push (list :pos (1- end) :node node :parent node) pos-list))))
      (when (> (length pos-list) 2)
        ;; sort pos-list with :pos
        (setq pos-list (sort pos-list #'(lambda(a b) (< (plist-get a :pos) (plist-get b :pos)))))
        ;; perform expand/contract action with pos-list
        (dotimes (i (length pos-list))
          (setq start (nth i pos-list))
          (cl-incf i)
          (setq end (nth i pos-list))
          (setq pos-start (plist-get start :pos))
          (setq pos-end (plist-get end :pos))
          (setq node (plist-get start :node))
          (setq parent (plist-get start :parent))
          ;; automatic semicolon insertion (ASI) sperated list, maybe omit ";"
          (setq asi (js2-block-node-p parent))
          (unless (js2r--comments-between pos-start pos-end)
            (setq content (s-trim (buffer-substring pos-start pos-end)))
            ;; add ";" for ASI if it omitted
            (when (and is-contract asi (not (js2-block-node-p node)) (not (= (char-before pos-start) ?\;)))
              (setq content (concat content ";")))
            (if is-contract
                (setq content (concat content " "))
              (if (and
                   ;; don't append newline for object prop name
                   (js2-object-node-p parent)
                   ;; ensure it's not object start/end {}
                   (not (eq node parent))
                   (not (eq (plist-get end :node) parent))
                   ;; check object-prop-left-node-p (workaround)
                   (zerop (js2-node-pos node)))
                  (setq content (concat content " "))
                (setq content (concat content newline))))
            (push (list :beg pos-start :end pos-end :contents content) changes)))
        (js2r--execute-changes changes)
        (js2-reparse)))))

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
