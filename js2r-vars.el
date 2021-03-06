;;; js2r-vars.el --- Variable declaration manipulation functions for js2-refactor    -*- lexical-binding: t; -*-

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

(require 'multiple-cursors-core)
(require 'dash)

(require 'js2r-helpers)

;; Helpers

(defun js2r--name-node-at-point (&optional pos)
  (setq pos (or pos (point)))
  (let ((current-node (js2-node-at-point pos)))
    (unless (js2-name-node-p current-node)
      (setq current-node (js2-node-at-point (- (point) 1))))
    (if (not (and current-node (js2-name-node-p current-node)))
        (error "Point is not on an identifier.")
      current-node)))

(defun js2r--local-name-node-at-point (&optional pos)
  (setq pos (or pos (point)))
  (let ((current-node (js2r--name-node-at-point pos)))
    (unless (js2r--local-name-node-p current-node)
      (error "Point is not on a local identifier"))
    current-node))

(defun js2r--local-name-node-p (node)
  (let ((parent (js2-node-parent node)))
    (and parent (js2-name-node-p node)
         ;; is not foo in { foo: 1 }
         (not (and (js2-object-prop-node-p parent)
                   (eq node (js2-object-prop-node-left parent))
                   ;; could be { foo } though, in which case the node
                   ;; is parent's both left and right.
                   (not (eq node (js2-object-prop-node-right parent)))))
         ;; is not foo in x.foo
         (not (and (js2-prop-get-node-p parent)
                   (eq node (js2-prop-get-node-right parent))))
         ;; is not foo in import { foo as bar } from ...
         ;; could be bar, though.
         (not (and (js2-export-binding-node-p parent)
                   (eq node (js2-export-binding-node-extern-name parent))
                   (not (eq node (js2-export-binding-node-local-name parent))))))))

(defun js2r--name-node-defining-scope (name-node)
  (unless (js2r--local-name-node-p name-node)
    (error "Node is not on a local identifier"))
  (js2-get-defining-scope
   (js2-node-get-enclosing-scope name-node)
   (js2-name-node-name name-node)))

(defun js2r--local-usages-of-name-node (name-node)
  (unless (js2r--local-name-node-p name-node)
    (error "Node is not on a local identifier"))
  (let* ((name (js2-name-node-name name-node))
         (scope (js2r--name-node-defining-scope name-node))
         (result nil))
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (js2r--local-name-node-p node)
                  (string= name (js2-name-node-name node))
                  (eq scope (js2r--name-node-defining-scope node)))
         (add-to-list 'result node))
       t))
    result))

(defun js2r--local-var-positions (name-node)
  (-map 'js2-node-abs-pos (js2r--local-usages-of-name-node name-node)))

(defun js2r--var-defining-node (var-node)
  (unless (js2r--local-name-node-p var-node)
    (error "Node is not on a local identifier"))
  (let* ((name (js2-name-node-name var-node))
         (scope (js2r--name-node-defining-scope var-node)))
    (js2-symbol-ast-node
     (js2-scope-get-symbol scope name))))


;; Add to jslint globals annotation

(defun current-line-contents ()
  "Find the contents of the current line, minus indentation."
  (buffer-substring (save-excursion (back-to-indentation) (point))
                    (save-excursion (end-of-line) (point))))

(require 'thingatpt)

(defun js2r-add-to-globals-annotation ()
  (interactive)
  (let ((var (thing-at-point 'symbol)))
    (save-excursion
      (beginning-of-buffer)
      (when (not (string-match "^/\\* *global " (current-line-contents)))
        (newline)
        (forward-line -1)
        (insert "/* global */")
        (newline)
        (forward-line -1))
      (while (not (string-match "*/" (current-line-contents)))
        (forward-line))
      (end-of-line)
      (delete-char -2)
      (unless (looking-back "global ")
        (while (looking-back " ")
          (delete-char -1))
        (insert ", "))
      (insert (concat var " */")))))


;; Rename variable

;;;###autoload
(defun js2r-rename-var ()
  "Renames the variable on point and all occurrences in its lexical scope."
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (let* ((current-node (js2r--local-name-node-at-point))
	  (len (js2-node-len current-node))
	  (current-start (js2-node-abs-pos current-node))
	  (current-end (+ current-start len))
	  (delta (- (point) current-start)))
     (save-excursion
       (mapc (lambda (beg)
	       (when (not (= beg current-start))
		 (goto-char (+ beg delta))
		 (set-mark (+ beg len))
		 (mc/create-fake-cursor-at-point)))
	     (js2r--local-var-positions current-node)))
     (push-mark current-end)
     (activate-mark))
   (mc/maybe-multiple-cursors-mode)))

(add-to-list 'mc--default-cmds-to-run-once 'js2r-rename-var)

;; Change local variable to use this. instead

(defun js2r-var-to-this ()
  "Changes the variable on point to use this.var instead."
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (save-excursion
     (let ((node (js2-node-at-point)))
       (when (js2-var-decl-node-p node)
	 (let ((kids (js2-var-decl-node-kids node)))
	   (when (cdr kids)
	     (error "Currently does not support converting multivar statements."))
	   (goto-char (js2-node-abs-pos (car kids))))))
     (--each (js2r--local-var-positions (js2r--local-name-node-at-point))
       (goto-char it)
       (cond ((looking-back "var ") (delete-char -4))
	     ((looking-back "let ") (delete-char -4))
	     ((looking-back "const ") (delete-char -6)))
       (insert "this.")))))

;; Inline var

(defun js2r-inline-var ()
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (save-excursion
     (let* ((current-node (js2r--local-name-node-at-point))
	    (definer (js2r--var-defining-node current-node))
	    (definer-start (js2-node-abs-pos definer))
	    (var-init (js2-node-parent definer))
	    (initializer (js2-var-init-node-initializer
			  var-init)))
       (unless initializer
	 (error "Var is not initialized when defined."))
       (let* ((var-len (js2-node-len current-node))
	      (init-beg (js2-node-abs-pos initializer))
	      (init-end (+ init-beg (js2-node-len initializer)))
	      (var-init-beg (copy-marker (js2-node-abs-pos var-init)))
	      (var-init-end (copy-marker (+ var-init-beg (js2-node-len var-init))))
	      (contents (buffer-substring init-beg init-end)))
	 (mapc (lambda (beg)
		 (when (not (= beg definer-start))
		   (goto-char beg)
		   (delete-char var-len)
		   (insert contents)))
	       (js2r--local-var-positions current-node))
	 (js2r--delete-var-init var-init-beg var-init-end))))))


(defun js2r--was-single-var ()
  (save-excursion
    (goto-char (point-at-bol))
    (or (looking-at "^[[:space:]]*\\(var\\|const\\|let\\)[[:space:]]?;?$")
	(looking-at "^[[:space:]]*,[[:space:]]*$"))))

(defun js2r--was-starting-var ()
  (or (looking-back "var ")
      (looking-back "const ")
      (looking-back "let ")))

(defun js2r--was-ending-var ()
  (looking-at ";"))

(defun js2r--delete-var-init (beg end)
  (goto-char beg)
  (delete-char (- end beg))
  (cond
   ((js2r--was-single-var)
    (delete-region (point-at-bol) (point-at-eol))
    (delete-blank-lines))

   ((js2r--was-starting-var)
    (delete-char 1)
    (if (looking-at " ")
        (delete-char 1)
      (join-line -1)))

   ((js2r--was-ending-var)
    (if (looking-back ", ")
        (delete-char -1)
      (join-line)
      (delete-char 1))
    (delete-char -1))

   (t (delete-char 2))))

;; two cases
;;   - it's the only var -> remove the line
;;   - there are several vars -> remove the node then clean up commas


;; Extract variable

(defun js2r--start-of-parent-stmt ()
  (js2-node-abs-pos (js2r--closest-stmt-node)))

(defun js2r--object-literal-key-behind (pos)
  (save-excursion
    (goto-char pos)
    (when (looking-back "\\sw: ?")
      (backward-char 2)
      (js2-name-node-name (js2r--name-node-at-point)))))

(defun js2r--line-above-is-blank ()
  (save-excursion
    (forward-line -1)
    (string= "" (current-line-contents))))

;;;###autoload
(defun js2r-extract-var ()
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (let ((keyword (if js2r-prefer-let-over-var "let" "var")))
        (if (use-region-p)
            (js2r--extract-var-between (region-beginning) (region-end) keyword)
          (let ((node (js2r--closest-extractable-node)))
            (js2r--extract-var-between (js2-node-abs-pos node)
                                       (js2-node-abs-end node) keyword))))))

;;;###autoload
(defun js2r-extract-let ()
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (if (use-region-p)
       (js2r--extract-var-between (region-beginning) (region-end) "let")
     (let ((node (js2r--closest-extractable-node)))
       (js2r--extract-var-between (js2-node-abs-pos node)
                                  (js2-node-abs-end node) "let")))))

;;;###autoload
(defun js2r-extract-const ()
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (if (use-region-p)
       (js2r--extract-var-between (region-beginning) (region-end) "const")
     (let ((node (js2r--closest-extractable-node)))
       (js2r--extract-var-between (js2-node-abs-pos node)
                                  (js2-node-abs-end node) "const")))))

(add-to-list 'mc--default-cmds-to-run-once 'js2r-extract-var)
(add-to-list 'mc--default-cmds-to-run-once 'js2r-extract-let)
(add-to-list 'mc--default-cmds-to-run-once 'js2r-extract-const)

(defun js2r--extract-var-between (beg end keyword)
  (interactive "r")
  (unless (js2r--single-complete-expression-between-p beg end)
    (error "Can only extract single, complete expressions to var"))

  (let ((deactivate-mark nil)
        (expression (buffer-substring beg end))
        (orig-var-end (make-marker))
        (new-var-end (make-marker))
        (name (or (js2r--object-literal-key-behind beg) "name")))

    (delete-region beg end)
    (insert name)
    (set-marker orig-var-end (point))

    (goto-char (js2r--start-of-parent-stmt))
    (js2r--insert-var name keyword)
    (set-marker new-var-end (point))
    (insert " = " expression ";")
    (when (or (js2r--line-above-is-blank)
              (string-match-p "^function " expression))
      (newline))
    (newline)
    (indent-region new-var-end orig-var-end)
    (save-excursion
      (goto-char new-var-end)
      (set-mark (- (point) (length name)))
      (mc/create-fake-cursor-at-point))
    (goto-char orig-var-end)
    (set-mark (- (point) (length name)))
    (set-marker orig-var-end nil)
    (set-marker new-var-end nil))
  (mc/maybe-multiple-cursors-mode))

(defun js2r--insert-var (name keyword)
  "Insert a var definition for NAME."
  (insert (format "%s %s" keyword name)))

(defun js2r-split-var-declaration ()
  "Split a variable declaration into separate variable
declarations for each declared variable."
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (save-excursion
     (let* ((declaration (or (js2r--closest #'js2-var-decl-node-p) (error "No var declaration at point.")))
	    (kids (js2-var-decl-node-kids declaration))
	    (stmt (js2-node-parent-stmt declaration))
        (tt (js2-var-decl-node-decl-type declaration))
        (keyword (cond
                  ((= tt js2-VAR) "var")
                  ((= tt js2-LET) "let")
                  ((= tt js2-CONST) "const"))))
       (goto-char (js2-node-abs-end stmt))
       (mapc (lambda (kid)
           (js2r--insert-var (js2-node-string kid) keyword)
	       (insert ";")
	       (newline)
	       (if (save-excursion
		     (goto-char (js2-node-abs-end kid))
		     (looking-at ", *\n *\n"))
		   (newline)))
	     kids)
       (delete-char -1) ;; delete final newline
       (let ((end (point)))
	 (js2r--goto-and-delete-node stmt)
	 (indent-region (point) end))))))

(provide 'js2r-vars)
;;; js2-vars.el ends here
