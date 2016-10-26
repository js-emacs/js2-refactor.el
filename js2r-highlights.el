;;; -*- lexical-binding: t -*-

(defun js2r-highlight-thing-at-point (pos)
  "Highlight all occurrences of the thing at point.  Generally,
you would use this when the point is on a variable, and it will
highlight all usages of it in its defining scope.  You can also
use it on strings, numbers or literal regexps (highlights
occurrences in the whole buffer), or on keywords `this' and
`super' (highlights occurrences in the current function)."
  (interactive "d")
  (js2r-highlight-forgetit)
  (js2r--hl-things (or (js2r--hl-get-regions pos)
                       (js2r--hl-get-regions (- pos 1)))))

(defun js2r-highlight-free-vars (pos &optional undeclared?)
  "Highlights free variables in the function surrounding
point (all variables defined in an upper scope).  If a function
has no free variables, or if they are all globals, it can be
safely lifted to an upper scope.  By default undeclared variables
are not included (assumed to be globally defined somewhere else).
Pass a prefix argument if you need to include them (the optional
`undeclared?' argument)."
  (interactive "d\nP")
  (js2r-highlight-forgetit)
  (js2r--hl-things (js2r--hl-get-free-vars-regions pos undeclared?)))

(defun js2r-highlight-exits (pos)
  "Highlights forced exit points from the function surrounding
point, that is, `return' and `throw' statements."
  (interactive "d")
  (js2r-highlight-forgetit)
  (js2r--hl-things (js2r--hl-get-exits-regions pos)))

(defun js2r-highlight-rename (pos new-name)
  "Replace the highlighted things with something else.  Currently
this only works if the mode was called with
`js2r-highlight-thing-at-point'."
  (interactive "d\nsReplace with: ")
  (let ((places (sort (js2r--hl-get-regions pos)
                      (lambda (a b)
                        (< (cdr (assq 'begin b))
                           (cdr (assq 'begin a)))))))
    (save-excursion
     (dolist (p places)
       (let ((begin (cdr (assq 'begin p)))
             (end (cdr (assq 'end p))))
         (delete-region begin end)
         (goto-char begin)
         (insert new-name)))
     (message "%d occurrences renamed to %s" (length places) new-name))
    (js2r-highlight-forgetit)))

(defun js2r-highlight-forgetit ()
  "Exit the highlight minor mode."
  (interactive)
  (remove-overlays (point-min) (point-max) 'js2r-highlights t)
  (js2r--hl-mode 0))

(defun js2r-highlight-move-next ()
  "Move cursor to the next highlighted node."
  (interactive)
  (catch 'done
    (dolist (i (js2r--hl-get-overlays nil))
      (let ((x (overlay-start i)))
        (when (> x (point))
          (goto-char x)
          (throw 'done nil))))))

(defun js2r-highlight-move-prev ()
  "Move cursor to the previous highlighted node."
  (interactive)
  (catch 'done
    (dolist (i (js2r--hl-get-overlays t))
      (when (< (overlay-end i) (point))
        (goto-char (overlay-start i))
        (throw 'done nil)))))

(defun js2r--hl-get-var-regions ()
  (let* ((current-node (js2r--local-name-node-at-point))
         (len (js2-node-len current-node)))
    (mapcar (lambda (beg)
              `((begin . ,beg)
                (end . ,(+ beg len))))
            (js2r--local-var-positions current-node t))))

(defun js2r--constant-node-value (node)
  (cond
    ((js2-number-node-p node) (js2-number-node-value node))
    ((js2-string-node-p node) (js2-string-node-value node))
    ((js2-regexp-node-p node) (js2-regexp-node-value node))
    (t (error "Not a constant node"))))

(defun js2r--hl-get-constant-regions (const)
  (let* ((regions (list))
         (type (js2-node-type const))
         (value (js2r--constant-node-value const)))
    (js2-visit-ast js2-mode-ast
                   (lambda (node end-p)
                     (unless end-p
                       (cond
                         ((and (= type (js2-node-type node))
                               (equal value (js2r--constant-node-value node)))
                          (push `((begin . ,(js2-node-abs-pos node))
                                  (end . ,(js2-node-abs-end node)))
                                regions))))
                     t))
    regions))

(defun js2r--hl-get-regions (pos)
  (let ((node (js2-node-at-point pos)))
    (cond
      ((js2-name-node-p node) (js2r--hl-get-var-regions))
      ((or (js2-string-node-p node)
           (js2-number-node-p node)
           (js2-regexp-node-p node))
       (js2r--hl-get-constant-regions node))
      ((js2-this-or-super-node-p node)
       (js2r--hl-get-this-regions node)))))

(defun js2r--hl-get-this-regions (node)
  (let ((func (js2-mode-find-parent-fn node))
        (type (js2-node-type node))
        (regions (list)))
    (unless func
      (error "Not inside a function"))
    (js2-visit-ast func
                   (lambda (node end-p)
                     (cond
                       ((js2-function-node-p node)
                        (or (eq node func)
                            (eq (js2-function-node-form node) 'FUNCTION_ARROW)))
                       ((eq (js2-node-type node) type)
                        (push `((begin . ,(js2-node-abs-pos node))
                                (end . ,(js2-node-abs-end node)))
                              regions))
                       (t t))))
    regions))

(defun js2r--hl-get-free-vars-regions (pos undeclared?)
  (let* ((node (js2-node-at-point pos))
         (func (js2-mode-find-enclosing-fn node))
         (regions (list)))
    (cl-flet ((is-free? (node)
                (cl-block is-free?      ; cl-flet bug?
                  (let* ((name (js2-name-node-name node))
                         (sym (if (symbolp name) name (intern name)))
                         (p (js2-node-parent node)))
                    (while p
                      (when (js2-scope-p p)
                        (when (cdr (assq sym (js2-scope-symbol-table p)))
                          (return-from is-free? nil)))
                      (when (eq p func)
                        (return-from is-free?
                          (or undeclared?
                              (not (null (js2-get-defining-scope func name))))))
                      (setf p (js2-node-parent p)))))))
      (js2-visit-ast
       func
       (lambda (node end-p)
         (unless end-p
           (when (and (js2r--local-name-node-p node)
                      (not (and (js2-function-node-p func)
                                (eq node (js2-function-node-name func))))
                      (is-free? node))
             (push `((begin . ,(js2-node-abs-pos node))
                     (end . ,(js2-node-abs-end node)))
                   regions)))
         t)))
    regions))

(defun js2r--hl-get-exits-regions (pos)
  (let* ((node (js2-node-at-point pos))
         (func (js2-mode-find-parent-fn node))
         (regions (list)))
    (unless func
      (error "Not inside a function"))
    (js2-visit-ast
     func
     (lambda (node end-p)
       (cond
         ((js2-function-node-p node)
          (eq node func))
         ((not end-p)
          (when (or (js2-throw-node-p node)
                    (js2-return-node-p node))
            (push `((begin . ,(js2-node-abs-pos node))
                    (end . ,(js2-node-abs-end node)))
                  regions))
          t))))
    regions))

(defun js2r--hl-get-overlays (rev)
  (sort (remove-if-not (lambda (ov)
                         (overlay-get ov 'js2r-highlights))
                       (overlays-in (point-min) (point-max)))
        (if rev
            (lambda (a b)
              (> (overlay-start a) (overlay-start b)))
            (lambda (a b)
              (< (overlay-start a) (overlay-start b))))))

(defun js2r--hl-things (things &rest options)
  (let ((line-only (plist-get options :line-only))
        (things (sort things
                      (lambda (a b)
                        (< (cdr (assq 'begin a))
                           (cdr (assq 'begin b)))))))
    (cond
      (things (cl-loop
                 for ref in things
                 for beg = (cdr (assq 'begin ref))
                 for end = (if line-only
                               (save-excursion
                                (goto-char beg)
                                (end-of-line)
                                (point))
                               (cdr (assq 'end ref)))
                 do (let ((ovl (make-overlay beg end)))
                      (overlay-put ovl 'face 'highlight)
                      (overlay-put ovl 'evaporate t)
                      (overlay-put ovl 'js2r-highlights t)))
              (message "%d places highlighted" (length things))
              (js2r--hl-mode 1))
      (t
       (message "No places found")))))

(define-minor-mode js2r--hl-mode
  "Internal mode used by `js2r-highlights'"
  nil
  "/•"
  `(
    (,(kbd "C-<down>") . js2r-highlight-move-next)
    (,(kbd "C-<up>") . js2r-highlight-move-prev)
    (,(kbd "C-<return>") . js2r-highlight-rename)
    (,(kbd "<escape>") . js2r-highlight-forgetit)
    (,(kbd "C-g") . js2r-highlight-forgetit)
    ))

(provide 'js2r-highlights)
