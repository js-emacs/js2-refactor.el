;;; -*- lexical-binding: t -*-

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
       (js2r--hl-get-constant-regions node)))))

(defun js2r-highlight-thing-at-point (pos)
  (interactive "d")
  (js2r-hl-forgetit)
  (js2r--hl-things (or (js2r--hl-get-regions pos)
                       (js2r--hl-get-regions (- pos 1)))))

(defun js2r--hl-get-free-vars-regions (pos)
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
                        (return-from is-free? t))
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

(defun js2r-highlight-free-vars (pos)
  (interactive "d")
  (js2r-hl-forgetit)
  (js2r--hl-things (js2r--hl-get-free-vars-regions pos)))

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

(defun js2r-highlight-exits (pos)
  (interactive "d")
  (js2r-hl-forgetit)
  (js2r--hl-things (js2r--hl-get-exits-regions pos)))

(defun js2r-hl--get-overlays (rev)
  (sort (remove-if-not (lambda (ov)
                         (overlay-get ov 'js2r-highlights))
                       (overlays-in (point-min) (point-max)))
        (if rev
            (lambda (a b)
              (> (overlay-start a) (overlay-start b)))
            (lambda (a b)
              (< (overlay-start a) (overlay-start b))))))

(defun js2r-hl-forgetit ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'js2r-highlights t)
  (js2r--hl-mode 0))

(defun js2r-hl-move-next ()
  (interactive)
  (catch 'done
    (dolist (i (js2r-hl--get-overlays nil))
      (let ((x (overlay-start i)))
        (when (> x (point))
          (goto-char x)
          (throw 'done nil))))))

(defun js2r-hl-move-prev ()
  (interactive)
  (catch 'done
    (dolist (i (js2r-hl--get-overlays t))
      (when (< (overlay-end i) (point))
        (goto-char (overlay-start i))
        (throw 'done nil)))))

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

(defun js2r-hl-rename (pos new-name)
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
    (js2r-hl-forgetit)))

(define-minor-mode js2r--hl-mode
  "Internal mode used by `js2r-highlights'"
  nil
  "/•"
  `(
    (,(kbd "C-<down>") . js2r-hl-move-next)
    (,(kbd "C-<up>") . js2r-hl-move-prev)
    (,(kbd "C-<return>") . js2r-hl-rename)
    (,(kbd "<escape>") . js2r-hl-forgetit)
    (,(kbd "C-g") . js2r-hl-forgetit)
    ))

(provide 'js2r-highlights)
