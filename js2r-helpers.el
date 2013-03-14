(require 'dash)
(require 's)

(defun js2r--fix-special-modifier-combinations (key)
  (case key
    ("C-s-i" "s-TAB")
    ("C-s-m" "s-RET")
    (otherwise key)))

(defun js2r--key-pairs-with-modifier (modifier keys)
  (->> (string-to-list keys)
    (--map (js2r--fix-special-modifier-combinations
            (concat modifier (char-to-string it))))
    (s-join " ")
    (read-kbd-macro)))

(defun js2r--key-pairs-with-prefix (prefix keys)
  (read-kbd-macro (concat prefix " " keys)))

(defun js2r--guard ()
  (when js2-parsed-errors
    (error "Can't refactor while buffer has parse errors.")))

(defun js2r--guard-yas ()
  (unless (fboundp 'yas/expand-snippet)
    (error "This command requires yasnippet to run.")))

(defun js2r--current-quotes-char ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defalias 'js2r--point-inside-string-p 'js2r--current-quotes-char)

(defun js2r--closest-node-where (p node)
  (if (or (null node)
          (apply p node nil))
      node
    (js2r--closest-node-where p (js2-node-parent node))))

(defun js2r--closest (p)
  (save-excursion
    (cond
     ((bolp) (back-to-indentation))
     ((looking-at ";") (forward-char -1))
     ((looking-back ";") (forward-char -2))
     ((looking-back "}") (forward-char -1)))
    (js2r--closest-node-where p (js2-node-at-point))))

(defun js2r--goto-and-delete-node (node)
  (goto-char (js2-node-abs-pos node))
  (delete-char (js2-node-len node)))


(defun js2r--path-to-root (node)
  (when node
    (cons node (js2r--path-to-root (js2-node-parent node)))))

(defun js2r--first-common-ancestor (node1 node2)
  (if (eq node1 node2)
      node1
    (let ((path1 (reverse (js2r--path-to-root node1)))
          (path2 (reverse (js2r--path-to-root node2)))
          (last-common nil))
      (while (eq (car path1) (car path2))
        (setq last-common (car path1))
        (setq path1 (cdr path1))
        (setq path2 (cdr path2)))
      last-common)))

(defun js2r--first-common-ancestor-in-region (beg end)
  (js2r--first-common-ancestor (js2-node-at-point beg)
                               (js2-node-at-point end)))

(provide 'js2r-helpers)
