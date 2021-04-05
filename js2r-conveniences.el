;;; js2r-conveniences.el --- Convenience functions for js2-refactor    -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Convenience functions for logging statements by inserting
;; `console.log' statements and moving lines respecting object
;; literal syntax.

;;; Code:

(require 'js2r-helpers)

(defun js2r-log-this (arg)
  "Log of the node at point, adding a 'console.log()' statement.
With a prefix argument ARG, use JSON pretty-printing for logging."
  (interactive "P")
  (js2r--guard)
  (js2r--wait-for-parse
   (let* ((log-info (js2r--figure-out-what-to-log-where))
	  (stmt (car log-info))
	  (pos (cdr log-info)))
     (save-excursion
       (goto-char pos)
       (when (looking-at "[;{]")
	 (forward-char 1))
       (newline-and-indent)
       (if arg
	   (progn (insert "console.log(\"" stmt " = \");")
		  (newline-and-indent)
		  (insert "console.dir(" stmt ", { depth:null, colors: true });"))
	 (insert "console.log(\"" stmt " = \", " stmt ");"))))))

(defun js2r-debug-this ()
  "Debug the node at point, adding a 'debug()' statement."
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (let* ((log-info (js2r--figure-out-what-to-log-where))
	  (stmt (car log-info))
	  (pos (cdr log-info)))
     (save-excursion
       (goto-char pos)
       (when (looking-at "[;{]")
	 (forward-char 1))
       (newline-and-indent)
       (insert "debug(" (js2r--wrap-text stmt " = %s") ", " stmt ");")))))

(defun js2r--figure-out-what-to-log-where ()
  "Return a dotted pair containing the statement to log and the
position where the log should be inserted."
  (let ((parent-stmt (js2-node-parent-stmt (js2-node-at-point))))

    (if (use-region-p)
        (cons (buffer-substring (region-beginning) (region-end))
              (js2r--find-suitable-log-position-around parent-stmt))

      (let* ((node (js2r--name-node-at-point))
             (parent (js2-node-parent node)))

        (cond

         ((js2-function-node-p parent)
          (cons (js2-name-node-name node)
                (js2-node-abs-pos (js2-function-node-body parent))))

         ((js2-prop-get-node-p parent)
          (cons (buffer-substring (js2-node-abs-pos parent) (js2-node-abs-end parent))
                (js2r--find-suitable-log-position-around parent-stmt)))

         (:else
          (cons (js2-name-node-name node)
                (js2r--find-suitable-log-position-around parent-stmt))))))))

(defun js2r--find-suitable-log-position-around (parent-stmt)
  "Return the position close to PARENT-STMT where the log statement should be inserted."
  (if (or js2r-log-before-point (js2-return-node-p parent-stmt))
      (save-excursion
        (goto-char (js2-node-abs-pos parent-stmt))
        (skip-chars-backward " \t\n\r") ; Can't use skip-syntax-backward since \n is end-comment
        (point))
    (js2-node-abs-end parent-stmt)))

(defun js2r-split-string ()
  "Split the string node at point.  If the string is already split, join it instead."
  (interactive)
  (when (js2r--point-inside-string-p)
    (let ((delimiter (js2r--string-delimiter (js2-node-at-point))))
     (if (looking-back " \"")
         (progn
           (forward-char -2)
           (insert "  +")
           (forward-char -2))
       (if (looking-at (regexp-quote (format "%s + %s" delimiter delimiter)))
           (delete-char 5)
         (insert (format "%s + %s" delimiter delimiter)))))))

(defun js2r-string-to-template ()
  "Convert the string at point into a template string."
  (interactive)
  (let ((node (js2-node-at-point)))
    (when (js2-string-node-p node)
      (let* ((start (js2-node-abs-pos node))
             (end (+ start (js2-node-len node))))
        (when (memq (char-after start) '(?' ?\"))
          (save-excursion
            (goto-char end) (delete-char -1) (insert "`")
            (goto-char start) (delete-char 1) (insert "`")
            (perform-replace "`" "\\`" nil nil nil nil nil (1+ start) (1- end))))))))

(defun js2r--string-delimiter (node)
  "Return the delimiter character of the string node NODE.
It can be a single or double quote."
  (save-excursion
    (goto-char (js2-node-abs-pos node))
    (char-to-string (following-char))))

(defun move-line-down ()
  "Move the current line down one line."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  "Move the current line up one line."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun js2r-move-line-down ()
  "Move the current line down one line.
Make sure commas are placed correctly when moving a line up or
down in an object or array literal."
  (interactive)
  (if (and (js2r--current-line-is-a-list-item)
           (js2r--next-line-is-a-list-item))
      (js2r--move-line-down-as-list-item)
    (move-line-down))
  (funcall indent-line-function))

(defun js2r-move-line-up ()
  "Move the current line up one line.
Make sure commas are placed correctly when moving a line up or
down in an object or array literal."
  (interactive)
  (if (and (js2r--current-line-is-a-list-item)
           (js2r--previous-line-is-a-list-item))
      (js2r--move-line-up-as-list-item)
    (move-line-up))
  (funcall indent-line-function))

(defun js2r--current-line-is-prefixed-with-list-item-start ()
  "Return whether the current line is prefixed with '{' or '['."
  (save-excursion
    (back-to-indentation)
    (looking-back "\\({\\|\\[\\|,\\)\\(\s\\|\n\\|\t\\)*")))

(defun js2r--current-line-is-postfixed-with-list-item-end ()
  "Return whether the current line is postfixed with '{' or '['."
  (save-excursion
    (end-of-line)
    (or (looking-back ",\s*") ; line ends in comma
        (looking-at "\\(\s\\|\n\\|\t\\)*\\(\\]\\|}\\)"))))

(defun js2r--current-line-is-a-list-item ()
  "Return whether the current line contain an array or object literal."
  (and (js2r--current-line-is-prefixed-with-list-item-start)
       (js2r--current-line-is-postfixed-with-list-item-end)))

(defun js2r--next-line-is-a-list-item ()
  "Return whether the current line contain an array or object literal."
  (save-excursion
    (forward-line)
    (js2r--current-line-is-a-list-item)))

(defun js2r--previous-line-is-a-list-item ()
  "Return whether the previous line contain an array or object literal, and only that."
  (save-excursion
    (forward-line -1)
    (js2r--current-line-is-a-list-item)))

(defun js2r--current-line-has-comma ()
  "Return whether the current line ends with a comma."
  (save-excursion
    (end-of-line)
    (looking-back ",\s*")))

(defun js2r--previous-line-has-comma ()
  "Return whether the previous line ends with a comma."
  (save-excursion
    (forward-line -1)
    (js2r--current-line-has-comma)))

(defun js2r--move-line-down-as-list-item ()
  "Move the current line containing a list literal down one line, and also move the comma."
  (move-line-down)
  (if (not (js2r--previous-line-has-comma))
      (save-excursion
        (end-of-line)
        (delete-char -1)
        (forward-line -1)
        (end-of-line)
        (insert ","))))

(defun js2r--move-line-up-as-list-item ()
  "Move the current line containing a list literal up one line, and also move the comma."
  (move-line-up)
  (if (not (js2r--current-line-has-comma))
      (save-excursion
        (end-of-line)
        (insert ",")
        (forward-line)
        (end-of-line)
        (delete-char -1))))

(defun js2r-toggle-use-strict-defun ()
  "Toggle 'use strict' in the current function.
If `point' is not in a function, or FORCE-FILE is given, toggle
'use strict' globally. Otherwise, toggle it in the current
function."
  (interactive)
  (js2r--wait-for-parse
   (let* ((fun (or (js2r--defun-at-point)
                   (user-error "Not in a function")))
          (body (js2-function-node-body fun)))
     (unless (js2-block-node-p body)
       (user-error "Function body must be a block"))
     (save-excursion
       (goto-char (js2-node-abs-pos body))
       (save-match-data
         (if (search-forward-regexp js2r--use-strict-regexp
                                    (js2-node-abs-end body) t)
             (progn (js2r--delete-match)
                    (js2r--delete-blank-lines))
           (goto-char (js2-node-abs-pos body))
           (forward-char)
           (let ((start (point))
                 (oneline? (= (line-number-at-pos (js2-node-abs-end body))
                              (line-number-at-pos (js2-node-abs-pos body)))))
             (js2r--with-indentation
               (insert "\n" (js2r--use-strict))
               (unless (looking-at-p "\n[[:space:]]*$")
                 (insert "\n"))
               (when oneline?
                 (insert "\n"))))))))))

(defun js2r-toggle-use-strict-module ()
  "Toggle 'use strict' globally."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (re-search-forward (concat "^" js2r--use-strict-regexp) nil t)
          (progn (js2r--delete-match)
                 (js2r--delete-blank-lines))
        (insert (js2r--use-strict))
        (unless (looking-at-p "\n[[:space:]]*$")
          (insert "\n\n"))))))

(provide 'js2r-conveniences)
;;; js2r-conveniences.el ends here
