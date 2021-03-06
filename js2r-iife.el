;;; js2r-iife.el --- IIFE wrapping functions for js2-refactor    -*- lexical-binding: t; -*-

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

(require 'js2r-helpers)
(require 'cl-lib)

(defconst js2r--iife-regexp "[[:space:]]*(\\(?:function ([^)]*)\\|([^)]*) => {\\)")
(defconst js2r--use-strict-regexp "[[:space:]]*\\(['\"]\\)use strict\\1")

(defun js2r-looking-at-iife-p ()
  "Check if `point' is `looking-at' an IIFE."
  (looking-at-p js2r--iife-regexp))

(defun js2r-wrap-in-iife (beg end)
  "Wrap the current region in an iife.
BEG and END are the start and end of the region, respectively."
  (interactive "r")
  (cl-assert (memq js2r-iife-style '(function-inner function lambda))
             nil "`js2r-iife-style' invalid")
  (let ((end-marker (copy-marker end t))
        (strict js2r-use-strict))
    (save-excursion
      (goto-char beg)
      (when (js2r-looking-at-iife-p)
        (user-error "Region is already an immediately invoked function expression"))
      (when (looking-at-p js2r--use-strict-regexp)
        (setq strict nil))
      (insert "(" (pcase js2r-iife-style
                    ((or `function `function-inner) "function ()")
                    (`lambda "() =>"))
              " {\n")
      (when strict (insert (pcase js2r-prefered-quote-type
                             (1 "\"use strict\"")
                             (2 "'use strict'"))
                           ";\n"))
      (goto-char end-marker)
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (insert "}" (pcase js2r-iife-style
                    ((or `function `lambda) ")()")
                    (`function-inner "())"))
              ";\n")
      (indent-region beg (point)))
    (back-to-indentation)
    (set-marker end-marker nil)))

(defun js2r-wrap-buffer-in-iife ()
  "Wrap the entire buffer in an immediately invoked function expression"
  (interactive)
  (let ((eob-nl? (eq (char-before (point-max)) ?\n)))
    (js2r-wrap-in-iife (point-min) (point-max))
    (unless eob-nl?
      (save-excursion
        (goto-char (point-max))
        ;; `backward-delete-char' is advised in DOOM
        (delete-char -1)))))

(defun js2r--selected-name-positions ()
  "Returns the (beginning . end) of the name at cursor, or active region."
  (let ((current-node (js2-node-at-point))
        beg end)
    (unless (js2-name-node-p current-node)
      (setq current-node (js2-node-at-point (- (point) 1))))
    (if (not (and current-node (js2-name-node-p current-node)))
        (error "Point is not on an identifier."))
    (if (use-region-p)
        (cons (region-beginning) (region-end))
      (progn
        (setq end (+ (js2-node-abs-pos current-node)
                     (js2-node-len current-node)))
        (skip-syntax-backward ".w_")
        (cons (point) end)))))

(defun js2r--read-iife-short-name (name)
  "Read an iife short name for NAME.
See `js2r-add-global-to-iife'."
  (read-string "Short name (%s): " (substring name 0 1) nil name))

(defun js2r-add-global-to-iife (global short)
  "Add GLOBAL under the name SHORT to the current IIFE."
  (interactive
   (let ((global (read-string "Global: " (thing-at-point 'symbol))))
     (list global (js2r--read-iife-short-name global))))
  (save-excursion
    (save-match-data
      (atomic-change-group
        (let* (beg end)
          (unless (search-backward-regexp js2r--iife-regexp)
            (error "No immediately invoked function expression found"))
          (goto-char (match-end 0))
          (save-excursion
            (search-backward ")")
            (unless (= (char-before) ?\()
              (insert ", "))
            (insert short))
          (setq beg (point))
          (backward-char)
          (forward-list)
          (setq end (point))
          (unless (search-forward "(" (+ 3 (point)) t)
            (user-error "IIFE not called"))
          (forward-char -1)
          (forward-list)
          (forward-char -1)
          (unless (eq (char-before) ?\()
            (insert ", "))
          (insert global)
          (goto-char beg)
          (while (search-forward-regexp (format "\\_<%s\\_>" global) end t)
            (replace-match short t t)))))))

(defun js2r-inject-global-in-iife ()
  "Create shortcut for marked global by injecting it in the wrapping IIFE"
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (cl-destructuring-bind (beg . end) (js2r--selected-name-positions)
     (deactivate-mark)
     (let ((name (buffer-substring beg end)))
       (js2r-add-global-to-iife name (js2r--read-iife-short-name name))))))

(defun js2r-unwrap-iife ()
  "Unwrap the IIFE at `point'."
  (interactive)
  (save-match-data
    (unless (looking-at js2r--iife-regexp)
      (user-error "`point' is not on an IIFE"))
    (let ((body (save-excursion
                  (goto-char (match-end 0))
                  (forward-char -1)
                  (let ((start (1+ (point))))
                    (forward-list)
                    (forward-char -1)
                    (buffer-substring-no-properties start (point))))))
      (let ((start (point)))
        (forward-list)
        (delete-region start (point))

        (when (looking-at "\\(([^)]*)\\)?;$?")
          (delete-region (match-beginning 0) (match-end 0)))

        (insert (string-trim body))
        (indent-region start (point))
        (back-to-indentation)))))

(defun js2r-unwrap-iife-in-buffer ()
  "Unwrap the first IIFE in the current buffer.
See `js2r-wrap-buffer-in-iife'."
  (search-forward-regexp js2r--iife-regexp)
  (js2r-unwrap-iife))

(provide 'js2r-iife)
;;; js2-iife.el ends here
