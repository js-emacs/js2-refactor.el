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

(defconst js2r--iife-regexp "[[:space:]]*(\\(?:function (\)\\|() => {\\)")
(defconst js2r--use-strict-regexp "[[:space:]]*\\(['\"]\\)use strict\\1")

(defun js2r-wrap-in-iife (beg end)
  "Wrap the current region in an iife.
BEG and END are the start and end of the region, respectively."
  (interactive "r")
  (cl-assert (memq js2r-iife-function-style '(function-inner function lambda))
             nil "`js2r-iife-function-style' invalid")
  (let ((end-marker (copy-marker end t))
        (strict js2r-use-strict))
    (save-excursion
      (goto-char beg)
      (when (looking-at-p js2r--iife-regexp)
        (user-error "Region is already an immediately invoked function expression"))
      (when (looking-at-p js2r--use-strict-regexp)
        (setq strict nil))
      (insert "(" (pcase js2r-iife-function-style
                    ((or `function `function-inner) "function ()")
                    (`lambda "() =>"))
              " {\n")
      (when strict (insert (pcase js2r-prefered-quote-type
                             (1 "\"use strict\"")
                             (2 "'use strict'"))
                           ";\n"))
      (goto-char end-marker)
      (insert "}" (pcase js2r-iife-function-style
                    ((or `function `lambda) ")()")
                    (`function-inner "())"))
              ";\n")
      (indent-region beg (point)))
    (set-marker end-marker nil)))

(defun js2r-wrap-buffer-in-iife ()
  "Wrap the entire buffer in an immediately invoked function expression"
  (interactive)
  (js2r-wrap-in-iife (point-min) (point-max)))

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

(defun js2r-inject-global-in-iife ()
  "Create shortcut for marked global by injecting it in the wrapping IIFE"
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (save-excursion
     (let* ((name-pos (js2r--selected-name-positions))
	    (name-beg (car name-pos))
	    (name-end (cdr name-pos))
	    (name (buffer-substring-no-properties name-beg name-end))
	    (short (buster--global-shortcut name))
	    beg end)
       (unless (search-backward-regexp js2r--iife-regexp)
	 (error "No immediately invoked function expression found."))
       (deactivate-mark)
       (forward-char 11)
       (insert short)
       (unless (looking-at ")")
	 (insert ", "))
       (search-forward "{")
       (setq beg (point))
       (backward-char)
       (forward-list)
       (forward-char)
       (setq end (point))
       (insert name)
       (unless (looking-at ")")
	 (insert ", "))
       (replace-string name short t beg end)))))

(provide 'js2r-iife)
;;; js2-iife.el ends here
