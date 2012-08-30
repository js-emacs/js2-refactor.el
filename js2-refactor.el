;;; js2-refactor.el --- The beginnings of a JavaScript refactoring library in emacs.

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region
;; Package-Requires: ((js2-mode "20101228") (mark-multiple "1.0.0"))

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

;; The beginnings of a JavaScript refactoring library in emacs.
;;
;; This is a collection of small refactoring functions to further the idea of a
;; JavaScript IDE in Emacs that started with js2-mode.
;;
;; ## Installation
;;
;; Start by installing the dependencies:
;;
;;  * js2-mode https://github.com/mooz/js2-mode/
;;  * mark-multiple https://github.com/magnars/mark-multiple.el
;;
;; It is also recommended to get
;; [expand-region](https://github.com/magnars/expand-region.el) to more easily mark
;; vars, method calls and functions for refactorings.
;;
;; Then add this to your emacs settings:
;;
;;     (require 'js2-refactor)
;;
;; ## Usage
;;
;; All refactorings start with `C-c C-m` and then a two-letter mnemonic shortcut.
;;
;;  * `ef` is `extract-function`: Extracts the marked expressions out into a new named function.
;;  * `em` is `extract-method`: Extracts the marked expressions out into a new named method in an object literal.
;;  * `eo` is `expand-object`: Converts a one line object literal to multiline.
;;  * `co` is `contract-object`: Converts a multiline object literal to one line.
;;  * `wi` is `wrap-buffer-in-iife`: Wraps the entire buffer in an immediately invoked function expression
;;  * `ig` is `inject-global-in-iife`: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
;;  * `ag` is `add-to-globals-annotation`: Creates a `/*global */` annotation if it is missing, and adds the var at point to it.
;;  * `ev` is `extract-var`: Takes a marked expression and replaces it with a var.
;;  * `iv` is `inline-var`: Replaces all instances of a variable with its initial value.
;;  * `rv` is `rename-var`: Renames the variable on point and all occurrences in its lexical scope.
;;  * `vt` is `var-to-this`: Changes local `var a` to be `this.a` instead.
;;  * `ao` is `arguments-to-object`: Replaces arguments to a function call with an object literal of named arguments. Requires yasnippets.
;;  * `3i` is `ternary-to-if`: Converts ternary operator to if-statement.
;;  * `sv` is `split-var-declaration`: Splits a `var` with multiple vars declared, into several `var` statements.
;;  * `uw` is `unwrap`: Replaces the parent statement with the selected region.
;;
;; There are also some minor conveniences bundled:
;;
;;  * `C-S-down` and `C-S-up` moves the current line up or down. If the line is an
;;    element in an object or array literal, it makes sure that the commas are
;;    still correctly placed.
;;
;;
;; ## Contributions
;;
;; * [Matt Briggs](https://github.com/mbriggs) contributed `js2r-add-to-globals-annotation`
;;
;; Thanks!
;;
;;
;; ## Contribute
;;
;; This project is still in its infancy, and everything isn't quite sorted out
;; yet. If you're eager to contribute, please add an issue here on github and we
;; can discuss your changes a little before diving into the elisp. :-)

;;; Code:

(require 'js2-mode)
(require 'js2r-helpers)

;;; Settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar js2r-use-strict nil
  "When non-nil, js2r inserts strict declarations in IIFEs.")


;;; Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-formatting)

(define-key js2-mode-map (kbd "C-c RET eo") 'js2r-expand-object)
(define-key js2-mode-map (kbd "C-c RET co") 'js2r-contract-object)


;;; Immediately invoked function expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-iife)

(define-key js2-mode-map (kbd "C-c RET wi") 'js2r-wrap-buffer-in-iife)
(define-key js2-mode-map (kbd "C-c RET ig") 'js2r-inject-global-in-iife)


;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-vars)

(define-key js2-mode-map (kbd "C-c RET ev") 'js2r-extract-var)
(define-key js2-mode-map (kbd "C-c RET iv") 'js2r-inline-var)
(define-key js2-mode-map (kbd "C-c RET rv") 'js2r-rename-var)
(define-key js2-mode-map (kbd "C-c RET vt") 'js2r-var-to-this)
(define-key js2-mode-map (kbd "C-c RET ag") 'js2r-add-to-globals-annotation)
(define-key js2-mode-map (kbd "C-c RET sv") 'js2r-split-var-declaration)

;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-functions)

(define-key js2-mode-map (kbd "C-c RET ef") 'js2r-extract-function)
(define-key js2-mode-map (kbd "C-c RET em") 'js2r-extract-method)
(define-key js2-mode-map (kbd "C-c RET tf") 'js2r-toggle-function-expression-and-declaration)
(define-key js2-mode-map (kbd "C-c RET ao") 'js2r-arguments-to-object)

;;; Wrapping ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-wrapping)

(define-key js2-mode-map (kbd "C-c RET uw") 'js2r-unwrap)
(define-key js2-mode-map (kbd "C-c RET wl") 'js2r-wrap-in-for-loop)

;;; Conditionals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-conditionals)

(define-key js2-mode-map (kbd "C-c RET 3i") 'js2r-ternary-to-if)

;;; Conveniences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-conveniences)

;; Make sure commas are placed correctly when moving a line in a literal
(define-key js2-mode-map (kbd "<C-S-down>") 'js2r-move-line-down)
(define-key js2-mode-map (kbd "<C-S-up>") 'js2r-move-line-up)


(provide 'js2-refactor)
;;; js2-refactor.el ends here
