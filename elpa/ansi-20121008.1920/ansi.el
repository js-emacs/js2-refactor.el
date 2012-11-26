;;; ansi.el --- Turn string into ansi strings

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.2
;; Keywords: color, ansi
;; URL: http://github.com/rejeep/ansi

;; This file is NOT part of GNU Emacs.


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This package defines functions that turn string into ansi colored
;; strings.

;; You can paint strings (see `ansi-colors' for all possible text
;; colors).
;;
;;   (ansi-red "foo")
;;   (ansi-black "bar")
;;
;;
;; You can paint a string backgrounds (see `ansi-on-colors' for all
;; possible background colors).
;;
;;   (ansi-on-blue "foo")
;;   (ansi-on-green "bar")
;;
;;
;; You can add styles to a string (see `ansi-styles' for all possible
;; styles).
;;
;;   (ansi-bold "foo")
;;   (ansi-blink "bar")
;;
;;
;; You can use `with-ansi', which allows for a simplified DSL.
;;
;;   (with-ansi
;;    (red "foo")
;;    (black "bar"))
;;
;;   (with-ansi
;;    (on-blue "foo")
;;    (on-green "bar"))
;;
;;   (with-ansi
;;    (bold "foo")
;;    (blink "bar"))
;;
;;
;; If you want to add multiple effects on a single string, you can use
;; nesting:
;;
;;   (ansi-bold
;;    (ansi-red "foo"))
;;
;;   (with-ansi
;;    (bold
;;     (red "foo")))
;;
;;
;; Before adding effects on strings, the ansi functions first passes
;; their arguments to the `format' function. This means you can write
;; like this:
;;
;;   (ansi-bold "%d passed, %d failed" passed failed)


;;; Code:

(eval-when-compile
  (require 'cl))


(defconst ansi-colors
  '((black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defconst ansi-on-colors
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
  "List of colors to draw text on.")

(defconst ansi-styles
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

(defconst ansi-reset 0
  "Ansi code for reset.")


(defmacro ansi-defun (list effect)
  "Creates an ansi function with EFFECT."
  (let ((fn-name (intern (format "ansi-%s" (symbol-name effect)))))
    `(defun ,fn-name (string &rest objects)
       ,(format "Add %s ansi effect on STRING." effect)
       (ansi-effect ,list ',effect string objects))))

(defmacro with-ansi (&rest body)
  "Allows using shortcut names of coloring functions."
  `(flet
       ,(mapcar
         (lambda (alias)
           (let ((fn (intern (format "ansi-%s" (symbol-name alias)))))
             `(,alias (string &rest objects) (apply ',fn (cons string objects)))))
         (append
          (mapcar 'car ansi-colors)
          (mapcar 'car ansi-on-colors)
          (mapcar 'car ansi-styles)))
     ,(cons 'ansi-concat body)))


(defun ansi-concat (&rest sequences)
  "Like `concat' but concats only the string values from SEQUENCES."
  (let ((strings (remove-if-not 'stringp sequences)))
    (apply 'concat strings)))

(defun ansi-effect (list effect string objects)
  "Add EFFECT to string."
  (let ((code (cdr (assoc effect list)))
        (formatted (apply 'format (cons string objects))))
    (format "\e[%sm%s\e[%sm" code formatted ansi-reset)))


;; COLORS
(ansi-defun ansi-colors black)
(ansi-defun ansi-colors red)
(ansi-defun ansi-colors green)
(ansi-defun ansi-colors yellow)
(ansi-defun ansi-colors blue)
(ansi-defun ansi-colors magenta)
(ansi-defun ansi-colors cyan)
(ansi-defun ansi-colors white)

;; ON COLORS
(ansi-defun ansi-on-colors on-black)
(ansi-defun ansi-on-colors on-red)
(ansi-defun ansi-on-colors on-green)
(ansi-defun ansi-on-colors on-yellow)
(ansi-defun ansi-on-colors on-blue)
(ansi-defun ansi-on-colors on-magenta)
(ansi-defun ansi-on-colors on-cyan)
(ansi-defun ansi-on-colors on-white)

;; STYLES
(ansi-defun ansi-styles bold)
(ansi-defun ansi-styles dark)
(ansi-defun ansi-styles italic)
(ansi-defun ansi-styles underscore)
(ansi-defun ansi-styles blink)
(ansi-defun ansi-styles rapid)
(ansi-defun ansi-styles contrary)
(ansi-defun ansi-styles concealed)
(ansi-defun ansi-styles strike)


(provide 'ansi)

;;; ansi.el ends here
