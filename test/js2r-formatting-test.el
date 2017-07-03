;;; js2r-formatting-test.el --- Unit tests for js2r-formatting.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: test

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

;;

;;; Code:

(require 'js2-refactor)
(require 'buttercup)

(describe "Looking at function start"
  (it "should return nil when not looking at function start"
    (with-js2-buffer "function a() {}"
      (goto-char (point-min))
      (expect (js2r--looking-at-function-start) :to-be nil))
    (with-js2-buffer "function a() {}"
      (goto-char (point-max))
      (backward-char 1)
      (expect (js2r--looking-at-function-start) :to-be nil)))

  (it "should return t when looking at function start"
    (with-js2-buffer "function a() {}"
      (goto-char (point-max))
      (backward-char 2)
      (expect (js2r--looking-at-function-start) :to-be t))
    (with-js2-buffer "function a(a, b) {}"
      (goto-char (point-max))
      (backward-char 2)
      (expect (js2r--looking-at-function-start) :to-be t))
    (with-js2-buffer "function a(a, b){}"
      (goto-char (point-max))
      (backward-char 2)
      (expect (js2r--looking-at-function-start) :to-be t)))

  (it "should return t when looking at arrow function start"
    (with-js2-buffer " a => {}"
      (goto-char (point-max))
      (backward-char 2)
      (expect (js2r--looking-at-function-start) :to-be t))
    (with-js2-buffer "(a, b) => {}"
      (goto-char (point-max))
      (backward-char 2)
      (expect (js2r--looking-at-function-start) :to-be t))
    (with-js2-buffer "(a, b)=>{}"
      (goto-char (point-max))
      (backward-char 2)
      (expect (js2r--looking-at-function-start) :to-be t))))

(provide 'js2r-formatting-test)
;;; js2r-formatting-test.el ends here
