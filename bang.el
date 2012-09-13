;;; The startings of a modern list api for Emacs

;; Right now depends on cl for the heavy lifting, but that should be changed.

(require 'cl)

(defvar !compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:
(let ((!compare-fn =)) (!union numbers1 numbers2 numbers3)")

(defun !concat (list)
  (apply 'concatenate 'list list))

(defalias '!map 'mapcar)
(defalias '!select 'remove-if-not)
(defalias '!partial 'apply-partially)

(defun !mapcat (fn list)
  (!concat (!map fn list)))

(defun !uniq (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (let (result)
    (while list
      (add-to-list 'result (car list) nil !compare-fn)
      (setq list (cdr list)))
    (reverse result)))

(defun !intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (let (result)
    (while list
      (when (!contains-p list2 (car list))
        (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (reverse result)))

(defun !difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (let (result)
    (while list
      (unless (!contains-p list2 (car list))
        (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (reverse result)))

(defun !contains-p (list element)
  "Return whether LIST contains ELEMENT.
The test for equality is done with `equal',
or with `!compare-fn' if that's non-nil."
  (cond
   ((null !compare-fn)    (member element list))
   ((eq !compare-fn 'eq)  (memq element list))
   ((eq !compare-fn 'eql) (memql element list))
   (t
    (let ((lst list))
      (while (and lst
                  (not (funcall !compare-fn element (car lst))))
        (setq lst (cdr lst)))
      lst))))

(provide 'bang)
;;; bang.el ends here
