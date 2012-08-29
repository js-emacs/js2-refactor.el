;;; The startings of a modern list api for Emacs

;; Right now depends on cl for the heavy lifting, but that should be changed.

(require 'cl)

(defun !concat (list)
  (apply 'concatenate 'list list))

(defalias '!map 'mapcar)

(defun !mapcat (fn list)
  (!concat (!map fn list)))

(defun !uniq (list &optional compare-fn)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with COMPARE-FN if that's non-nil."
  (let (result)
    (while list
      (add-to-list 'result (car list) nil compare-fn)
      (setq list (cdr list)))
    (reverse result)))

(defun !union (list list2)
  (let (result)
    (while list
      (when (member (car list) list2)
        (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (reverse result)))

(defun !difference (list list2)
  (let (result)
    (while list
      (unless (member (car list) list2)
        (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (reverse result)))

(provide 'bang)
;;; bang.el ends here
