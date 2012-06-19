(defun js2r-unwrap (beg end)
  (interactive "r")
  (js2r--guard)
  (unless (use-region-p)
    (error "Select the code that you want keep when unwrapping."))
  (let* ((ancestor (js2-node-parent-stmt
                    (js2r--first-common-ancestor-in-region (point) (mark))))
         (abeg (js2-node-abs-pos ancestor))
         (aend (js2-node-abs-end ancestor)))
    (save-excursion
      (goto-char end)
      (delete-char (- aend end))
      (goto-char abeg)
      (delete-char (- beg abeg)))
    (indent-region (point-min) (point-max))))

(provide 'js2r-wrapping)
