(defun js--looking-at-object-start ()
  (and (looking-at "{")
       (not (looking-back ")[\s\n]*"))))

(defun js--goto-closest-object-start ()
  (while (not (js--looking-at-object-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on an object")
      (goto-char (nth 1 (syntax-ppss))))))

(defun js--ensure-newline ()
  (if (and (not (looking-at "\s*\n"))
           (not (looking-back "\n\s*")))
      (newline-and-indent)))

(defun js--ensure-just-one-space ()
  (interactive)
  (while (or (looking-at "\s*\n")
             (looking-back "\n\s*"))
    (when (looking-at "\n")
      (delete-char 1))
    (when (looking-back "\n\s")
      (backward-char)
      (delete-char -1))
    (just-one-space))
  (just-one-space))

(defmacro js--create-object-whitespace-traverser (name func)
  `(defun ,name ()
     (interactive)
     (save-excursion
       (if (not (js--looking-at-object-start))
           (js--goto-closest-object-start))
       (let ((end (make-marker)))
         (set-marker end (save-excursion
                           (forward-list)
                           (point)))
         (forward-char)
         ,func
         (while (< (point) end)
           (when (looking-at ",")
             (forward-char)
             ,func)
           (if (looking-at "\\s(")
               (forward-list)
             (forward-char)))
         (backward-char)
         ,func))))

(js--create-object-whitespace-traverser js-expand-object
                                        (js--ensure-newline))

(js--create-object-whitespace-traverser js-contract-object
                                        (js--ensure-just-one-space))

(provide 'js2r-formatting)
