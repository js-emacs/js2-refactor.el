(defun js2r--looking-at-object-start ()
  (and (looking-at "{")
       (not (looking-back ")[\s\n]*"))))

(defun js2r--goto-closest-object-start ()
  (while (not (js2r--looking-at-object-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on an object")
      (goto-char (nth 1 (syntax-ppss))))))

(defun js2r--ensure-newline ()
  (if (and (not (looking-at "\s*\n"))
           (not (looking-back "\n\s*")))
      (newline-and-indent)))

(defun js2r--ensure-just-one-space ()
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

(defmacro js2r--create-object-whitespace-traverser (name func)
  `(defun ,name ()
     (interactive)
     (save-excursion
       (if (not (js2r--looking-at-object-start))
           (js2r--goto-closest-object-start))
       (let ((end (make-marker)))
         (set-marker end (save-excursion
                           (forward-list)
                           (point)))
         (forward-char)
         ,func
         (while (< (point) end)
           (while (js2r--point-inside-string-p)
             (forward-char))
           (when (looking-at ",")
             (forward-char)
             ,func)
           (if (looking-at "\\s(")
               (forward-list)
             (forward-char)))
         (backward-char)
         ,func))))

(js2r--create-object-whitespace-traverser js2r-expand-object
                                        (js2r--ensure-newline))

(js2r--create-object-whitespace-traverser js2r-contract-object
                                        (js2r--ensure-just-one-space))

(provide 'js2r-formatting)
