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

(defmacro js2r--create-bracketed-whitespace-traverser
  (name ws-fix-func looking-at-start-func
	goto-closest-start-func subexpr-str)
  "Build a function to expand or contract a given type of
   bracketed expression, i.e., function body, object literal, or
   array.
   Parameters:
       name:                    name of the function to be built
       ws-fix-func:             function to adjust whitespace at point
       looking-at-start-func:   returns t if point is at
                                    the start of the bracketed
                                    thing we want to act on
       goto-closest-start-func: moves point if necessary
                                    until looking-at-start-func
                                    is true
       subexpr-str:             literal delimiter of parts of the
                                    thing to be expanded or contracted"
  `(defun ,name ()
     (interactive)
     (save-excursion
       (if (not ,looking-at-start-func)
           ,goto-closest-start-func)
       (let ((end (make-marker)))
         (set-marker end (save-excursion
                           (forward-list)
                           (point)))
         (forward-char)
         ,ws-fix-func
         (while (< (point) end)
           (while (js2r--point-inside-string-p)
             (forward-char))
           (when (looking-at ,subexpr-str)
             (forward-char)
             ,ws-fix-func)
           (if (looking-at "\\s(")  ; FIXME: This might be more elegant if
				    ; abstracted out into a "looking-at-list-start"
				    ; function.
               (forward-list)
             (forward-char)))
         (backward-char)
         ,ws-fix-func))))

(js2r--create-bracketed-whitespace-traverser js2r-expand-object
					     (js2r--ensure-newline)
					     (js2r--looking-at-object-start)
					     (js2r--goto-closest-object-start)
					     ",")

(js2r--create-bracketed-whitespace-traverser js2r-contract-object
					     (js2r--ensure-just-one-space)
					     (js2r--looking-at-object-start)
					     (js2r--goto-closest-object-start)
					     ",")

(defun js2r--looking-at-array-start ()
  (looking-at "\\["))

(defun js2r--goto-closest-array-start ()
  (while (not (js2r--looking-at-array-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on an array")
      (goto-char (nth 1 (syntax-ppss))))))

(js2r--create-bracketed-whitespace-traverser js2r-expand-array
					     (js2r--ensure-newline)
					     (js2r--looking-at-array-start)
					     (js2r--goto-closest-array-start)
					     ",")

(js2r--create-bracketed-whitespace-traverser js2r-contract-array
					     (js2r--ensure-just-one-space)
					     (js2r--looking-at-array-start)
					     (js2r--goto-closest-array-start)
					     ",")
(defun js2r--looking-at-function-start ()
  (and (looking-at "{")
       ;; FIXME: Look backward for the "function" keyword, separated
       ;; from the open paren by, at most, a valid identifier and
       ;; whitespace.  Right now this will try to expand anything
       ;; surrounded by braces, e.g., a "for" loop body.
       (looking-back ")[\s\n]*")))

(defun js2r--goto-closest-function-start ()
  (while (not (js2r--looking-at-function-start))
    (if (eq (car (syntax-ppss)) 0)
        (error "Cursor is not on a function body")
      (goto-char (nth 1 (syntax-ppss))))))

(js2r--create-bracketed-whitespace-traverser js2r-expand-function
					     (js2r--ensure-newline)
					     (js2r--looking-at-function-start)
					     (js2r--goto-closest-function-start)
					     ";")

(js2r--create-bracketed-whitespace-traverser js2r-contract-function
					     (js2r--ensure-just-one-space)
					     (js2r--looking-at-function-start)
					     (js2r--goto-closest-function-start)
					     ";")


(js2r--create-object-whitespace-traverser js2r-expand-object
                                        (js2r--ensure-newline))

(js2r--create-object-whitespace-traverser js2r-contract-object
                                        (js2r--ensure-just-one-space))

(provide 'js2r-formatting)
