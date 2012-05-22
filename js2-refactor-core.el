(defun js2r--guard ()
  (when js2-parsed-errors
    (error "Can't refactor while buffer has parse errors.")))

(provide 'js2-refactor-core)
