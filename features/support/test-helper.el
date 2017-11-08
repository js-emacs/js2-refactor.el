;; Temporary fix for https://github.com/ecukes/ecukes/issues/170

(when (require 'cl-preloaded nil t)
  (setf (symbol-function 'cl--assertion-failed)
	(lambda (form &optional string sargs args)
	  "Fake version"
	  ;; (if debug-on-error
	  ;;     (apply debugger `(cl-assertion-failed ,form ,string ,@sargs))
	  (if string
	      (apply #'error string (append sargs args))
	    (signal 'cl-assertion-failed `(,form ,@sargs))))))
