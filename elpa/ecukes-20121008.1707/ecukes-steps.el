;;; ecukes-steps.el --- Functions to define and call step definitions

(require 'ecukes-parse)

(defvar ecukes-steps-definitions nil
  "All defined step definitions.")


(defalias 'Given 'ecukes-steps-define-or-call-step
  "Put the system in a known state.")

(defalias 'When 'ecukes-steps-define-or-call-step
  "Describe the key action.")

(defalias 'Then 'ecukes-steps-define-or-call-step
  "Observe outcomes.")

(defalias 'And 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

(defalias 'But 'ecukes-steps-define-or-call-step
  "Make Given/When/Then read more fluently.")

(defun ecukes-steps-define-or-call-step (name &rest args)
  "Define or call step."
  (let ((arg (car args)))
    (if (functionp arg)
        (ecukes-steps-define name arg)
      (ecukes-steps-call name args))))

(defun ecukes-steps-define (regex fn)
  "Define step."
  (unless (--any? (equal regex it) ecukes-steps-definitions)
    (add-to-list
     'ecukes-steps-definitions
     (make-ecukes-step-def :regex regex :fn fn))))

(defun ecukes-steps-call (name args)
  "Call step"
  (let* ((query (apply 'format (cons name args)))
         (step-def (ecukes-steps-find query)))
    (if step-def
        (apply (ecukes-step-def-fn step-def)
               (or args
                   (ecukes-steps-args
                    (make-ecukes-step :body name))))
      (error (ansi-red "Step not defined: `%s`" query)))))

(defun ecukes-steps-missing-definition (steps)
  "Return from STEPS those who have not been defined."
  (--reject (ecukes-steps-find (ecukes-step-body it)) steps))

(defun ecukes-steps-find (name)
  "Find step by name."
  (--first (s-matches? (ecukes-step-def-regex it) name) ecukes-steps-definitions))

(defun ecukes-steps-args (step)
  "Return args from step BODY."
  (let* ((body (ecukes-step-body step))
         (step-def (ecukes-steps-find body)))
    (if step-def
        (cdr (s-match (ecukes-step-def-regex step-def) body))
      (loop for sub on (cdr (split-string body "\""))
            by (function cddr)
            collect (car sub)))))

(provide 'ecukes-steps)

;;; ecukes-steps.el ends here
