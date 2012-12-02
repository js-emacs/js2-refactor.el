;;; ecukes-hooks.el --- A number of hooks that allow us to run code at various points in the test cycle

(defvar ecukes-hooks-before ()
  "List of before hooks.")

(defvar ecukes-hooks-after ()
  "List of after hooks.")

(defvar ecukes-hooks-setup ()
  "List of setup hooks.")

(defvar ecukes-hooks-teardown ()
  "List of teardown hooks.")


(defmacro define-hook (list body)
  `(add-to-list ,list (lambda () ,@body) t))

(defmacro Before (&rest body)
  "Run BODY in before hook."
  `(define-hook 'ecukes-hooks-before ,body))

(defmacro After (&rest body)
  "Run BODY in after hook."
  `(define-hook 'ecukes-hooks-after ,body))

(defmacro Setup (&rest body)
  "Run BODY in setup hook."
  `(define-hook 'ecukes-hooks-setup ,body))

(defmacro Teardown (&rest body)
  "Run BODY in teardown hook."
  `(define-hook 'ecukes-hooks-teardown ,body))


(defun ecukes-hooks-run-before ()
  "Run all before hooks."
  (ecukes-hooks-run ecukes-hooks-before))

(defun ecukes-hooks-run-after ()
  "Run all after hooks."
  (ecukes-hooks-run ecukes-hooks-after))

(defun ecukes-hooks-run-setup ()
  "Run all setup hooks."
  (ecukes-hooks-run ecukes-hooks-setup))

(defun ecukes-hooks-run-teardown ()
  "Run all teardown hooks."
  (ecukes-hooks-run ecukes-hooks-teardown))

(defun ecukes-hooks-run (hooks)
  "Run HOOKS."
  (-each hooks 'funcall))

(provide 'ecukes-hooks)

;;; ecukes-hooks.el ends here
