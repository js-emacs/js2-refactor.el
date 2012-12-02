;;; ecukes-setup.el --- Common setup for drivers

(eval-when-compile
  (require 'cl))
(require 's)
(require 'dash)
(require 'ansi)
(require 'ecukes-def)
(require 'ecukes-steps)
(require 'ecukes-hooks)
(require 'ecukes-project)
(require 'ecukes-template)
(require 'ecukes-helpers)


(defvar ecukes-tags nil
  "List of tags.")

(defvar ecukes-verbose nil
  "Show `message' output if true.")

(defvar ecukes-message nil
  "Tells if current message if from Ecukes or external.")

(defvar ecukes-internal-message-log nil
  "List with `message' output.")

(defvar ecukes-message-log nil
  "List with `message' output (only from external code).")

(defadvice message (around message-around activate)
  (let ((message
         (if (car (ad-get-args 0))
             (apply 'format (ad-get-args 0))
           "")))
    (unless ecukes-message
      (add-to-list 'ecukes-message-log message t 'eq))
    (when (or ecukes-message ecukes-verbose)
      (add-to-list 'ecukes-internal-message-log `(message . ,message) t 'eq)
      ad-do-it)))

(defadvice print (around print-around activate)
  (add-to-list 'ecukes-internal-message-log `(print . ,ad-do-it) t 'eq))

(defun ecukes-quit (&optional exit-code)
  "Quit Emacs with EXIT-CODE and write to file if in graphical mode."
  (or exit-code (setq exit-code 1))
  (let ((outfile (getenv "ECUKES_OUTFILE"))
        (output
         (-map
          (lambda (log)
            (let ((type (car log))
                  (message (cdr log)))
              (if (eq type 'print)
                  (prin1-to-string message)
                message)))
          ecukes-internal-message-log)))
    (when outfile
      (with-temp-buffer
        (insert (s-join "\n" output) "\n")
        (write-file outfile nil))))
  (kill-emacs exit-code))

(defun usage ()
  "Show usage information and quit."
  (let ((ecukes-message t))
    (message
     (ecukes-template-get 'usage))
    (ecukes-quit)))

(defun ecukes-setup ()
  "Validate and load."
  (ecukes-setup-argv)
  (ecukes-setup-features-dir-exist)
  (ecukes-setup-load))

(defun ecukes-setup-argv ()
  "Setup options from `argv'."
  (let ((options))
    (when (-contains? argv "--dbg")
      (setq debug-on-error t)
      (setq debug-on-entry t)
      (setq ecukes-verbose t)
      (push "--dbg" options))
    (when (-contains? argv "--verbose")
      (setq ecukes-verbose t)
      (push "--verbose" options))
    (when (-contains? argv "--win")
      (push "--win" options))
    (let ((is-tag))
      (-each
       argv
       (lambda (arg)
         (cond ((equal arg "--tags")
                (push "--tags" options)
                (setq is-tag t))
               (t
                (when is-tag
                  (push arg options)
                  (setq
                   ecukes-tags
                   (-concat
                    ecukes-tags
                    (--map
                     (substring it 1)
                     (split-string arg ","))))
                  (setq is-tag nil)))))))
    (setq argv (-difference argv options))))

(defun ecukes-setup-features-dir-exist ()
  "Print usage and quit if there's no features directory."
  (unless (file-directory-p (ecukes-project-features-path))
    (let ((ecukes-message t))
      (message
       (ansi-red "Missing `features` directory.")))
    (usage)))

(defun ecukes-setup-load ()
  "Load support and step definitions."
  (ecukes-setup-load-support)
  (ecukes-setup-load-step-definitions))

(defun ecukes-setup-load-support ()
  "Load project support files."
  (let* ((env-file (expand-file-name "env.el" (ecukes-project-support-path)))
         (support-files
          (-reject
           (lambda (support-file)
             (s-equals? support-file env-file))
           (directory-files (ecukes-project-support-path) t "\\.el$"))))
    (load env-file nil t)
    (-map
     (lambda (support-file)
       (load support-file nil t))
     support-files)))

(defun ecukes-setup-load-step-definitions ()
  "Load project step definition files."
  (let ((step-definition-files (directory-files (ecukes-project-step-definitions-path) t "-steps\\.el$")))
    (-map
     (lambda (step-definition-file)
       (load step-definition-file nil t))
     step-definition-files)))


(provide 'ecukes-setup)
