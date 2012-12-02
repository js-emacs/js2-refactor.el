;;; ecukes-new.el --- Setup up Ecukes for a project

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 's)
(require 'dash)
(require 'ansi)
(require 'ecukes-project)
(require 'ecukes-template)

(defun ecukes-new ()
  "Create new Ecukes setup for project."
  (if (ecukes-new-exists-p)
      (error
       (ansi-red "Ecukes already exists for this project"))
    (ecukes-new-create-root)
    (ecukes-new-create-step-definitions)
    (ecukes-new-create-support)
    (ecukes-new-create-feature)))

(defun ecukes-new-create-root ()
  "Create features directory."
  (make-directory (ecukes-project-features-path))
  (ecukes-new-message 0 "features"))

(defun ecukes-new-create-step-definitions ()
  "Create step-definitions directory and step definition."
  (let ((step-definitions-path (expand-file-name "step-definitions" (ecukes-project-features-path))))
    (make-directory step-definitions-path)
    (ecukes-new-message 2 "step-definition")
    (let ((step-definition
           (expand-file-name (format "%s-steps.el" (ecukes-project-name)) step-definitions-path)))
      (ecukes-template-write step-definition 'step-definition)))
  (ecukes-new-message 4 (ecukes-project-name) "-steps.el"))

(defun ecukes-new-create-support ()
  "Create support directory."
  (let ((support (expand-file-name "support" (ecukes-project-features-path))))
    (make-directory support)
    (ecukes-new-message 2 "support")
    (let ((env (expand-file-name "env.el" support)))
      (ecukes-template-write env 'env `(("project-name" . ,(ecukes-project-name))))))
  (ecukes-new-message 4 "env.el"))

(defun ecukes-new-create-feature ()
  "Create feature file."
  (let ((feature
         (expand-file-name
          (format "%s.feature" (ecukes-project-name)) (ecukes-project-features-path))))
    (ecukes-template-write feature 'feature))
  (ecukes-new-message 2 (format "%s.feature" (ecukes-project-name))))

(defun ecukes-new-exists-p ()
  "Check if there already exist an Ecukes setup."
  (file-directory-p (ecukes-project-features-path)))

(defun ecukes-new-message (indent &rest objects)
  "Print message about created file."
  (message
   "create%s%s"
   (s-repeat (1+ indent) " ")
   (ansi-green (apply 's-concat objects))))

(provide 'ecukes-new)

;;; ecukes-new.el ends here
