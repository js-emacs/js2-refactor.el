;;; ecukes-project.el --- Project helpers

(defun ecukes-project-path (&optional dir)
  "Path to project."
  (or dir (setq dir default-directory))
  (if (file-directory-p (expand-file-name "features" dir))
      (directory-file-name dir)
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      (unless (equal dir "/")
        (ecukes-project-path new-dir)))))

(defun ecukes-project-name ()
  "Name of the project."
  (file-name-nondirectory (ecukes-project-path)))

(defun ecukes-project-features-path ()
  "Path to project features dir."
  (directory-file-name (expand-file-name "features" (ecukes-project-path))))

(defun ecukes-project-support-path ()
  "Path to project features dir."
  (directory-file-name (expand-file-name "support" (ecukes-project-features-path))))

(defun ecukes-project-step-definitions-path ()
  "Path to project step definitions dir."
  (directory-file-name (expand-file-name "step-definitions" (ecukes-project-features-path))))

(provide 'ecukes-project)

;;; ecukes-project.el ends here
