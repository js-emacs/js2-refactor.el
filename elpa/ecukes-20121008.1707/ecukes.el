;;; ecukes.el --- Cucumber for Emacs

(defvar ecukes-path
  (file-name-directory load-file-name)
  "Path to ecukes.")

(add-to-list 'load-path ecukes-path)

(require 'ecukes-run)
(require 'ecukes-stats)
(require 'ecukes-setup)
(require 'ansi-color)

(defun ecukes (&optional ask-for-tags)
  (interactive "P")
  (unless (ecukes-project-path)
    (error "You are not visiting an Ecukes project."))
  (ecukes-setup)
  (if ask-for-tags
      (setq
       ecukes-tags
       (--map
        (substring it 1)
        (split-string (read-string "Run tags: ") ","))))
  (let ((feature-files
         (if (s-matches? "\.feature$" (buffer-file-name))
             (list (buffer-file-name))
           (directory-files (ecukes-project-features-path) t "\\.feature$"))))
    (let ((ecukes-buffer (get-buffer-create "*ecukes*"))
          (buffers (buffer-list))
          (ecukes-internal-message-log)
          (ecukes-stats-steps 0)
          (ecukes-stats-steps-passed 0)
          (ecukes-stats-steps-failed 0)
          (ecukes-stats-steps-skipped 0)
          (ecukes-stats-scenarios 0)
          (ecukes-stats-scenarios-passed 0)
          (ecukes-stats-scenarios-failed 0))
      (ecukes-run feature-files)
      (save-excursion
        (set-buffer ecukes-buffer)
        (erase-buffer)
        (-map
         (lambda (log)
           (let ((type (car log))
                 (message (cdr log)))
             (if (eq type 'message)
                 (insert (ansi-color-apply message) "\n"))))
         ecukes-internal-message-log)
        (font-lock-mode t)
        (toggle-read-only 1))
      (display-buffer ecukes-buffer)
      (-each
       (buffer-list)
       (lambda (buffer)
         (unless (-contains? buffers buffer)
           (let ((buffer-modified-p nil))
             (kill-buffer buffer))))))))

(provide 'ecukes)

;;; ecukes.el ends here
