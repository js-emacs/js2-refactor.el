;;; espuds-helpers.el --- Helper functions


(defun espuds-fake-eval (contents)
  "Dump contents to a temp file and then load it."
  (let ((file (make-temp-file "espuds-")))
    (with-temp-file file
      (insert contents))
    (load file)))

(defun espuds-buffer-contents ()
  "Return all text in current buffer."
  (buffer-string))

(defun espuds-region ()
  "Return the text selected by region, if any."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    ""))

(defun espuds-quit ()
  "Quit without signal."
  (flet ((signal (&rest args) nil))
    (keyboard-quit)))

(provide 'espuds-helpers)

;;; espuds-helpers.el ends here
