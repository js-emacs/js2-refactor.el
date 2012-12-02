;;; ecukes-print.el --- Print various stuff on screen

(require 'ecukes-stats)
(require 'ecukes-steps)
(require 'ecukes-template)

(defvar ecukes-print-offset 0
  "Current indentation offset (number of spaces).")

(defun ecukes-print-missing-steps (steps)
  "Print missing steps"
  (ecukes-print-missing-steps-header)
  (let ((step-bodies))
    (-each
     steps
     (lambda (step)
       (let ((step-body (ecukes-print-step-body step))
             (step-string (ecukes-print-step-string step)))
         (unless (--any? (equal step-body it) step-bodies)
           (add-to-list 'step-bodies step-body)
           (ecukes-print-message "%s\n" step-string)))))))

(defun ecukes-print-missing-steps-header ()
  "Print missing steps header."
  (ecukes-print-message
   (ansi-yellow "Some steps does not have a matching definition. Please implement the following step definitions:\n")))

(defun ecukes-print-step-string (step)
  "Return missing step string."
  (let ((head (ecukes-step-head step))
        (body (ecukes-print-step-body step))
        (args (ecukes-print-step-args step)))
    (ansi-yellow
     (ecukes-template-get
      'missing-step
      `(("head" . ,head)
        ("body" . ,body)
        ("args" . ,args))))))

(defun ecukes-print-step-args (step)
  "Return args from STEP."
  (let* ((result)
         (arg (ecukes-step-arg step))
         (args (ecukes-steps-args step))
         (type (ecukes-step-type step))
         (args-count
          (+
           (length args)
           (if (or
                (equal type 'table)
                (equal type 'py-string)) 1 0))))
    (if (= args-count 1)
        "arg"
      (progn
        (-dotimes
         args-count
         (lambda (n)
           (add-to-list 'result (format "arg-%d" (1+ n)) t)))
        (s-join " " result)))))

(defun ecukes-print-step-body (step)
  "Return body from STEP."
  (let* ((body (ecukes-step-body step))
         (args (ecukes-steps-args step))
         (result body))
    (when args
      (-each
       args
       (lambda (arg)
         (setq result (s-replace (s-concat "\"" arg "\"") "\\\"\\\\([^\\\"]+\\\\)\\\"" result)))))
    result))

(defun ecukes-print-intro (intro)
  "Print INTRO."
  (let ((ecukes-print-offset 0)
        (header (ecukes-intro-header intro)))
    (ecukes-print-message "Feature: %s" header)
    (let ((ecukes-print-offset 2)
          (description (ecukes-intro-description intro)))
      (--each description (ecukes-print-message it)))
    (ecukes-print-newline)))

(defun ecukes-print-background-header ()
  "Print background header."
  (let ((ecukes-print-offset 2))
    (ecukes-print-message "Background:")))

(defun ecukes-print-scenario-header (scenario)
  "Print SCENARIO header."
  (let ((name (ecukes-scenario-name scenario))
        (tags (ecukes-scenario-tags scenario))
        (ecukes-print-offset 2))
    (when tags
      (ecukes-print-message
       (ansi-cyan
        (s-join " " (--map (s-concat "@" it) tags)))))
    (ecukes-print-message "Scenario: %s" name)))

(defun ecukes-print-step (step status)
  "Print STEP in correct STATUS color."
  (let ((name (ecukes-step-name step))
        (err (ecukes-step-err step)))
    (let ((ecukes-print-offset 4))
      (ecukes-print-message
       (ecukes-print-status name status)))
    (if (eq (ecukes-step-type step) 'table)
        (ecukes-print-table step status)
      (if (eq (ecukes-step-type step) 'py-string)
          (ecukes-print-py-string step status)))
    (when (eq status 'failure)
      (let ((ecukes-print-offset 6))
        (-each
         (s-lines (or err "Unknown error..."))
         (lambda (line)
           (ecukes-print-message
            (ansi-red line))))))))

(defun ecukes-print-table (step status)
  "Print STEP table."
  (let* ((table (ecukes-step-arg step))
         (rows (length table))
         (cols (length (car table)))
         (widths))
    (-dotimes
     cols
     (lambda (col)
       (let ((width 0))
         (-dotimes
          rows
          (lambda (row)
            (setq width (max width (length (nth col (nth row table)))))))
         (push width widths))))
    (setq widths (reverse widths))
    (-dotimes
     rows
     (lambda (row)
       (let ((col-strings))
         (-dotimes
          cols
          (lambda (col)
            (let* ((orig (nth col (nth row table)))
                   (pad (- (nth col widths) (length orig)))
                   (col-string (s-concat (ecukes-print-status orig status) (s-repeat pad " "))))
              (add-to-list 'col-strings col-string t 'eq))))
         (let ((ecukes-print-offset 6))
           (ecukes-print-message (s-concat "| " (s-join " | " col-strings) " |"))))))))

(defun ecukes-print-py-string (step status)
  "Print STEP py-string."
  (let* ((arg (ecukes-step-arg step))
         (lines (s-lines arg)))
    (let ((ecukes-print-offset 6))
      (ecukes-print-message
       (ecukes-print-status "\"\"\"" status))
      (-each
       lines
       (lambda (line)
         (ecukes-print-message
          (ecukes-print-status line status))))
      (ecukes-print-message
       (ecukes-print-status "\"\"\"" status)))))

(defun ecukes-print-status (string status)
  "Return STRING in correct color depending on STATUS."
  (let ((color
         (cond ((eq status 'success)
                'ansi-green)
               ((eq status 'failure)
                'ansi-red)
               ((eq status 'skipped)
                'ansi-cyan))))
    (funcall color string)))

(defun ecukes-print-stats-summary ()
  "Print stats summary."
  (ecukes-print-message (ecukes-stats-summary)))

(defun ecukes-print-newline ()
  "Print newline."
  (ecukes-print-message " "))

(defun ecukes-print-message (format-string &rest args)
  "Print MESSAGE."
  (let ((ecukes-message t))
    (message (apply 'ecukes-print-format (cons format-string args)))))

(defun ecukes-print-format (format-string &rest args)
  "Return formatted message."
  (let ((message (apply 'format (cons format-string args)))
        (offset (s-repeat ecukes-print-offset " ")))
    (s-concat offset message)))

(provide 'ecukes-print)

;;; ecukes-print.el ends here
