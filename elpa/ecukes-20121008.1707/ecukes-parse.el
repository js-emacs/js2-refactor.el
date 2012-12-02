;;; ecukes-parse.el --- Simple line by line parser for feature files


(defconst ecukes-parse-intro-re
  "^\\s-*Feature:\\s-*\\(.+[^ ]\\)\\s-*$"
  "Regexp matching feature header.")

(defconst ecukes-parse-background-re
  "^\\s-*Background:"
  "Regexp matching background header.")

(defconst ecukes-parse-scenario-re
  "^[\t ]*Scenario:[\t ]*\\(.+?\\)[\t ]*$"
  "Regexp matching scenario header.")

(defconst ecukes-parse-step-re
  "^\\s-*\\(Given\\|When\\|Then\\|And\\|But\\)\\s-+\\(.+[^ ]\\)\\s-*$"
  "Regexp matching step.")

(defconst ecukes-parse-tags-re
  "^\\s-*@"
  "Regexp matching scenario tags.")

(defconst ecukes-parse-py-string-re
  "^\\s-*\"\"\""
  "Regexp matching py string.")

(defconst ecukes-parse-table-re
  "^\\s-*|.+|"
  "Regexp matching table.")


(defun ecukes-parse-feature (feature)
  "Parse FEATURE."
  (with-temp-buffer
    (insert-file-contents-literally feature)
    (goto-char (point-min))
    (let ((intro (ecukes-parse-intro))
          (background (ecukes-parse-background))
          (scenarios (ecukes-parse-scenarios)))
      (make-ecukes-feature :intro intro :background background :scenarios scenarios))))

(defun ecukes-parse-intro ()
  "Parse intro."
  (when (re-search-forward ecukes-parse-intro-re nil t)
    (let ((header (match-string 1)) (description))
      (while (not (progn (forward-line 1) (ecukes-parse-new-section-p)))
        (let ((line (ecukes-parse-line t)))
          (if line (add-to-list 'description line t))))
      (make-ecukes-intro :header header :description description))))

(defun ecukes-parse-background ()
  "Parse background."
  (when (re-search-forward ecukes-parse-background-re nil t)
    (let ((steps (ecukes-parse-block-steps)))
      (make-ecukes-background :steps steps))))

(defun ecukes-parse-scenarios ()
  "Parse scenarios."
  (let ((scenarios))
    (while (re-search-forward ecukes-parse-scenario-re nil t)
      (add-to-list 'scenarios (ecukes-parse-scenario) t))
    scenarios))

(defun ecukes-parse-scenario ()
  "Parse scenario."
  (let ((name (ecukes-parse-scenario-name))
        (tags (ecukes-parse-scenario-tags))
        (steps (ecukes-parse-block-steps)))
    (make-ecukes-scenario :name name :steps steps :tags tags)))

(defun ecukes-parse-scenario-name ()
  "Parse scenario name."
  (save-excursion
    (let ((line (ecukes-parse-line)))
      (nth 1 (s-match ecukes-parse-scenario-re line)))))

(defun ecukes-parse-scenario-tags ()
  "Parse tags."
  (save-excursion
    (forward-line -1)
    (let ((line (ecukes-parse-line t)))
      (when (and line (s-matches? ecukes-parse-tags-re line))
        (-distinct
         (-map
          (lambda (tag)
            (substring tag 1))
          (split-string line "\\s-+")))))))

(defun ecukes-parse-block-steps ()
  "Parse steps in block."
  (let ((steps))
    (while (ecukes-forward-step)
      (let ((step (ecukes-parse-step)))
        (add-to-list 'steps step t 'eq)))
    steps))

(defun ecukes-parse-step ()
  "Parse step."
  (let* ((name (ecukes-parse-line t))
         (matches (s-match ecukes-parse-step-re name))
         (head (nth 1 matches))
         (body (nth 2 matches))
         (arg)
         (type))
    (cond
     ((ecukes-parse-py-string-step-p)
      (setq arg (ecukes-parse-py-string-step))
      (setq type 'py-string))
     ((ecukes-parse-table-step-p)
      (setq arg (ecukes-parse-table-step))
      (setq type 'table))
     (t (setq type 'regular)))
    (make-ecukes-step :name name :head head :body body :type type :arg arg)))

(defun ecukes-parse-table-step-p ()
  "Check if step is a table step or not."
  (save-excursion
    (forward-line 1)
    (let ((line (ecukes-parse-line)))
      (s-matches? ecukes-parse-table-re line))))

(defun ecukes-parse-table-step ()
  "Parse table step."
  (save-excursion
    (forward-line 1)
    (let ((rows))
      (while (s-matches? ecukes-parse-table-re (ecukes-parse-line))
        (add-to-list 'rows (ecukes-parse-table-step-row) t 'eq)
        (forward-line 1))
      rows)))

(defun ecukes-parse-table-step-row ()
  "Parse row in table."
  (let ((row (ecukes-parse-line)))
    (butlast (cdr (split-string row "\\s-*|\\s-*")))))

(defun ecukes-parse-py-string-step-p ()
  "Check if step is a py string step or not."
  (save-excursion
    (forward-line 1)
    (let ((line (ecukes-parse-line)))
      (s-matches? ecukes-parse-py-string-re line))))

(defun ecukes-parse-py-string-step ()
  "Parse py string step."
  (save-excursion
    (forward-line 1)
    (let ((whites
           (save-excursion
             (back-to-indentation)
             (current-column)))
          (lines))
      (forward-line 1)
      (while (not (s-matches? ecukes-parse-py-string-re (ecukes-parse-line)))
        (let ((line (ecukes-parse-line)))
          (push (if (<= whites (length line)) (substring line whites) nil) lines))
        (forward-line 1))
      (s-join "\n" (nreverse lines)))))

(defun ecukes-parse-line (&optional strip-whitespace)
  "Parse current line."
  (let* ((raw (buffer-substring (line-beginning-position) (line-end-position)))
         (line (if strip-whitespace (s-trim raw) raw)))
    (if (and strip-whitespace (equal line "")) nil line)))

(defun ecukes-forward-step ()
  "Go one step forward within current section."
  (forward-line 1)
  (let ((line (ecukes-parse-line t)))
    (unless (ecukes-parse-new-section-p)
      (if (s-matches? ecukes-parse-step-re (or line ""))
          (not (not line))
        (ecukes-forward-step)))))

(defun ecukes-parse-new-section-p ()
  "Check if current line is the start of a new section."
  (let ((line (or (ecukes-parse-line t) "")))
    (or
     (eobp)
     (s-matches? ecukes-parse-background-re line)
     (s-matches? ecukes-parse-scenario-re line)
     (s-matches? ecukes-parse-tags-re line))))


(provide 'ecukes-parse)

;;; ecukes-parse.el ends here
