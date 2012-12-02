;;; espuds-cursor.el --- Cursor related definitions


;; Goes to LINE if it exist.
;;
;; Example:
;;   When I go to line "12"
(When "^I go to line \"\\([0-9]+\\)\"$"
      (lambda (line)
        (let ((num-lines (count-lines (point-min) (point-max)))
              (line-num (string-to-number line))
              (message "Requested line '%s', but buffer only has '%d' line(s)."))
          (assert (<= line-num num-lines) nil message line num-lines)
          (goto-line line-num))))

;; Goes to POINT if it exist.
;;
;; Example:
;;   When I go to point "12"
(When "^I go to point \"\\([0-9]+\\)\"$"
      (lambda (point)
        (let ((size (buffer-size))
              (point-num (string-to-number point))
              (message "Requested point '%s', but buffer only has '%d' point(s)."))
          (assert (<= (1- point-num) size) nil message point-num size)
          (goto-char point-num))))

;; Go to WORD if it exist.
;;
;; Example:
;;   When I go to word "SOME WORD"
(When "^I go to word \"\\(.+\\)\"$"
      (lambda (word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "\\b%s\\b" word) nil t))
              (message "Can not go to word '%s' since it does not exist in the current buffer: %s"))
          (assert search nil message word (espuds-buffer-contents)))
        (backward-char (length word))))

;; Checks that the cursor is at a specific position.
;;
;; Example:
;;   Then the cursor should be at point "12"
(Then "^the cursor should be at point \"\\(.+\\)\"$"
      (lambda (point)
        (let ((message "Expected cursor to be at point '%s', but was at '%s'"))
          (assert (= (string-to-number point) (point)) nil message point (point)))))

;; Checks that the cursor is before some text.
;;
;; Example:
;;   Then the cursor should be before "Foo"
(Then "^the cursor should be before \"\\(.+\\)\"$"
      (lambda (expected)
        (let ((actual
               (progn
                 (buffer-substring-no-properties (point) (min (point-max) (+ (point) 5)))))
              (message "Expected '%s' to be before point but was '%s'."))
          (assert (looking-at (regexp-quote expected)) nil message expected actual))))

;; Checks that the cursor is after some text.
;;
;; Example:
;;   Then the cursor should be after "Foo"
(Then "^the cursor should be after \"\\(.+\\)\"$"
      (lambda (expected)
        (let ((actual
               (progn
                 (buffer-substring-no-properties (point) (max (point-min) (- (point) 5)))))
              (message "Expected '%s' to be after point but was '%s'."))
          (assert (looking-back (regexp-quote expected)) nil message expected actual))))

;; Checks that the cursor is between some text.
;;
;; Example:
;;   Then the cursor should be between "Foo" and "Bar"
(Then "^the cursor should be between \"\\(.+\\)\" and \"\\(.+\\)\"$"
      (lambda (left right)
        (let ((search
               (and
                (looking-back (regexp-quote left))
                (looking-at (regexp-quote right))))
              (message "Expected '%s' to be left of point and '%s' to be right of point, but was: '%s[CURSOR]%s'")
              (before
               (buffer-substring-no-properties
                (max (point-min) (- (point) 5))
                (point)))
              (after
               (buffer-substring-no-properties
                (point)
                (min (point-max) (+ (point) 5)))))
          (assert search nil message left right before after))))

;; Places the cursor between text.
;;
;; Example:
;;   When I place the cursor between "Foo" and "Bar"
(When "^I place the cursor between \"\\(.+\\)\" and \"\\(.+\\)\"$"
      (lambda (left right)
        (goto-char (point-min))
        (let ((search (search-forward (concat left right) nil t))
              (message "Can not place cursor between '%s' and '%s', because there is no such point: '%s'"))
          (assert search nil message left right (espuds-buffer-contents)))
        (backward-char (length right))))

;; Places the cursor before first instance of text.
;;
;; Example:
;;   When I place the cursor before "Foo"
(When "^I place the cursor before \"\\(.+\\)\"$"
      (lambda (arg)
        (goto-char (point-min))
        (let ((search (search-forward arg nil t))
              (message "Can not place cursor before '%s', because there is no such point: '%s'"))
          (backward-char (length arg))
          (assert search nil message arg (espuds-buffer-contents)))))

;; Places the cursor after first instance of text.
;;
;; Example:
;;   When I place the cursor after "Foo"
(When "^I place the cursor after \"\\(.+\\)\"$"
      (lambda (arg)
        (goto-char (point-min))
        (let ((search (search-forward arg nil t))
              (message "Can not place cursor after '%s', because there is no such point: '%s'"))
          (assert search nil message arg (espuds-buffer-contents)))))

;; Places the cursor at the beginning of buffer.
;;
;; Example:
;;   When I go to beginning of buffer
(When "^I go to beginning of buffer$" 'beginning-of-buffer)

;; Places the cursor at the end of buffer.
;;
;; Example:
;;   When I go to end of buffer
(When "^I go to end of buffer$" 'end-of-buffer)

;; Places the cursor at the beginning of the line.
;;
;; Example:
;;   When I go to beginning of line
(When "^I go to beginning of line$" 'move-beginning-of-line)

;; Places the cursor at the end of the line.
;;
;; Example:
;;   When I go to end of line
(When "^I go to end of line$" 'move-end-of-line)


(provide 'espuds-cursor)

;;; espuds-cursor.el ends here
