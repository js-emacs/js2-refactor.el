;;; espuds-text.el --- Text related definitions

;; Inserts CONTENTS into the current buffer.
;;
;; Example:
;;   When I insert "CONTENTS"
;;
;;   When I insert:
;;     """
;;     CONTENTS
;;     """
(When "^I insert\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (contents)
        (insert contents)))


;; Asserts that the current buffer includes some text.
;;
;; Example:
;;   Then I should see "CONTENTS"
;;
;;   Then I should see:
;;   """
;;   CONTENTS
;;   """
(Then "^I should see\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents))
              (message "Expected '%s' to be part of '%s', but was not."))
          (assert (search expected actual) nil message expected actual))))

;; Asserts that the current buffer does not include some text.
;;
;; Example:
;;   Then I should not see "CONTENTS"
;;
;;   Then I should not see:
;;   """
;;   CONTENTS
;;   """
(Then "^I should not see\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents))
              (message "Expected '%s' to not be part of '%s', but was."))
          (assert (not (search expected actual)) nil message expected actual))))


;; Asserts that the current buffer matches some text.
;;
;; Example:
;;   Then I should see pattern "CONTENTS"
;;
;;   Then I should see pattern:
;;   """
;;   CONTENTS
;;   """
(Then "^I should see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents))
              (message "Expected to see pattern '%s' in '%s', but did not."))
          (assert
           (string-match-p expected actual) nil message expected actual))))

;; Asserts that the current buffer does not match some text.
;;
;; Example:
;;   Then I should not see pattern "CONTENTS"
;;
;;   Then I should not see pattern:
;;   """
;;   CONTENTS
;;   """
(Then "^I should not see pattern\\(?: \"\\(.+\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-buffer-contents))
              (message "Expected to not see pattern '%s' in '%s', but did."))
          (assert
           (not (string-match-p expected actual)) nil message expected actual))))

;; Selects TEXT if found. Otherwise signal an error.
;;
;; Example:
;;   When I select "SOME TEXT"
;;
(When "^I select \"\\(.+\\)\"$"
      (lambda (text)
        (goto-char (point-min))
        (let ((search (re-search-forward text nil t)))
          (assert search nil "The text '%s' was not found in the current buffer." text))
        (set-mark (point))
        (re-search-backward text)))

;; Asserts that there nothing to see in the current buffer.
;;
;; Example:
;;   Then I should not see anything
;;   Then the buffer should be empty
(Then "^I should not see anything$\\|^the buffer should be empty$"
      (lambda ()
        (let ((message "Expected buffer to be empty, but had content: '%s'"))
          (assert (equal (buffer-size) 0) nil message (espuds-buffer-contents)))))


(provide 'espuds-text)

;;; espuds-text.el ends here
