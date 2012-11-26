;;; espuds-misc.el --- Definitions that don't fit in any other file

;; Turns on some mode.
;;
;; Example:
;;   When I turn on ruby-mode
(When "^I turn on \\(.+\\)$"
      (lambda (mode)
        (let ((v (vconcat [?\C-u 1 ?\M-x] (string-to-vector mode))))
          (execute-kbd-macro v))))

;; Set some variable
;;
;; Example:
;;   When I set sentence-end-double-space to nil
(When "^I set \\(.+\\) to \\(.+\\)$"
      (lambda (var val)
        (set (intern var) (read val))))

;; Loads CONTENTS with Emacs load command.
;;
;; Example:
;;   When I load the following:
;;   """
;;   CONTENTS
;;   """
(When "^I load the following:$"
      (lambda (contents)
        (espuds-fake-eval contents)))

;; Creates a new temp file called FILE and opens it.
;;
;; Example:
;;   When I open temp file "SOME FILE"
(When "^I open temp file \"\\(.+\\)\"$"
      (lambda (file)
        (find-file (make-temp-file file))))

;; Asserts that MESSAGE has been printed.
;;
;; Example:
;;   Then I should see message "MESSAGE"
(Then "^I should see message \"\\(.+\\)\"$"
      (lambda (message)
        (let ((msg "Expected '%s' to be included in the list of printed messages, but was not."))
          (assert (equal (car (last ecukes-message-log)) message) nil msg message))))


(provide 'espuds-misc)

;;; espuds-misc.el ends here
