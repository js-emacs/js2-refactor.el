;;; espuds-buffer.el --- Buffer related definitions


;; Switches to BUFFER.
;;
;; Example:
;;   When I switch to buffer "Foo"
;;   Given I am in buffer "*scratch*"
(Given "^\\(?:I am in buffer\\|I switch to buffer\\) \"\\(.+\\)\"$"
       (lambda (buffer)
         (let ((v (vconcat [?\C-x ?b] (string-to-vector buffer))))
           (execute-kbd-macro v))))

;; Asserts that the current buffer is BUFFER.
;;
;; Example:
;;   Then I should be in buffer "*scratch*"
(Then "^I should be in buffer \"\\(.+\\)\"$"
      (lambda (buffer)
        (let ((message "Expected to be in buffer '%s', but was in '%s'"))
          (assert (equal buffer (buffer-name)) nil message buffer (buffer-name)))))


;; Asserts that the current buffer is connected to FILE.
;;
;; Example:
;;   Then I should be in file "/path/to/some/file"
(Then "^I should be in file \"\\(.+\\)\"$"
      (lambda (file)
        (let ((file-name (buffer-file-name)))
          (if file-name
              (let ((match (equal file (file-name-nondirectory file-name))))
                (assert match nil "Expected file to be '%s', but was '%s'." file file-name))
            (assert file-name nil "Expected file to be '%s', but not visiting any file." file)))))

;; Clears all text in the current buffer.
;;
;; Example:
;;   Given the buffer is empty
;;   When I clear the buffer
(Given "^the buffer is empty$\\|^I clear the buffer$"
       (lambda ()
         (erase-buffer)))


(provide 'espuds-buffer)

;;; espuds-buffer.el ends here
