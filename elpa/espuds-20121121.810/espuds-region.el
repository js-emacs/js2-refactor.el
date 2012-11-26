;;; espuds-region.el --- region related definitions


;; Deactivates mark.
;;
;; Example:
;;   Given there is no region selected
(Given "^there is no region selected$"
       (lambda ()
         (deactivate-mark)))

;; Activates transient mark mode.
;;
;; Example:
;;   Given transient mark mode is active
;;   Given transient mark mode is inactive
(Given "^transient mark mode is \\(active\\|inactive\\)$"
       (lambda (status)
         (transient-mark-mode
          (if (string= status "active") 1 -1))))

;; Sets the mark at point.
;;
;; Example:
;;   When I set the mark
(When "^I set the mark$"
      (lambda ()
        (set-mark (point))))

;; Pop and move point to the top position on the mark-ring
;;
;; Example:
;;   When I pop the mark
(When "^I pop the mark$"
      (lambda ()
        (set-mark-command 4)))

;; Asserts that the selected region is same as EXPECTED.
;;
;; Example:
;;   Then the region should be "REGION"
;;
;;   Then the region should be:
;;   """
;;   REGION
;;   """
(Then "^the region should be\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (let ((actual (espuds-region))
              (message "Expected the region to be '%s', but was '%s'."))
          (assert (equal expected actual) nil message expected actual))))

;; Asserts that the region is not active.
;;
;; Example:
;;   Then the region should not be active
(Then "^the region should not be active$"
      (lambda ()
        (let ((message "Expected the region not to be active, but it was."))
          (assert (not (region-active-p)) nil message))))

(provide 'espuds-region)

;;; espuds-region.el ends here
