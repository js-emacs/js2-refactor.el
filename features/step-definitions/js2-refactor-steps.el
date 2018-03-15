(require 'cl-lib)

(And "^I turn on js2-mode and js2-refactor-mode$"
     (lambda ()
       (js2-mode)
       (js2-refactor-mode)
       (js2-parse)))

(And "^The buffer is parsed$"
     (lambda ()
       (js2-parse)))

(And "^delete-selection-mode is active$"
     (lambda ()
        (delete-selection-mode 1)))

(When "^I go to character \"\\(.+\\)\"$"
      (lambda (char)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" char) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message char (espuds-buffer-contents)))))

(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
      (lambda (pos word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" word) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (cl-assert search nil message word (espuds-buffer-contents))
          (if (string-equal "front" pos) (backward-word)))))

(When "^I set logging to be before the use-site$"
      (lambda ()
        (setq-local js2r-log-before-point t)))
