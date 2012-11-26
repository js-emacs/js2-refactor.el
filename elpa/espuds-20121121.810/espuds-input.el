;;; espuds-input.el --- Input related definitions


(defvar espuds-action-chain nil
  "List of actions to execute.")

(defvar espuds-chain-active nil
  "Is t if chaining is active, nil otherwise.")

(defvar espuds-previous-keyboard-input nil
  "Previous input command (keybinding).")


;; Starts an action chain.
;;
;; Example:
;;   When I start an action chain
(When "^I start an action chain$"
      (lambda ()
        (setq espuds-action-chain nil)
        (setq espuds-chain-active t)))

;; Executes the action chain.
;;
;; Example:
;;   When I execute the action chain
(When "^I execute the action chain$"
      (lambda ()
        (execute-kbd-macro espuds-action-chain)
        (setq espuds-chain-active nil)))

;; If action chaining is active. Add KEYBINDING to the action
;; chain. Otherwise execute the function that KEYBINDING is bound to.
;;
;; Example:
;;   When I press "C-h e"
(When "^I press \"\\(.+\\)\"$"
      (lambda (keybinding)
        (when (and
               (equal espuds-previous-keyboard-input "C-y")
               (equal keybinding "M-y")
               (eq (key-binding (kbd "M-y")) 'yank-pop))
          (setq this-command 'yank))
        (let ((macro (edmacro-parse-keys keybinding)))
          (if espuds-chain-active
              (setq espuds-action-chain (vconcat espuds-action-chain macro))
            (if (and (equal keybinding "C-g")
                     (eq (key-binding (kbd "C-g")) 'keyboard-quit))
                (espuds-quit)
              (execute-kbd-macro macro))))
        (setq espuds-previous-keyboard-input keybinding)))

;; Quit without signal.
;;
;; Example:
;;   When I quit
(When "^I quit$" 'espuds-quit)

;; If action chaining is active. Add TYPING to the action
;; chain. Otherwise simulate the TYPING.
;;
;; Example:
;;   When I type "TYPING"
(When "^I type \"\\(.+\\)\"$"
      (lambda (typing)
        (if espuds-chain-active
            (setq espuds-action-chain (vconcat espuds-action-chain (string-to-vector typing)))
          (execute-kbd-macro (string-to-vector typing)))))


(provide 'espuds-input)

;;; espuds-input.el ends here
