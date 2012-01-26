;; This extension is dependent on js2-mode and the mark-multiple library:
;;
;;     https://github.com/mooz/js2-mode
;;     https://github.com/magnars/mark-multiple.el
;;

(require 'js2-mode)

;;; Formatting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-formatting)

;; Expand and contract object
(define-key js2-mode-map (kbd "C-c RET eo") 'js-expand-object)
(define-key js2-mode-map (kbd "C-c RET co") 'js-contract-object)



;;; Immediately invoked function expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-iife)

;; Create shortcut for marked global by injecting it in the wrapping IIFE
(define-key js2-mode-map (kbd "C-c RET ig") 'js-inject-global-in-iife)



;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-vars)

;; Extract
(define-key js2-mode-map (kbd "C-c RET ev") 'js-extract-variable)

;; Rename
(define-key js2-mode-map (kbd "C-c RET rv") 'js2-rename-var)



;;; Conveniences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2r-conveniences)

;; Make sure commas are placed correctly when moving a line in a literal
(define-key js2-mode-map (kbd "<C-S-down>") 'js-move-line-down)
(define-key js2-mode-map (kbd "<C-S-up>") 'js-move-line-up)



(provide 'js2-refactor)
