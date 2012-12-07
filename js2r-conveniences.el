;; Split a string

(defun js2r-split-string ()
  (interactive)
  (when (js2r--point-inside-string-p)
    (if (looking-back " \"")
        (progn
          (forward-char -2)
          (insert "  +")
          (forward-char -2))
      (if (looking-at (regexp-quote "\" + \""))
          (delete-char 5)
        (insert "\" + \"")))))

;; Make sure commas are placed correctly when moving a line up or down
;; in an object or array literal.

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun js2r--current-line-is-prefixed-with-list-item-start ()
  (save-excursion
    (back-to-indentation)
    (looking-back "\\({\\|\\[\\|,\\)\\(\s\\|\n\\)*"))) ; { or [ or , then space

(defun js2r--current-line-is-postfixed-with-list-item-end ()
  (save-excursion
    (end-of-line)
    (or (looking-back ",\s*") ; line ends in comma
        (looking-at "\\(\s\\|\n\\)*\\(\\]\\|}\\)")))) ; space then ] or }

(defun js2r--current-line-is-a-list-item ()
  (and (js2r--current-line-is-prefixed-with-list-item-start)
       (js2r--current-line-is-postfixed-with-list-item-end)))

(defun js2r--next-line-is-a-list-item ()
  (save-excursion
    (forward-line)
    (js2r--current-line-is-a-list-item)))

(defun js2r--previous-line-is-a-list-item ()
  (save-excursion
    (forward-line -1)
    (js2r--current-line-is-a-list-item)))

(defun js2r--current-line-has-comma ()
  (save-excursion
    (end-of-line)
    (looking-back ",\s*")))

(defun js2r--previous-line-has-comma ()
  (save-excursion
    (forward-line -1)
    (js2r--current-line-has-comma)))

(defun js2r--move-line-down-as-list-item ()
  (move-line-down)
  (if (not (js2r--previous-line-has-comma))
      (save-excursion
        (end-of-line)
        (delete-char -1)
        (forward-line -1)
        (end-of-line)
        (insert ","))))

(defun js2r--move-line-up-as-list-item ()
  (move-line-up)
  (if (not (js2r--current-line-has-comma))
      (save-excursion
        (end-of-line)
        (insert ",")
        (forward-line)
        (end-of-line)
        (delete-char -1))))

(defun js2r-move-line-down ()
  (interactive)
  (if (and (js2r--current-line-is-a-list-item)
           (js2r--next-line-is-a-list-item))
      (js2r--move-line-down-as-list-item)
    (move-line-down)))

(defun js2r-move-line-up ()
  (interactive)
  (if (and (js2r--current-line-is-a-list-item)
           (js2r--previous-line-is-a-list-item))
      (js2r--move-line-up-as-list-item)
    (move-line-up)))

(provide 'js2r-conveniences)
