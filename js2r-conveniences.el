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

(defun js--current-line-is-prefixed-with-list-item-start ()
  (save-excursion
    (back-to-indentation)
    (looking-back "\\({\\|\\[\\|,\\)\\(\s\\|\n\\)*"))) ; { or [ or , then space

(defun js--current-line-is-postfixed-with-list-item-end ()
  (save-excursion
    (end-of-line)
    (or (looking-back ",\s*") ; line ends in comma
        (looking-at "\\(\s\\|\n\\)*\\(\\]\\|}\\)")))) ; space then ] or }

(defun js--current-line-is-a-list-item ()
  (and (js--current-line-is-prefixed-with-list-item-start)
       (js--current-line-is-postfixed-with-list-item-end)))

(defun js--forward-line-is-a-list-item ()
  (save-excursion
    (forward-line)
    (js--current-line-is-a-list-item)))

(defun js--previous-line-is-a-list-item ()
  (save-excursion
    (forward-line -1)
    (js--current-line-is-a-list-item)))

(defun js--current-line-has-comma ()
  (save-excursion
    (end-of-line)
    (looking-back ",\s*")))

(defun js--previous-line-has-comma ()
  (save-excursion
    (forward-line -1)
    (js--current-line-has-comma)))

(defun js--move-line-down-as-list-item ()
  (move-line-down)
  (if (not (js--previous-line-has-comma))
      (save-excursion
        (end-of-line)
        (delete-char -1)
        (forward-line -1)
        (end-of-line)
        (insert ","))))

(defun js--move-line-up-as-list-item ()
  (move-line-up)
  (if (not (js--current-line-has-comma))
      (save-excursion
        (end-of-line)
        (insert ",")
        (forward-line)
        (end-of-line)
        (delete-char -1))))

(defun js-move-line-down ()
  (interactive)
  (if (and (js--current-line-is-a-list-item)
           (js--forward-line-is-a-list-item))
      (js--move-line-down-as-list-item)
    (move-line-down)))

(defun js-move-line-up ()
  (interactive)
  (if (and (js--current-line-is-a-list-item)
           (js--previous-line-is-a-list-item))
      (js--move-line-up-as-list-item)
    (move-line-up)))

(provide 'js2r-conveniences)
