;; Make sure commas are placed correctly when moving a line up or down
;; in an object or array literal.

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

(defun js--next-line-is-a-list-item ()
  (save-excursion
    (next-line)
    (js--current-line-is-a-list-item)))

(defun js--previous-line-is-a-list-item ()
  (save-excursion
    (previous-line)
    (js--current-line-is-a-list-item)))

(defun js--current-line-has-comma ()
  (save-excursion
    (end-of-line)
    (looking-back ",\s*")))

(defun js--previous-line-has-comma ()
  (save-excursion
    (previous-line)
    (js--current-line-has-comma)))

(defun js--move-line-down-as-list-item ()
  (move-line-down)
  (if (not (js--previous-line-has-comma))
      (save-excursion
        (end-of-line)
        (delete-char -1)
        (previous-line)
        (end-of-line)
        (insert ","))))

(defun js--move-line-up-as-list-item ()
  (move-line-up)
  (if (not (js--current-line-has-comma))
      (save-excursion
        (end-of-line)
        (insert ",")
        (next-line)
        (end-of-line)
        (delete-char -1))))

(defun js-move-line-down ()
  (interactive)
  (if (and (js--current-line-is-a-list-item)
           (js--next-line-is-a-list-item))
      (js--move-line-down-as-list-item)
    (move-line-down)))

(defun js-move-line-up ()
  (interactive)
  (if (and (js--current-line-is-a-list-item)
           (js--previous-line-is-a-list-item))
      (js--move-line-up-as-list-item)
    (move-line-up)))

(provide 'js2r-conveniences)
