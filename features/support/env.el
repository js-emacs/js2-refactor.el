(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq js2-refactor-root-path project-directory)
  (setq js2-refactor-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path js2-refactor-root-path)
(add-to-list 'load-path js2-refactor-util-path)
(add-to-list 'load-path (expand-file-name "espuds" js2-refactor-util-path))
(add-to-list 'load-path (expand-file-name "vendor" js2-refactor-util-path))

(require 'bang)
(require 'mark-multiple)
(require 'js2-refactor)
(require 'espuds)
(require 'ert)

(Before
 (mm/clear-all)
 (switch-to-buffer
  (get-buffer-create "*js2-refactor*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (cua-mode 0)
 (delete-selection-mode 0)
 (set-default 'indent-tabs-mode nil)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))

(After)
