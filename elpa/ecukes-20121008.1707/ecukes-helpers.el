;;; ecukes-helpers.el --- Misc helpers

(defun ecukes-feature-steps (features)
  "Return all steps in all FEATURES."
  (let* ((scenarios
          (-flatten (--map (ecukes-feature-scenarios it) features)))
         (backgrounds
          (-reject 'null (--map (ecukes-feature-background it) features)))
         (scenario-steps
          (--map (ecukes-scenario-steps it) scenarios))
         (background-steps
          (--map (ecukes-background-steps it) backgrounds)))
    (-flatten (-concat background-steps scenario-steps))))

(provide 'ecukes-helpers)

;;; ecukes-helpers.el ends here
