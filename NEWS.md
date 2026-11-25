# CASPEN 0.0.3

- Aligns simple-ensemble out-of-fold predictions with original sample order
  for binary, survival, and multiclass training CV.
- Preserves survival risk scores when computing the simple ensemble and retains
  class probability columns in multiclass independent testing.
- Uses the requested base learners for the multiclass prediction matrix,
  including when no second-stage learner is requested.

# CASPEN 0.0.2

- Consolidates continuous-outcome, gene-level performance, automatic tuning,
  cell-type specificity, and multiclass robustness updates.
- Ensures binary elastic-net predictions receive numeric matrices with current
  `glmnet` releases.
- Ensures pathway-level binary AdaBoost receives matrices required by
  `JOUSBoost`.
