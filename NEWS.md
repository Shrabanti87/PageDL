# PageDL 0.0.4

- Renamed the R package from CASPEN to PageDL. Load it with `library(PageDL)`.
  Existing function and bundled data names are unchanged.
- Pathway selection and pathway- and gene-level performance functions suppress
  model warnings by default. Use `suppress.model.warnings = FALSE` to show
  warnings from model tuning, fitting, and scoring.

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
