# PageDL 0.0.5

- Renames the model and demo helpers to `pagedl_model_info()` and
  `launch_pagedl_demo()`. The bundled datasets are `pagedl_example` and
  `pagedl_test_example`.
- Uses `PAGEDL_OPENAI_MODEL` to configure the optional default OpenAI model.
  The `OPENAI_API_KEY` setting is unchanged.

# PageDL 0.0.4

- Changed the R package name to PageDL. Load it with `library(PageDL)`.
- Pathway selection and pathway- and gene-level performance functions suppress
  model warnings by default. Use `suppress.model.warnings = FALSE` to show
  warnings from model tuning, fitting, and scoring.

# 0.0.3

- Aligns simple-ensemble out-of-fold predictions with original sample order
  for binary, survival, and multiclass training CV.
- Preserves survival risk scores when computing the simple ensemble and retains
  class probability columns in multiclass independent testing.
- Uses the requested base learners for the multiclass prediction matrix,
  including when no second-stage learner is requested.

# 0.0.2

- Consolidates continuous-outcome, gene-level performance, automatic tuning,
  cell-type specificity, and multiclass robustness updates.
- Ensures binary elastic-net predictions receive numeric matrices with current
  `glmnet` releases.
- Ensures pathway-level binary AdaBoost receives matrices required by
  `JOUSBoost`.
