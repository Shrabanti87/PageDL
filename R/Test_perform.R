#' Evaluate model performance on testing data
#'
#' This function evaluates the predictive performance of trained models
#' on an independent testing dataset using a fixed parameter configuration
#' (no cross-validation). Performance metrics are computed for each
#' specified base model and, if enabled, the ensemble model. The results
#' provide an unbiased assessment of out-of-sample generalization.
#' @param outcome.type Character string specifying the prediction task. Must be
#' one of `"survival"`, `"binary"`, `"categorical"`, or `"continuous"`.
#' @param outcome Outcome vector for the training data. For `"binary"` and `"survival"` outcomes, this should be a binary vector (0/1). For `"categorical"` outcomes, this should be a numeric or factor vector representing class labels.
#' @param survdays Numeric vector of survival times for the training data, required when `outcome.type = "survival"`.
#' @param time_point Numeric value specifying the time point at which survival predictions are evaluated.
#' @param train_data training data
#' @param test_data testing data
#' @param test_outcome Outcome vector for testing data. For `"binary"` and `"survival"` outcomes, this should be a binary vector (0/1). For `"categorical"` outcomes, this should be a numeric or factor vector representing class labels.
#' @param test_time Survival times for testing data, required when `outcome.type = "survival"`.
#' @param features Feature specification. For pathway-based modeling this should be a list of pathways where each element is a character vector of genes belonging to that pathway. For protein-level modeling this should be a character vector of proteins used as features.
#' @param models.indiv Character vector specifying base machine learning or deep learning models used for individual model training. Supported abbreviations include: `"XG"` (xgboost), `"RF"` (random forest), `"EN"` (elastic net), `"GB"` (gradient boosting), `"KNN"` (k-nearest neighbors), `"SVM"` (support vector machine), `"NB"` (naive Bayes), `"DCT"` (decision tree), `"NN"` (neural network), `"ADB"` (AdaBoost), and `"MB"` (model-based boosting).
#' @param ensemble Logical. If `TRUE`, ensemble (stacking) prediction modeling is performed.
#' @param models.ens Character vector specifying models used to construct the stacking/ensemble model. Model abbreviations follow the same convention as `models.indiv`.
#' @param param.indiv Named list of user-specified parameters for the individual base models listed in `models.indiv`. If `NULL`, default parameters appropriate for the specified outcome type are used.
#' @param param.ens Named list of user-specified parameters for ensemble model training. If `NULL`, default parameters are used.
#' @param auto.tune Logical; if `TRUE`, tune supported base learners using only
#' the training data before fitting the independent-test models.
#' @param tune.method Candidate-search strategy. `"random"` is the recommended
#' fast option and `"grid"` evaluates a deterministic grid. The accepted
#' `"successive_halving"`, `"hyperband"`, and `"bayes"` values currently use
#' PageDL's random-search backend.
#' @param tune.n Positive integer number of candidate parameter settings sampled
#' per tuned model.
#' @param tune.folds Integer number of folds used by the internal tuning CV.
#' @param tune.iter Integer number of repeated internal tuning evaluations.
#' @param tune.models Optional character vector restricting tuning to a subset
#' of `models.indiv`. `NULL` considers every supported requested model.
#' @param suppress.model.warnings Logical. If `TRUE` (default), suppress
#' warnings from tuning, fitting, and scoring models. Use `FALSE` to show
#' warnings emitted by the model workflow. Messages and errors remain visible.
#'
#' @examples
#' \dontrun{
#' # -----------------------------------------------------------------------
#' # Independent test-set evaluation
#' # -----------------------------------------------------------------------
#' data(pagedl_example)
#'
#' data(pagedl_test_example)
#'
#' train_x <- pagedl_example$data_to_pass
#' train_y <- pagedl_example$outcome
#' test_x <- pagedl_test_example$test_data
#' test_y <- pagedl_test_example$test_outcome
#'
#' path_res <- pathway_select(
#'   outcome.type = "binary",
#'   outcome = train_y,
#'   data = train_x,
#'   features = pagedl_example$pathways[1:10],
#'   models.indiv = c("RF", "EN", "SVM"),
#'   iter = 5,
#'   num.folds = 3,
#'   fs.AUC.cut = 0.70,
#'   fs.sp.cut = 0.10
#' )
#'
#' selected_features <- path_res$features
#'
#' test_res <- Test_perform(
#'   outcome.type = "binary",
#'   outcome = train_y,
#'   train_data = train_x,
#'   test_data = test_x,
#'   test_outcome = test_y,
#'   features = selected_features,
#'   models.indiv = c("RF", "EN", "SVM"),
#'   ensemble = TRUE,
#'   models.ens = c("RF", "EN", "SVM")
#' )
#'
#' test_res[, c("model", "auc", "sp95")]
#'
#' # For survival, also supply survival time and time_point.
#' set.seed(2)
#' survival_time <- sample(120:1800, nrow(pagedl_example$data_to_pass),
#'                         replace = TRUE)
#' survival_status <- pagedl_example$outcome
#' test_survival_time <- sample(120:1800, nrow(pagedl_test_example$test_data),
#'                              replace = TRUE)
#'
#' surv_test_res <- Test_perform(
#'   outcome.type = "survival",
#'   outcome = survival_status,
#'   survdays = survival_time,
#'   time_point = 365,
#'   train_data = train_x,
#'   test_data = test_x,
#'   test_outcome = test_y,
#'   test_time = test_survival_time,
#'   features = selected_features,
#'   models.indiv = c("RF", "EN", "DCT"),
#'   ensemble = TRUE,
#'   models.ens = c("RF", "EN")
#' )
#'
#' surv_test_res[, c("model", "cindex", "auc", "sp95")]
#' }
#'
#' @export

Test_perform <- function(outcome.type, outcome, survdays = NULL, time_point = NULL,
                         train_data, test_data, test_outcome, test_time = NULL,
                         features, models.indiv, ensemble = FALSE, models.ens = NULL,
                         param.indiv = NULL, param.ens = NULL,
                         auto.tune = FALSE,
                         tune.method = c("random", "grid", "successive_halving",
                                         "hyperband", "bayes"),
                         tune.n = 5,
                         tune.folds = 3,
                         tune.iter = 1,
                         tune.models = NULL,
                         suppress.model.warnings = TRUE) {
  type_out <- tolower(outcome.type)
  tune.method <- match.arg(tune.method)
  if (!type_out %in% c("binary", "categorical", "survival", "continuous")) {
    stop("Invalid outcome.type: Must be 'binary', 'categorical', 'survival', or 'continuous'.")
  }
  run_fit <- if (isTRUE(suppress.model.warnings)) suppressWarnings else identity
  tuning.table <- NULL
  if (isTRUE(auto.tune)) {
    tune.genes <- unique(unlist(lapply(features, extract_pathway_genes)))
    tune.genes <- tune.genes[tune.genes %in% colnames(train_data)]
    if (length(tune.genes)) {
      tuned <- run_fit(pagedl_auto_tune_params(
        outcome.type = type_out,
        outcome = outcome,
        x = train_data[, tune.genes, drop = FALSE],
        models = models.indiv,
        param.indiv = param.indiv,
        tune.method = tune.method,
        tune.n = tune.n,
        tune.folds = tune.folds,
        tune.iter = tune.iter,
        tune.models = tune.models,
        survdays = survdays,
        time_point = time_point,
        seed = 1
      ))
      param.indiv <- tuned$param.indiv
      tuning.table <- tuned$tuning.table
    }
  }
  if (type_out == "binary") {
    res <- run_fit(binary_test_performance(
      outcome      = outcome,
      train_data   = train_data,
      test_data    = test_data,
      test_outcome = test_outcome,
      features     = features,
      models.indiv = models.indiv,
      ensemble     = ensemble,
      models.ens   = models.ens,
      param.indiv  = param.indiv,
      param.ens    = param.ens
    ))
  } else if (type_out == "survival") {
    if (is.null(survdays) || is.null(test_time) || is.null(time_point)) {
      stop("survdays, test_time, and time_point are required for survival outcomes.")
    }
    res <- run_fit(surv_test_performance(
      xtrain_time  = survdays,
      outcome      = outcome,
      xtest_time   = test_time,
      xtest_status = test_outcome,
      time_point   = time_point,
      train_data   = train_data,
      test_data    = test_data,
      features     = features,
      models.indiv = models.indiv,
      ensemble     = ensemble,
      models.ens   = models.ens,
      param.indiv  = param.indiv,
      param.ens    = param.ens
    ))
  } else if (type_out == "categorical") {
    res <- run_fit(cat_test_performance(
      outcome      = outcome,
      train_data   = train_data,
      test_data    = test_data,
      test_outcome = test_outcome,
      features     = features,
      models.indiv = models.indiv,
      ensemble     = ensemble,
      models.ens   = models.ens,
      param.indiv  = param.indiv,
      param.ens    = param.ens
    ))
  } else if (type_out == "continuous") {
    res <- run_fit(continuous_test_performance(
      outcome      = outcome,
      train_data   = train_data,
      test_data    = test_data,
      test_outcome = test_outcome,
      features     = features,
      models.indiv = models.indiv,
      ensemble     = ensemble,
      models.ens   = models.ens,
      param.indiv  = param.indiv,
      param.ens    = param.ens
    ))
  }

  if (isTRUE(auto.tune)) attr(res, "tuning") <- tuning.table
  return(res)
}
