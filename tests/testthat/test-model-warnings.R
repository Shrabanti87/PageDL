test_that("performance functions suppress model warnings unless requested", {
  fit <- function(...) {
    message("Fitting model")
    warning("Model diagnostic", call. = FALSE)
    data.frame(model = "Simple", auc = 0.75, sp95 = 0.25)
  }
  local_mocked_bindings(binary_train_performance = fit, binary_test_performance = fit)

  x <- data.frame(G1 = seq_len(6))
  args <- list(outcome.type = "binary", outcome = rep(0:1, 3),
               train_data = x, models.indiv = "DCT")
  calls <- list(
    list(Train_perform_CV, list(features = list(p = "G1"), iter = 1)),
    list(Test_perform, list(features = list(p = "G1"), test_data = x,
                            test_outcome = args$outcome)),
    list(Gene_Train_perform_CV, list(genes = "G1", iter = 1)),
    list(Gene_Test_perform, list(genes = "G1", test_data = x,
                                 test_outcome = args$outcome))
  )

  for (call in calls) {
    fit_args <- c(args, call[[2]])
    expect_no_warning(default <- suppressMessages(do.call(call[[1]], fit_args)))
    expect_message(do.call(call[[1]], fit_args), "Fitting model")
    expect_warning(
      shown <- suppressMessages(do.call(call[[1]], c(fit_args, suppress.model.warnings = FALSE))),
      "Model diagnostic"
    )
    expect_identical(default, shown)
  }
})

test_that("pathway selection suppresses model warnings during scoring", {
  score <- binary_roc
  local_mocked_bindings(binary_roc = function(...) {
    warning("Scoring diagnostic", call. = FALSE)
    score(...)
  })

  y <- rep(0:1, each = 30)
  x <- data.frame(G1 = seq_along(y), G2 = sin(seq_along(y)))
  args <- list(outcome.type = "binary", outcome = y, data = x,
               features = list(p = names(x)), models.indiv = "DCT",
               iter = 1, num.folds = 3)
  expect_no_warning(default <- suppressMessages(do.call(pathway_select, args)))
  expect_warning(
    shown <- suppressMessages(do.call(pathway_select,
                                      c(args, suppress.model.warnings = FALSE))),
    "Scoring diagnostic"
  )
  expect_identical(default$auc, shown$auc)
})
