make_binary_example <- function() {
  set.seed(42)
  n <- 72L
  x <- matrix(rnorm(n * 8L), nrow = n)
  colnames(x) <- paste0("G", seq_len(ncol(x)))
  y <- rep(c(0L, 1L), each = n / 2L)
  x[y == 1L, 1:2] <- x[y == 1L, 1:2] + 0.4

  list(
    x = as.data.frame(x),
    y = y,
    pathways = list(signal = colnames(x)[1:4])
  )
}

test_that("binary elastic net accepts pathway data frames", {
  example <- make_binary_example()

  result <- Train_perform_CV(
    outcome.type = "binary",
    outcome = example$y,
    train_data = example$x,
    features = example$pathways,
    models.indiv = "EN",
    iter = 1,
    num.folds = 2,
    param.indiv = list(EN = list(alpha = 0.2, nfolds = 3, s = "lambda.1se"))
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(any(is.finite(result$auc)))
})

test_that("binary AdaBoost receives matrix inputs", {
  example <- make_binary_example()

  result <- Train_perform_CV(
    outcome.type = "binary",
    outcome = example$y,
    train_data = example$x,
    features = example$pathways,
    models.indiv = "ADB",
    iter = 1,
    num.folds = 2,
    param.indiv = list(ADB = list(tree_depth = 1, n_rounds = 5, verbose = FALSE))
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
  expect_true(any(is.finite(result$auc)))
})
