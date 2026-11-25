test_that("binary Simple scores predictions in sample order across repeats", {
  y <- rep(0:1, each = 36)
  set.seed(11)
  x <- data.frame(G1 = rnorm(length(y)) + y, G2 = rnorm(length(y)))
  predictions <- sapply(1:2, function(iteration) {
    set.seed(iteration)
    folds <- balanced.folds.vec(y, 3)
    scores <- numeric(length(y))
    for (fold in 1:3) {
      holdout <- folds == fold
      fit <- rpart::rpart(label ~ ., data = data.frame(label = factor(y), x)[!holdout, ],
                          method = "class")
      scores[holdout] <- predict(fit, x[holdout, ], type = "prob")[, "1"]
    }
    scores
  })
  expected <- pROC::roc(y, rowMeans(predictions), direction = "<", quiet = TRUE)

  result <- suppressWarnings(Train_perform_CV(
    outcome.type = "binary", outcome = y, train_data = x,
    features = list(signal = names(x)), models.indiv = "DCT", iter = 2, num.folds = 3
  ))
  simple <- result[result$model == "Simple", ]
  expect_equal(as.numeric(simple$auc), as.numeric(expected$auc))
  expect_equal(simple$sp95, as.numeric(pROC::coords(expected, 0.95,
                                                   input = "specificity", ret = "sensitivity")))
})

test_that("survival Simple preserves aligned risks outside probability bounds", {
  set.seed(12)
  n <- 120
  x <- data.frame(G1 = rnorm(n), G2 = rnorm(n))
  event <- rexp(n, rate = exp(x$G1))
  censor <- rexp(n, rate = 0.4)
  time <- pmin(event, censor)
  status <- as.integer(event <= censor)
  control <- list(minsplit = 10, maxdepth = 3, cp = 0.001)
  set.seed(1)
  folds <- balanced.folds.vec(status, 3)
  scores <- numeric(n)
  input <- data.frame(time, status, x)
  for (fold in 1:3) {
    holdout <- folds == fold
    fit <- rpart::rpart(survival::Surv(time, status) ~ ., input[!holdout, ],
                        method = "exp", control = do.call(rpart::rpart.control, control))
    scores[holdout] <- predict(fit, x[holdout, ])
  }
  expect_true(any(scores > 1))
  expected <- survival::concordance(survival::Surv(time, status) ~ scores,
                                     reverse = TRUE)$concordance

  result <- Train_perform_CV(
    outcome.type = "survival", outcome = status, survdays = time, time_point = 1,
    train_data = x, features = list(signal = names(x)), models.indiv = "DCT",
    iter = 1, num.folds = 3, param.indiv = list(DCT = control)
  )
  expect_equal(result$cindex[result$model == "Simple"], expected)
})

test_that("multiclass Simple uses base learners and preserves sample and class order", {
  set.seed(13)
  y <- factor(rep(c("C", "A", "B"), each = 36), levels = c("B", "C", "A"))
  x <- data.frame(G1 = rnorm(length(y)) + as.integer(y), G2 = rnorm(length(y)))
  input <- data.frame(label = y, x)
  set.seed(1)
  folds <- balanced.folds.vec(y, 3)
  expected <- matrix(NA_real_, nrow(x), nlevels(y), dimnames = list(NULL, levels(y)))
  for (fold in 1:3) {
    holdout <- folds == fold
    fit <- rpart::rpart(label ~ ., input[!holdout, ], method = "class", cp = 0.001)
    expected[holdout, ] <- predict(fit, x[holdout, ], type = "prob")[, levels(y)]
  }
  result <- Train_perform_CV(
    outcome.type = "categorical", outcome = y, train_data = x,
    features = list(signal = names(x)), models.indiv = "DCT",
    iter = 1, num.folds = 3, ensemble = TRUE, models.ens = "RF",
    param.ens = list(RF = list(ntree = 20))
  )
  expect_equal(attr(result, "predictions")$simple, expected)

  holdout <- folds == 1
  fit <- rpart::rpart(label ~ ., input[!holdout, ], method = "class", cp = 0.001)
  probs <- predict(fit, x[holdout, ], type = "prob")[, levels(y)]
  result <- Test_perform(
    outcome.type = "categorical", outcome = y[!holdout], train_data = x[!holdout, ],
    test_data = x[holdout, ], test_outcome = y[holdout],
    features = list(signal = names(x)), models.indiv = "DCT"
  )
  expect_equal(result$model, "simple")
  expect_equal(as.numeric(result$auc), as.numeric(pROC::multiclass.roc(y[holdout], probs)$auc))
})
