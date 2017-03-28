#'@export
makeRLearner.regr.autoxgboost = function() {
  makeRLearnerRegr(
    cl = "regr.autoxgboost",
    package = "xgboost",
    par.set = makeParamSet(
      # we pass all of what goes in 'params' directly to ... of xgboost
      # makeUntypedLearnerParam(id = "params", default = list()),
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0.3, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0.3, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = "binary:logistic", tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = "error", tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = 1, lower = 1L, tunable = FALSE)
    ),
    properties = c("numerics", "weights", "featimp"),
    name = "eXtreme Gradient Boosting",
    short.name = "autoxgboost",
    note = "All settings are passed directly, rather than through `xgboost`'s `params` argument. `nrounds` has been set to `1` and `verbose` to `0` by default."
  )
}

#' @export
trainLearner.regr.autoxgboost = function(.learner, .task, .subset, .weights = NULL, objective, eval_metric, early_stopping_rounds, ...) {

  parlist = list(...)
  parlist$eval_metric = eval_metric

  rdesc = makeResampleDesc("Holdout", split = 4/5)
  rinst = makeResampleInstance(rdesc, .task)
  train.inds = rinst$train.inds[[1]]
  test.inds = rinst$test.inds[[1]]

  if (is.null(.weights)) {
    watchlist = list(eval = createDMatrixFromTask(subsetTask(.task, test.inds)))
    data = createDMatrixFromTask(subsetTask(.task, train.inds))
  } else {
    watchlist = list(eval = createDMatrixFromTask(subsetTask(.task, test.inds),
      weights = .weights[test.inds]))
    data = createDMatrixFromTask(subsetTask(.task, train.inds),
      weights = .weights[train.inds])
  }

  if (is.null(objective))
    objective = "reg:linear"

  mod = xgboost::xgb.train(params = parlist, data = data, nrounds = 10^2, watchlist = watchlist,
    objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L)

  mod$test.inds = test.inds

  return(mod)
}

#' @export
predictLearner.regr.autoxgboost = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = predict(m, newdata = data.matrix(.newdata), ...)
}
