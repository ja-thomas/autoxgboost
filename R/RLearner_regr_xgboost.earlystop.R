#' @export
makeRLearner.regr.xgboost.earlystop = function() {
  makeRLearnerRegr(
    cl = "regr.xgboost.earlystop",
    package = "xgboost",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "eta", default = 0.3, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "gamma", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "max_depth", default = 6L, lower = 1L),
      makeNumericLearnerParam(id = "min_child_weight", default = 1, lower = 0),
      makeNumericLearnerParam(id = "subsample", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bytree", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "colsample_bylevel", default = 1, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "num_parallel_tree", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "lambda", default = 0, lower = 0),
      makeNumericLearnerParam(id = "lambda_bias", default = 0, lower = 0),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0),
      makeUntypedLearnerParam(id = "objective", default = "binary:logistic", tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = "error", tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = 1, lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "max.nrounds", default = 10^6L, lower = 1L, upper = 10^7L),
      makeNumericLearnerParam(id = "early.stopping.fraction", lower = 0, upper = 1, default = 4/5),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE)
    ),
    properties = c("numerics", "weights", "missings"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost.earlystop",
    note = ""
  )
}

#' @export
trainLearner.regr.xgboost.earlystop = function(.learner, .task, .subset, .weights = NULL,
  objective, eval_metric, max.nrounds, early_stopping_rounds, early.stopping.fraction = 4/5, nthread, ...) {

  parlist = list(...)
  parlist$eval_metric = eval_metric

  rdesc = makeResampleDesc("Holdout", split = early.stopping.fraction)
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

  if (!missing(nthread)) {
    mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
      objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L, nthread = nthread)
  } else {
      mod = xgboost::xgb.train(params = parlist, data = data, nrounds = max.nrounds, watchlist = watchlist,
      objective = objective, early_stopping_rounds = early_stopping_rounds, silent = 1L, verbose = 0L)
  }

  mod$test.inds = test.inds

  return(mod)
}

#' @export
predictLearner.regr.xgboost.earlystop = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = predict(m, newdata = data.matrix(.newdata), ...)
}
