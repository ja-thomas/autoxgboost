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
      makeUntypedLearnerParam(id = "objective", default = NULL, tunable = FALSE),
      makeUntypedLearnerParam(id = "eval_metric", default = NULL, tunable = FALSE),
      makeNumericLearnerParam(id = "base_score", default = 0.5, tunable = FALSE),
      makeIntegerLearnerParam(id = "early_stopping_rounds", default = 1, lower = 1L, tunable = FALSE),
      makeIntegerLearnerParam(id = "max.nrounds", default = 10^6L, lower = 1L, upper = 10^7L),
      makeUntypedLearnerParam(id = "early.stopping.data"),
      makeLogicalLearnerParam(id = "maximize", default = NULL, special.vals = list(NULL), tunable = FALSE),
      makeIntegerLearnerParam(id = "nthread", lower = 1L, tunable = FALSE),
      makeDiscreteLearnerParam(id = "booster", default = "gbtree", values = c("gbtree", "gblinear", "dart")),
      makeDiscreteLearnerParam(id = "sample_type", default = "uniform", values = c("uniform", "weighted"), requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "normalize_type", default = "tree", values = c("tree", "forest"), requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "rate_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeNumericLearnerParam(id = "skip_drop", default = 0, lower = 0, upper = 1, requires = quote(booster == "dart")),
      makeLogicalLearnerParam(id = "one_drop", default = FALSE, requires = quote(booster == "dart")),
      makeDiscreteLearnerParam(id = "tree_method", default = "exact", values = c("exact", "hist"), requires = quote(booster != "gblinear")),
      makeDiscreteLearnerParam(id = "grow_policy", default = "depthwise", values = c("depthwise", "lossguide"), requires = quote(tree_method == "hist")),
      makeIntegerLearnerParam(id = "max_leaves", default = 0L, lower = 0L, requires = quote(grow_policy == "lossguide")),
      makeIntegerLearnerParam(id = "max_bin", default = 256L, lower = 2L, requires = quote(tree_method == "hist"))
    ),
    properties = c("numerics", "weights", "missings"),
    name = "eXtreme Gradient Boosting",
    short.name = "xgboost.earlystop",
    note = ""
  )
}

#' @export
trainLearner.regr.xgboost.earlystop = function(.learner, .task, .subset, .weights = NULL,
  objective = NULL, eval_metric = NULL, early_stopping_rounds = 1, max.nrounds = 10^6, early.stopping.data, nthread, ...) {

  parlist = list(...)
  if (is.null(eval_metric))
    eval_metric = "rmse"
  parlist$eval_metric = eval_metric

  watchlist = list(eval = createDMatrixFromTask(early.stopping.data))

  if (is.null(.weights)) {
    data = createDMatrixFromTask(subsetTask(.task, .subset))
  } else {
    data = createDMatrixFromTask(subsetTask(.task, .subset), weights = .weights)
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

  return(mod)
}

#' @export
predictLearner.regr.xgboost.earlystop = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  p = predict(m, newdata = data.matrix(convertDataFrameCols(.newdata, ints.as.num = TRUE)), ...)
}
