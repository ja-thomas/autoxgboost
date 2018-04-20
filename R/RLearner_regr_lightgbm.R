#' @export
makeRLearner.regr.lightgbm = function() {
  makeRLearnerRegr(
    cl = "regr.lightgbm",
    package = "lightgbm",
    par.set = makeParamSet(
      makeUntypedLearnerParam("validation.data"),
      makeIntegerLearnerParam("nrounds", lower = 1, default = 10),
      makeDiscreteLearnerParam("metric", values = c("l1", "l2", "l2_root", "quantile", "mape", "huber", "fair")),
      makeDiscreteLearnerParam("obj", values = c("regression_l2", "regression_l1", "huber", "fair", "poisson", "quantile", "mape", "gamma", "tweedie", default = "regression_l2")),
      makeIntegerLearnerParam("verbose", lower = -1, upper = 1, tunable = FALSE),
      makeLogicalLearnerParam("record", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam("eval_freq", lower = 1, tunable = FALSE, requires = quote(verbose > 0)),
      makeUntypedLearnerParam("init_model"),
      makeIntegerLearnerParam("early_stopping_rounds", lower = 1),
      makeDiscreteLearnerParam("boosting", values = c("gbdt", "dart", "rf"), default = "gbdt", requires = quote(boosting != "rf" || bagging_freq > 0 && bagging_fraction < 1 && feature_fraction < 1)),
      makeNumericLearnerParam("learning_rate", lower = 0, upper = 1, default = 0.1),
      makeIntegerLearnerParam("num_leaves", lower = 1, default = 31),
      makeDiscreteLearnerParam("tree_learner", values = c("serial", "feature", "data", "voting"), default = "serial"),
      makeIntegerLearnerParam("num_threads", lower = 1),
      makeDiscreteLearnerParam("device", values = c("cpu", "gpu"), default = "cpu"),
      makeIntegerLearnerParam("max_depth", lower = -1, default = -1),
      makeIntegerLearnerParam("min_data_in_leaf", lower = 1, default = 20),
      makeNumericLearnerParam("min_sum_hessian_in_leaf", lower = 0, default = 1e-3),
      makeNumericLearnerParam("feature_fraction", lower = 0, upper = 1, default = 1),
      makeNumericLearnerParam("bagging_fraction", lower = 0, upper = 1, default = 1),
      makeIntegerLearnerParam("bagging_freq", lower = 0, default = 0),
      makeNumericLearnerParam("lambda_l1", lower = 0, default = 0),
      makeNumericLearnerParam("lambda_l2", lower = 0, default = 0),
      makeNumericLearnerParam("min_split_gain", lower = 0, default = 0),
      makeNumericLearnerParam("drop_rate", lower = 0, upper = 1, default = 0.1, requires = quote(boosting == "dart")),
      makeNumericLearnerParam("skip_drop", lower = 0, upper = 1, default = 0.5, requires = quote(boosting == "dart")),
      makeIntegerLearnerParam("max_drop", lower = 0, default = 50, requires = quote(boosting == "dart")),
      makeLogicalLearnerParam("xgboost_dart_mode", default = FALSE),
      makeIntegerLearnerParam("min_data_per_group", lower = 1, default = 100),
      makeIntegerLearnerParam("max_cat_threshold", lower = 0, default = 32),
      makeNumericLearnerParam("cat_l2", lower = 0, default = 10),
      makeIntegerLearnerParam("max_cat_to_onehot", lower = 0, default = 4)
      ),
    properties = c("numerics", "weights", "featimp", "missings", "factors"),
    name = "Light Gradient Boosting Machine",
    short.name = "lightgbm",
    note = ""
    )
}

#' @export
trainLearner.regr.lightgbm = function(.learner, .task, .subset, .weights = NULL, validation.data = NULL, metric, ...) {

  pv = list(...)
  train = getTaskData(.task, .subset, target.extra = TRUE)
  feat.cols = colnames(train$data)[vlapply(train$data, is.factor)]
  prep = lgb.prepare_rules(train$data)
  pv$data = lgb.Dataset(data.matrix(prep$data), label = as.numeric(train$target) - 1, categorical_feature = feat.cols)
  if (!is.null(validation.data))
    pv$valids = list(test = lgb.Dataset.create.valid(pv$data, data.matrix(validation.data$data), label = as.numeric(validation.data$target) - 1))
  pv$metric = coalesce(metric, "")

  mod = do.call(lgb.train, pv)
  return(list(mod = mod, rules = prep$rules))
}

#' @export
predictLearner.regr.lightgbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata = data.matrix(lgb.prepare_rules(.newdata, rules = m$rules)$data)
  predict(m$mod, .newdata)
 }
