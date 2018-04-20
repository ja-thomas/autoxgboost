#' @export
makeRLearner.classif.lightgbm = function() {
  makeRLearnerClassif(
    cl = "classif.lightgbm",
    package = "lightgbm",
    par.set = makeParamSet(
      makeUntypedLearnerParam("validation.data"),
      makeIntegerLearnerParam("nrounds", lower = 1, default = 10),
      makeDiscreteLearnerParam("metric", values = c("map", "auc", "binary_logloss", "binary_error", "multi_logloss", "multi_error")),
      makeIntegerLearnerParam("verbose", lower = -1, upper = 1, tunable = FALSE),
      makeLogicalLearnerParam("record", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam("eval_freq", lower = 1, tunable = FALSE, requires = quote(verbose > 0)),
      makeUntypedLearnerParam("init_model"),
      makeIntegerLearnerParam("early_stopping_rounds", lower = 1),
      makeDiscreteLearnerParam("boosting", values = c("dbdt", "dart", "rf")),
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
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "missings", "factors"),
    name = "Light Gradient Boosting Machine",
    short.name = "lightgbm",
    note = ""
    )
}

#' @export
trainLearner.classif.lightgbm = function(.learner, .task, .subset, .weights = NULL, validation.data = NULL, metric, ...) {

  pv = list(...)
  nc = length(getTaskDesc(.task)$class.levels)
  train = getTaskData(.task, .subset, target.extra = TRUE)
  feat.cols = colnames(train$data)[vlapply(train$data, is.factor)]
  prep = lightgbm::lgb.prepare_rules(train$data)
  pv$data = lightgbm::lgb.Dataset(data.matrix(prep$data), label = as.numeric(train$target) - 1, categorical_feature = feat.cols)
  if (!is.null(validation.data))
    pv$valids = list(test = lightgbm::lgb.Dataset.create.valid(pv$data, data.matrix(validation.data$data), label = as.numeric(validation.data$target) - 1))
  pv$metric = coalesce(metric, "")

  if(nc == 2) {
    pv$obj = "binary"
  } else {
    pv$obj = "multiclass"
    pv$num_class = nc
  }

  mod = do.call(lightgbm::lgb.train, pv)
  return(list(mod = mod, rules = prep$rules))
}

#' @export
predictLearner.classif.lightgbm = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  m = .model$learner.model
  cls = td$class.levels
  nc = length(cls)

  .newdata = data.matrix(lightgbm::lgb.prepare_rules(.newdata, rules = m$rules)$data)
  p = predict(m$mod, .newdata)

  if (nc == 2) {
    y = matrix(0, ncol = 2, nrow = nrow(.newdata))
    colnames(y) = cls
    y[, 1L] = 1 - p
    y[, 2L] = p
     if (.learner$predict.type == "prob") {
      return(y)
    } else {
      p = colnames(y)[max.col(y)]
      names(p) = NULL
      p = factor(p, levels = colnames(y))
      return(p)
    }
  } else {
     p = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
     colnames(p) = cls
     if (.learner$predict.type == "prob") {
        return(p)
     } else {
        ind = max.col(p)
        cns = colnames(p)
        return(factor(cns[ind], levels = cns))
    }
  }
}
