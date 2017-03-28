#' @title Fit and optimize a xgboost model.
#'
#' @description
#' An xgboost model is optimized based on a measure (see [\code{\link[mlr]{Measure}}]).
#' The bounds of parameter in which the model is optimized, is defined by \code{autoxgbparset}.
#' For the optimization itself bayesian optimization with \pkg{mlrMBO} is used.
#'
#'
#' @param task [\code{\link[mlr]{Task}}]\cr
#'   The task.
#' @param measure [\code{\link[mlr]{Measure}}]\cr
#'   Performance measure.
#' @param control [\code{\link[mlrMBO]{MBOControl}}]\cr
#'   Control object for mbo.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set.
#' @export
autoxgboost = function(task, measure, control, par.set = autoxgbparset) {

  tt = getTaskType(task)

  if (tt == "classif") {

    prob = "req.pred" %in% measure$properties
    predict.type = ifelse(prob, "prob", "response")
    objective = "binary:logistic"
    eval_metric = "error"

    #FIXME: Magic Numbers
    baseLearner = makeLearner("classif.autoxgboost", predict.type = predict.type,
    eval_metric = eval_metric, objective = objective, early_stopping_rounds = 10L)

  } else if (tt == "regr") {

    objective = "reg:linear"
    eval_metric = "rmse"
    #FIXME: Magic Numbers
    baseLearner = makeLearner("regr.autoxgboost",
    eval_metric = eval_metric, objective = objective, early_stopping_rounds = 10L)

  } else {
    stopf("Task must be regression or classification, but is %s", tt)
  }


  opt = smoof::makeSingleObjectiveFunction(name = "optimizeWrapper",
    fn = function(x) {

      lrn = setHyperPars(baseLearner, par.vals = x)
      mod = train(lrn, task)
      test = subsetTask(task, subset = mod$learner.model$test.inds)
      pred = predict(mod, test)
      res = performance(pred, measure)
      attr(res, "extras") = list(nrounds = mod$learner.model$best_iteration)
      res

    }, par.set = par.set, noisy = TRUE, has.simple.signature = FALSE, minimize = measure$minimize)

  mbo(fun = opt, control = control)

}
