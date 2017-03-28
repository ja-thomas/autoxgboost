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
#' @param max.nrounds [\code{integer(1L)}]\cr
#'   Maximum number of allowed iterations. Default is \code{10^6}.
#' @param early_stopping_rounds [\code{integer(1L}]\cr
#'   After how many iterations without an improvement in the OOB error should be stopped?
#'   Default is 10.
#' @export
autoxgboost = function(task, measure, control, par.set = autoxgbparset, max.nrounds = 10^6, early_stopping_rounds = 10L) {

  tt = getTaskType(task)
  td = getTaskDesc(task)

  if (tt == "classif") {

    predict.type = ifelse("req.pred" %in% measure$properties, "prob", "response")
    objective = ifelse(length(td$class.levels) == 2, "binary:logistic", "multi:softprob")
    eval_metric = ifelse(length(td$class.levels) == 2, "error", "merror")

    baseLearner = makeLearner("classif.autoxgboost", predict.type = predict.type,
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early_stopping_rounds,
      max.nrounds = max.nrounds)

  } else if (tt == "regr") {

    objective = "reg:linear"
    eval_metric = "rmse"

    baseLearner = makeLearner("regr.autoxgboost",
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early_stopping_rounds,
      max.nrounds = max.nrounds)

  } else {
    stop("Task must be regression or classification")
  }

  if (sum(td$n.feat[c("factors", "ordered")]) > 0)
    task = createDummyFeatures(task)


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


  des = generateDesign(n = 15, par.set)
  des$subsample = runif(15, 0.5, 0.55)

  mbo(fun = opt, control = control, design = des)

}
