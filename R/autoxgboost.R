#' @title Fit and optimize a xgboost model.
#'
#' @description
#' An xgboost model is optimized based on a measure (see [\code{\link[mlr]{Measure}}]).
#' The bounds of the parameter in which the model is optimized, are defined by \code{\link{autoxgbparset}}.
#' For the optimization itself bayesian optimization with \pkg{mlrMBO} is used.
#' The runtime is defined by \code{autoxgbcontrol}.
#' Both the parameter set and the control object can be set by the user.
#'
#'
#' @param task [\code{\link[mlr]{Task}}]\cr
#'   The task.
#' @param measure [\code{\link[mlr]{Measure}}]\cr
#'   Performance measure. If \code{NULL} \code{\link[mlr]{getDefaultMeasure}} is used.
#' @param control [\code{\link[mlrMBO]{MBOControl}}]\cr
#'   Control object for mbo. Specifies runtime behaviour.
#'   Default is to run for 80 iterations or 1 hour, whatever happens first.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set. Default is \code{\link{autoxgbparset}}.
#' @param max.nrounds [\code{integer(1)}]\cr
#'   Maximum number of allowed iterations. Default is \code{10^6}.
#' @param early.stopping.rounds [\code{integer(1L}]\cr
#'   After how many iterations without an improvement in the OOB error should be stopped?
#'   Default is 10.
#' @param build.final.model [\code{character(1)}]\cr
#'   Should the best found model be fitted on the complete dataset?
#'   Options are:
#'   \describe{
#'     \item{"model.only"}{A fitted xgboost model based on the best found configuration is returned.}
#'     \item{"optim.result.only"}{The optimization result is returned.}
#'     \item{"both"}{Both, model and optimization result are returned as a list.}
#'   }
#'   Default is \code{"model.only"}.
#' @param early.stopping.fraction [\code{numeric(1)}]\cr
#'   What fraction of the data should be used for early stopping (i.e. as a validation set).
#'   Default is \code{4/5}.
#' @param design.size [\code{integer(1)}]\cr
#'   Size of the initial design. Default is \code{15L}.
#' @param initial.subsample.range [\code{numeric(2)}]\cr
#'   From what range should the subsampling parameter in the initial design be sampled?
#'   It is useful to restrict to fraction to be rather small to speed up the calcuation of the initial design.
#'   Default is \code{c(0.5, 0.55)}.
#' @return Special: See \code{build.final.model}
#' @export
autoxgboost = function(task, measure = NULL, control = NULL, par.set = NULL, max.nrounds = 10^6,
  early.stopping.rounds = 10L, early.stopping.fraction = 4/5, build.final.model = "model.only",
  design.size = 15L, initial.subsample.range = c(0.5, 0.55)) {

  assetIntegerish(early.stopping.rounds, lower = 1L, len = 1L)
  assertNumeric(early.stopping.fraction, lower = 0, upper = 1, len = 1L)
  assertSubset(build.final.model, c("both", "model.only", "optim.result.only"))
  assetIntegerish(design.size, lower = 1)
  assertNumeric(initial.subsample.range, lower = 0, upper = 1, len = 2)
  if (! initial.subsample.range[2] > initial.subsample.range[1])
    stop("Upper initial subsample range musst be greater (>) than lower ranger")


  measure = coalesce(measure, getDefaultMeasure(task))
  control = coalesce(control, autoxgbcontrol)
  par.set = coalesce(par.set, autoxgbparset)

  tt = getTaskType(task)
  td = getTaskDesc(task)



  if (tt == "classif") {

    predict.type = ifelse("req.pred" %in% measure$properties, "prob", "response")
    objective = ifelse(length(td$class.levels) == 2, "binary:logistic", "multi:softprob")
    eval_metric = ifelse(length(td$class.levels) == 2, "error", "merror")

    baseLearner = makeLearner("classif.xgboost.earlystop", predict.type = predict.type,
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds,
      max.nrounds = max.nrounds, early.stopping.fraction = early.stopping.fraction)

  } else if (tt == "regr") {
    predict.type = NULL
    objective = "reg:linear"
    eval_metric = "rmse"

    baseLearner = makeLearner("regr.xgboost.earlystop",
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds,
      max.nrounds = max.nrounds, early.stopping.fraction = early.stopping.fraction)

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


  des = generateDesign(n = design.size, par.set)
  des$subsample = runif(design.size, initial.subsample.range[1], initial.subsample.range[2])

  optim.result = mbo(fun = opt, control = control, design = des)

  if (build.final.model != "optim.result.only") {

    lrn = buildFinalLearner(optim.result, objective, predict.type, par.set = par.set)
    mod = train(lrn, task)

    if (build.final.model == "model.only") {
      return(mod)
    } else {
      return(list(model = mod, optim.result = optim.result))
    }

  } else {
    return(optim.result)
  }

}
