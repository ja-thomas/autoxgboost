#' @title Fit and optimize a xgboost model.
#'
#' @description
#' An xgboost model is optimized based on a measure (see [\code{\link[mlr]{Measure}}]).
#' The bounds of the parameter in which the model is optimized, are defined by \code{\link{autoxgbparset}}.
#' For the optimization itself bayesian optimization with \pkg{mlrMBO} is used.
#' Without any specification of the control object, the optimizer runs for for 80 iterations or 1 hour, whatever happens first.
#' Both the parameter set and the control object can be set by the user.
#'
#'
#' @param task [\code{\link[mlr]{Task}}]\cr
#'   The task.
#' @param measure [\code{\link[mlr]{Measure}}]\cr
#'   Performance measure. If \code{NULL} \code{\link[mlr]{getDefaultMeasure}} is used.
#' @param control [\code{\link[mlrMBO]{MBOControl}}]\cr
#'   Control object for mbo. Specifies runtime behaviour.
#'   Default is to run for 160 iterations or 1 hour, whatever happens first.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set. Default is \code{\link{autoxgbparset}}.
#' @param max.nrounds [\code{integer(1)}]\cr
#'   Maximum number of allowed iterations. Default is \code{10^6}.
#' @param early.stopping.rounds [\code{integer(1L}]\cr
#'   After how many iterations without an improvement in the OOB error should be stopped?
#'   Default is 10.
#' @param build.final.model [\code{logical(1)}]\cr
#'   Should the best found model be fitted on the complete dataset?
#'   Default is \code{FALSE}.
#' @param early.stopping.fraction [\code{numeric(1)}]\cr
#'   What fraction of the data should be used for early stopping (i.e. as a validation set).
#'   Default is \code{4/5}.
#' @param design.size [\code{integer(1)}]\cr
#'   Size of the initial design. Default is \code{15L}.
#' @param initial.subsample.range [\code{numeric(2)} | \code{NULL}]\cr
#'   From what range should the subsampling parameter in the initial design be sampled?
#'   It is useful to restrict to fraction to be rather small to speed up the calcuation of the initial design.
#'   If \code{NULL} the full range defined in \code{par.set} is used.
#'   Default is \code{c(0.5, 0.55)}.
#' @param factor.encoder [\code{numeric(1)}]\cr
#'   Defines how factor variables are handled. Either \code{"impact"} for impact encoding (see \code{\link{makeImpactFeaturesWrapper}}).
#'   Or \code{"dummy"} for Dummy encoding (see \code{\link[mlr]{createDummyFeatures}}).
#'   Default is \code{"impact"}.
#' @param mbo.learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner from mlr, which is used as a surrogate to model our fitness function.
#'   If \code{NULL} (default), the default learner is determined as described here: \link[mlrMBO]{mbo_default_learner}.
#' @param nthread [integer(1)]\cr
#'   Number of cores to use.
#'   If \code{NULL} (default), xgboost will determine internally how many cores to use.
#' @param tune.threshold [logical(1)]\cr
#'   Should thresholds be tuned? This has only an effect for classification, see \code{\link[mlr]{tuneThreshold}}.
#'   Default is \code{TRUE}.
#' @return \code{\link{AutoxgbResult}}
#' @export
autoxgboost = function(task, measure = NULL, control = NULL, par.set = NULL, max.nrounds = 10^6,
  early.stopping.rounds = 10L, early.stopping.fraction = 4/5, build.final.model = TRUE,
  design.size = 15L, initial.subsample.range = c(0.5, 0.55), factor.encoder = "impact", mbo.learner = NULL,
  nthread = NULL, tune.threshold = TRUE) {

  assertIntegerish(early.stopping.rounds, lower = 1L, len = 1L)
  assertNumeric(early.stopping.fraction, lower = 0, upper = 1, len = 1L)
  assertFlag(build.final.model)
  assertIntegerish(design.size, lower = 1, null.ok = FALSE)
  assertNumeric(initial.subsample.range, lower = 0, upper = 1, len = 2, null.ok = TRUE)
  if (!is.null(initial.subsample.range) & (initial.subsample.range[2] <= initial.subsample.range[1]))
    stop("Upper initial subsample range musst be greater (>) than lower ranger")
  assertChoice(factor.encoder, c("impact", "dummy"))
  assertIntegerish(nthread, lower = 1, null.ok = TRUE)
  assertFlag(tune.threshold)

  measure = coalesce(measure, getDefaultMeasure(task))
  if (is.null(control)) {
    control = makeMBOControl()
    control = setMBOControlTermination(control, iters = 160L, time.budget = 3600L)
  }

  par.set = coalesce(par.set, autoxgboost::autoxgbparset)
  tt = getTaskType(task)
  td = getTaskDesc(task)
  has.cat.feats = sum(td$n.feat[c("factors", "ordered")]) > 0
  pv = list()
  if (!is.null(nthread))
    pv$nthread = nthread

  if (tt == "classif") {

    predict.type = ifelse("req.pred" %in% measure$properties, "prob", "response")
    objective = ifelse(length(td$class.levels) == 2, "binary:logistic", "multi:softprob")
    eval_metric = ifelse(length(td$class.levels) == 2, "error", "merror")

    base.learner = makeLearner("classif.xgboost.earlystop", id = "classif.xgboost.earlystop", predict.type = predict.type,
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds,
      max.nrounds = max.nrounds, early.stopping.fraction = early.stopping.fraction, par.vals = pv)

  } else if (tt == "regr") {
    predict.type = NULL
    objective = "reg:linear"
    eval_metric = "rmse"

    base.learner = makeLearner("regr.xgboost.earlystop", id = "regr.xgboost.earlystop",
      eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds,
      max.nrounds = max.nrounds, early.stopping.fraction = early.stopping.fraction, par.vals = pv)

  } else {
    stop("Task must be regression or classification")
  }

  if (has.cat.feats) {
    if (factor.encoder == "impact") {
      encode.fun = if (tt == "regr") mean else classOneFraction
      base.learner = makeImpactFeaturesWrapper(base.learner, fun = encode.fun)
    } else {
      task = createDummyFeatures(task)
    }
  }


  opt = smoof::makeSingleObjectiveFunction(name = "optimizeWrapper",
    fn = function(x) {

      lrn = setHyperPars(base.learner, par.vals = x)
      mod = train(lrn, task)
      test = subsetTask(task, subset = mod$learner.model$test.inds)
      pred = predict(mod, test)
      nrounds = getBestIteration(mod)

      if (tune.threshold && tt == "classif") {
        tune.res = tuneThreshold(pred = pred, measure = measure)
        res = tune.res$perf
        attr(res, "extras") = list(nrounds = nrounds, .threshold = tune.res$th)
      } else {
        res = performance(pred, measure)
        attr(res, "extras") = list(nrounds = nrounds)
      }

      res

    }, par.set = par.set, noisy = TRUE, has.simple.signature = FALSE, minimize = measure$minimize)


  des = generateDesign(n = design.size, par.set)
  des$subsample = runif(design.size, initial.subsample.range[1], initial.subsample.range[2])

  optim.result = mbo(fun = opt, control = control, design = des, learner = mbo.learner)

  lrn = buildFinalLearner(optim.result, objective, predict.type, par.set = par.set)

  if (has.cat.feats > 0 & factor.encoder == "impact") {
    lrn = makeImpactFeaturesWrapper(lrn, fun = encode.fun)
  }


  mod = if(build.final.model) {
    train(lrn, task)
  } else {
    NULL
  }

  makeS3Obj("AutoxgbResult",
    optim.result = optim.result,
    final.learner = lrn,
    final.model = mod,
    measure = measure)

}
