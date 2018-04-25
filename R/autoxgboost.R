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
#'   Control object for optimizer.
#'   If not specified, the default \code{\link[mlrMBO]{makeMBOControl}}] object will be used with
#'   \code{iterations} maximum iterations and a maximum runtime of \code{time.budget} seconds.
#' @param iterations [\code{integer(1L}]\cr
#'   Number of MBO iterations to do. Will be ignored if custom \code{control} is used.
#'   Default is \code{160}.
#' @param time.budget [\code{integer(1L}]\cr
#'   Time that can be used for tuning (in seconds). Will be ignored if custom \code{control} is used.
#'   Default is \code{3600}, i.e., one hour.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set to tune over. Default is \code{\link{autoxgbparset}} or \code{\link{lightgbmparset}}, depending on \code{boosting.backend}.
#' @param max.nrounds [\code{integer(1)}]\cr
#'   Maximum number of allowed boosting iterations. Default is \code{10^6}.
#' @param early.stopping.rounds [\code{integer(1L}]\cr
#'   After how many iterations without an improvement in the boosting OOB error should be stopped?
#'   Default is \code{10}.
#' @param build.final.model [\code{logical(1)}]\cr
#'   Should the model with the best found configuration be refitted on the complete dataset?
#'   Default is \code{FALSE}.
#' @param early.stopping.fraction [\code{numeric(1)}]\cr
#'   What fraction of the data should be used for early stopping (i.e. as a validation set).
#'   Default is \code{4/5}.
#' @param design.size [\code{integer(1)}]\cr
#'   Size of the initial design. Default is \code{15L}.
#' @param impact.encoding.boundary [\code{integer(1)}]\cr
#'   Defines the threshold on how factor variables are handled. Factors with more levels than the \code{"impact.encoding.boundary"} get impact encoded while factor variables with less or equal levels than the \code{"impact.encoding.boundary"} get dummy encoded.
#'   For \code{impact.encoding.boundary = 0L}, all factor variables get impact encoded while for \code{impact.encoding.boundary = .Machine$integer.max}, all of them get dummy encoded.
#'   Default is \code{10}.
#' @param mbo.learner [\code{\link[mlr]{Learner}}]\cr
#'   Regression learner from mlr, which is used as a surrogate to model our fitness function.
#'   If \code{NULL} (default), the default learner is determined as described here: \link[mlrMBO]{mbo_default_learner}.
#' @param nthread [integer(1)]\cr
#'   Number of cores to use.
#'   If \code{NULL} (default), xgboost will determine internally how many cores to use.
#' @param tune.threshold [logical(1)]\cr
#'   Should thresholds be tuned? This has only an effect for classification, see \code{\link[mlr]{tuneThreshold}}.
#'   Default is \code{TRUE}.
#' @param boosting.backend [\character(1)]\cr
#'   What boosting framework should be used? Currently supported are \code{"xgboost"} and \code{"lightgbm"}.
#' @return \code{\link{AutoxgbResult}}
#' @export
#' @examples
#' \donttest{
#' iris.task = makeClassifTask(data = iris, target = "Species")
#' ctrl = makeMBOControl()
#' ctrl = setMBOControlTermination(ctrl, iters = 1L) #Speed up Tuning by only doing 1 iteration
#' res = autoxgboost(iris.task, control = ctrl, tune.threshold = FALSE)
#' res
#' }
autoxgboost = function(task, measure = NULL, control = NULL, iterations = 160L, time.budget = 3600L,
  par.set = NULL, max.nrounds = 10^6, early.stopping.rounds = 10L, early.stopping.fraction = 4/5,
  build.final.model = TRUE, design.size = 15L, impact.encoding.boundary = 10L, mbo.learner = NULL,
  nthread = NULL, tune.threshold = TRUE, boosting.backend = "xgboost") {


  # check inputs
  assertClass(task, "SupervisedTask")
  assertClass(measure, "Measure", null.ok = TRUE)
  assertClass(control, "MBOControl", null.ok = TRUE)
  assertIntegerish(iterations)
  assertIntegerish(time.budget)
  assertClass(par.set, "ParamSet", null.ok = TRUE)
  assertIntegerish(max.nrounds, lower = 1L, len = 1L)
  assertIntegerish(early.stopping.rounds, lower = 1L, len = 1L)
  assertNumeric(early.stopping.fraction, lower = 0, upper = 1, len = 1L)
  assertFlag(build.final.model)
  assertIntegerish(design.size, lower = 1L, len = 1L)
  assertIntegerish(impact.encoding.boundary, lower = 0, len = 1L)
  assertIntegerish(nthread, lower = 1, len = 1L, null.ok = TRUE)
  assertFlag(tune.threshold)

  # set defaults
  measure = coalesce(measure, getDefaultMeasure(task))
  if (is.null(control)) {
    control = makeMBOControl()
    control = setMBOControlTermination(control, iters = iterations, time.budget = time.budget)
  }



  tt = getTaskType(task)
  td = getTaskDesc(task)
  has.cat.feats = sum(td$n.feat[c("factors", "ordered")]) > 0

  base.learner = createBaseLearner(boosting.backend, td, tt, measure, tune.threshold, nthread, early.stopping.rounds, max.nrounds, impact.encoding.boundary)
  if (!is.null(par.set))
    par.set = getDefaultParSet(base.learner, has.cat.feats)


  # Create pipeline

  preproc.pipeline = NULLCPO

  if (has.cat.feats && "factors" %nin% getLearnerProperties(base.learner)) {
    preproc.pipeline %<>>% generateCatFeatPipeline(task, impact.encoding.boundary)
  }

  preproc.pipeline %<>>% cpoDropConstants()


  # process data and apply pipeline

  # split early stopping data
  rinst = makeResampleInstance(makeResampleDesc("Holdout", split = early.stopping.fraction), task)
  task.test = subsetTask(task, rinst$test.inds[[1]])
  task.train = subsetTask(task, rinst$train.inds[[1]])

  task.train %<>>% preproc.pipeline
  task.test %<>>% retrafo(task.train)
  base.learner = setHyperPars(base.learner, early.stopping.data = task.test)

  # Optimize
  opt = smoof::makeSingleObjectiveFunction(name = "optimizeWrapper",
    fn = function(x) {
      lrn = setHyperPars(base.learner, par.vals = x)
      mod = train(lrn, task.train)
      pred = predict(mod, task.test)
      nrounds = getBestIteration(mod)

      if (tune.threshold && getTaskType(task.train) == "classif") {
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

  optim.result = mbo(fun = opt, control = control, design = des, learner = mbo.learner)


  lrn = buildFinalLearner(optim.result, objective, predict.type, par.set = par.set, preproc.pipeline = preproc.pipeline)

  mod = NULL
  if(build.final.model)
    mod = train(lrn, task)

  makeS3Obj("AutoxgbResult",
    optim.result = optim.result,
    final.learner = lrn,
    final.model = mod,
    measure = measure,
    preproc.pipeline = preproc.pipeline
  )

}
