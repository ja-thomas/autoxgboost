#' @export
makeRLearner.regr.autoxgboost = function() {
  makeRLearnerRegr(
    cl = "regr.autoxgboost",
    package = "autoxgboost",
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "measure", default = mse),
      makeUntypedLearnerParam(id = "control"),
      makeUntypedLearnerParam(id = "par.set", default = autoxgbparset),
      makeIntegerLearnerParam(id = "max.nrounds", lower = 1L, default = 10L^6),
      makeIntegerLearnerParam(id = "early.stopping.rounds", lower = 1, default = 10L),
      makeNumericLearnerParam(id = "early.stopping.fraction", lower = 0, upper = 1, default = 4/5),
      makeDiscreteLearnerParam(id = "build.final.model", values = c("model.only", "optim.result.only", "both"), default = "model.only"),
      makeIntegerLearnerParam(id = "design.size", lower = 1L, default = 15L),
      makeNumericVectorLearnerParam(id = "initial.subsample.range", len = 2, lower = c(0,0), upper = c(1,1), default = c(0.5, 0.55))
    ),
    properties = c("numerics", "weights"),
    name = "Automatic eXtreme Gradient Boosting",
    short.name = "autoxgboost",
    note = ""
    )
}

#' @export
trainLearner.regr.autoxgboost = function(.learner, .task, .subset, .weights = NULL,
  measure = mse, control, par.set = autoxgbparset, max.nrounds = 10^6, early.stopping.rounds = 10L,
  early.stopping.fraction = 4/5, build.final.model = "model.only", design.size = 15L,
  initial.subsample.range = c(0.5, 0.55)) {

  .task = subsetTask(.task, .subset)
  autoxgboost(.task, measure, control, par.set, max.nrounds, early.stopping.rounds, early.stopping.fraction, build.final.model, design.size, initial.subsample.range)

}

#' @export
predictLearner.regr.autoxgboost = function(.learner, .model, .newdata, ...) {

  if (inherits(.model, "MBOResult"))
    stop("build.final.model needs to be 'model.only' or 'both'")

  if (inherits(.model, "list"))
    model = .model$learner.model$model
  else
    model = .model$learner.model

  learner = model$learner

  predictLearner(learner, model, .newdata, ...)

}