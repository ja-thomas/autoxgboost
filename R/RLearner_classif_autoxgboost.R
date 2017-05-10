#' @export
makeRLearner.classif.autoxgboost = function() {
  makeRLearnerClassif(
    cl = "classif.autoxgboost",
    package = "autoxgboost",
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "measure", default = mse),
      makeUntypedLearnerParam(id = "control"),
      makeUntypedLearnerParam(id = "par.set", default = autoxgbparset),
      makeIntegerLearnerParam(id = "max.nrounds", lower = 1L, default = 10L^6),
      makeIntegerLearnerParam(id = "early.stopping.rounds", lower = 1, default = 10L),
      makeNumericLearnerParam(id = "early.stopping.fraction", lower = 0, upper = 1, default = 4/5),
      makeIntegerLearnerParam(id = "design.size", lower = 1L, default = 15L),
      makeNumericVectorLearnerParam(id = "initial.subsample.range", len = 2, lower = c(0,0), upper = c(1,1), default = c(0.5, 0.55)),
      makeDiscreteLearnerParam(id  = "factor.encode", values = c("impact", "dummy"), default = "impact"),
      makeUntypedLearnerParam(id = "mbo.learner")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob", "weights"),
    name = "Automatic eXtreme Gradient Boosting",
    short.name = "autoxgboost",
    note = ""
    )
}

#' @export
trainLearner.classif.autoxgboost = function(.learner, .task, .subset, .weights = NULL,
  measure = mmce, control, par.set = autoxgbparset, max.nrounds = 10^6, early.stopping.rounds = 10L,
  early.stopping.fraction = 4/5, build.final.model = TRUE, design.size = 15L,
  initial.subsample.range = c(0.5, 0.55), factor.encode = "impact", mbo.learner = NULL, ...) {

  .task = subsetTask(.task, .subset)
  autoxgboost(.task, measure, control, par.set, max.nrounds, early.stopping.rounds, early.stopping.fraction, build.final.model, design.size, initial.subsample.range)

}

#' @export
predictLearner.classif.autoxgboost = function(.learner, .model, .newdata, ...) {

  model = .model$learner.model$final.model

  learner = model$learner
  learner = setPredictType(learner, .learner$predict.type)

  predictLearner(learner, model, .newdata, ...)

}
