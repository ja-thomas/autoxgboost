#' @export
makeRLearner.classif.autoxgboost = function() {
  makeRLearnerClassif(
    cl = "classif.autoxgboost",
    package = "autoxgboost",
    par.set = makeParamSet(
      makeUntypedLearnerParam(id = "measure", default = mmce),
      makeUntypedLearnerParam(id = "control"),
      makeUntypedLearnerParam(id = "par.set", default = autoxgbparset),
      makeIntegerLearnerParam(id = "max.nrounds", lower = 1L, default = 10L^6),
      makeIntegerLearnerParam(id = "early.stopping.rounds", lower = 1, default = 10L),
      makeNumericLearnerParam(id = "early.stopping.fraction", lower = , upper = 1, default = 4/5),
      makeDiscreteLearnerParam(id = "build.final.model", values = c("model.only", "optim.result.only", "both"), default = "model.only", when = "both")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob", "weights"),
    name = "Automatic eXtreme Gradient Boosting",
    short.name = "autoxgboost",
    note = ""
    )
}

#' @export
trainLearner.classif.autoxgboost = function(.learner, .task, .subset, .weights = NULL,
  measure = mmce, control, par.set, max.nrounds = 10^6, early.stopping.rounds = 10L,
  early.stopping.fraction = 4/5, build.final.model = "model.only") {

  .task = subsetTask(.task, .subset)
  autoxgboost(.task, measure, control, par.set, max.nrounds, early.stopping.rounds, early.stopping.fraction, build.final.model)

}}

#' @export
predictLearner.classif.autoxgboost = function(.learner, .model, .newdata, build.final.model, ...) {

  if (build.final.model == "optim.result.only")
    stop("build.final.model needs to be 'model.only' or 'both'")




}
