#' @title Fit and optimize a xgboost model
#' 
#' @description FIXME: ToDo
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
  
  
  if(getTaskType(task) != "classif")
    stop("sorry, for now just classification...")
  
  
  prob = "req.pred" %in% measure$properties
  predict.type = ifelse(prob, "prob", "response")
  
  #FIXME: set correct objective as soon as more than just binary classif is supported
  objective = "binary:logistic"
  
  #FIXME: set correct objective as soon as more than just binary classif is supported
  #FIXME: we want to generate the eval directly metric from the measure
  eval_metric = "error"

  #FIXME: Magic Numbers
  baseLearner = makeLearner("classif.autoxgboost", predict.type = predict.type, 
    eval_metric = eval_metric, objective = objective, early_stopping_rounds = 10L)
  
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