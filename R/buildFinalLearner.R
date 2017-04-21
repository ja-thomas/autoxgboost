# Create xgboost learner based on the optimization result
buildFinalLearner = function(optim.result, objective, predict.type = NULL, par.set) {

  nrounds = getBestNrounds(optim.result)
  pars = trafoValue(par.set, optim.result$x)
  lrn = if (!is.null(predict.type)) {
    makeLearner("classif.xgboost", par.vals = pars, nrounds = nrounds,
      objective = objective, predict.type = predict.type)
  } else {
    makeLearner("regr.xgboost", par.vals = pars, nrounds = nrounds,
      objective = objective)
  }

  return(lrn)
}
