# Create xgboost learner based on the optimization result
buildFinalLearner = function(base.learner, optim.result, objective, predict.type = NULL, par.set) {

  nrounds = getBestNrounds(optim.result)
  pars = trafoValue(par.set, optim.result$x)
  lrn = setHyperPars(base.learner, nrounds = nrounds, par.vals = pars)
  if (!is.null(predict.type)) {
    lrn = setPredictType(lrn, predict.type)
  }

  return(lrn)
}
