# Create xgboost learner based on the optimization result
buildFinalLearner = function(optim.result, objective, predict.type = NULL, par.set,
  factor.encoding = FALSE) {

  nrounds = getBestNrounds(optim.result)
  pars = trafoValue(par.set, optim.result$x)

  lrn = if (!is.null(predict.type)) {
    makeLearner("classif.xgboost.custom", nrounds = nrounds, objective = objective,
      predict.type = predict.type, predict.threshold = getThreshold(optim.result))
  } else {
    makeLearner("regr.xgboost.custom", nrounds = nrounds, objective = objective)
  }
  if (factor.encoding)
    lrn = makeIntegerFeaturesWrapper(lrn)
  lrn = setHyperPars2(lrn, par.vals = pars)
  return(lrn)
}
