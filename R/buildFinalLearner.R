# Create xgboost learner based on the optimization result
buildFinalLearner = function(optim.result, objective, predict.type = NULL, par.set,
  dummy.cols = character(0L), impact.cols = character(0L)) {

  nrounds = getBestNrounds(optim.result)
  pars = trafoValue(par.set, optim.result$x)

  lrn = if (!is.null(predict.type)) {
    makeLearner("classif.xgboost", nrounds = nrounds, objective = objective,
      predict.type = predict.type, predict.threshold = getThreshold(optim.result))
  } else {
    makeLearner("regr.xgboost", nrounds = nrounds, objective = objective)
  }
  if (length(dummy.cols) != 0L)
    lrn = makeDummyFeaturesWrapper(lrn, cols = dummy.cols)
  if (length(impact.cols) != 0L)
    lrn = makeImpactFeaturesWrapper(lrn, cols = impact.cols)
  lrn = setHyperPars2(lrn, par.vals = pars)
  return(lrn)
}
