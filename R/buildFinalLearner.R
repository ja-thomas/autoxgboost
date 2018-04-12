# Create xgboost learner based on the optimization result
buildFinalLearner = function(optim.result, objective, predict.type = NULL, par.set,
  dummy.cols = character(0L), impact.cols = character(0L), preproc.pipeline) {

  nrounds = getBestNrounds(optim.result)
  pars = trafoValue(par.set, optim.result$x)

  lrn = if (!is.null(predict.type)) {
    makeLearner("classif.xgboost.custom", nrounds = nrounds, objective = objective,
      predict.type = predict.type, predict.threshold = getThreshold(optim.result))
  } else {
    makeLearner("regr.xgboost.custom", nrounds = nrounds, objective = objective)
  }
  lrn = setHyperPars2(lrn, par.vals = pars)

  lrn = preproc.pipeline %>>% lrn


  #FIXME mlrCPO #39
  #lrn$properties = c(lrn$properties, "weights")

  return(lrn)
}
