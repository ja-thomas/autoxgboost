# Create xgboost learner based on the optimization result
buildFinalLearner = function(optim.result, objective, predict.type, par.set) {

  nrounds = optim.result$opt.path$env$extra[[optim.result$best.ind]]$nrounds
  pars = trafoValue(par.set, optim.result$x)
  lrn = if (!missing(predict.type)) {
    makeLearner("classif.xgboost", par.vals = pars, nrounds = nrounds,
      objective = objective, predict.type = predict.type)
  } else {
    makeLearner("regr.xgboost", par.vals = optim.result$x, nrounds = nrounds,
      objective = objective)
  }

  return(lrn)
}
