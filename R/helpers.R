#Get best nrounds value from optimization result
getBestNrounds = function(optim.result) {
  optim.result$opt.path$env$extra[[optim.result$best.ind]]$nrounds
}

#get the iteration parameter of a fitted xboost model with early stopping
getBestIteration = function(mod) {
  UseMethod("getBestIteration")
}

getBestIteration.PreprocModel = function(mod) {
  mod$learner.model$next.model$learner.model$best_iteration
}

getBestIteration.WrappedModel = function(mod) {
  mod$learner.model$best_iteration
}


#relative frequency of first factor level
classOneFraction = function(x) {
  table(x)/length(x)[1]
}