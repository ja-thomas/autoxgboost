#Get best nrounds value from optimization result
getBestNrounds = function(optim.result) {
  optim.result$opt.path$env$extra[[optim.result$best.ind]]$nrounds
}

getThreshold = function(optim.result) {
  optim.result$opt.path$env$extra[[optim.result$best.ind]][[".threshold"]]
}

#get the iteration parameter of a fitted xboost model with early stopping
getBestIteration = function(mod) {
  getLearnerModel(mod, more.unwrap = TRUE)$best_iteration
}

#relative frequency of first factor level
classOneFraction = function(x) {
  (table(x) / length(x))[1]
}

# transfer rate for impact encoding between conditional and prior probabilities
impactEncodingLambda = function(n, slope.param, trust.param) {
  1 / (1 + exp(-((n - trust.param) / slope.param)))
}