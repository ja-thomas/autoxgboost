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


# impact encoding
impactEncoding = function(obj, work.cols, value.table, prior.table, slope.param, trust.param) {
  for (wc in work.cols) {
    tab = value.table[[wc]]
    if (ncol(tab) == 2) {# regression or binary classif
      obj[, wc] = as.factor(obj[, wc])
      levels(obj[, wc]) = tab[, 2] # set factor levels to conditional probabilities (classification) / conditional means (regression)
      n = as.numeric(table(obj[, wc])[as.character(obj[, wc])]) # vector with absolute frequencies of each factor level
      obj[, wc] = impactEncodingLambda(n, slope.param, trust.param) * as.numeric(as.character(obj[, wc])) + (1 - impactEncodingLambda(n, slope.param, trust.param)) * prior.table
    } else {# multiclass classif
      new.cols = paste(wc, colnames(tab[, -1]), sep = ".")
      obj[, new.cols] = obj[, wc]
      obj[, wc] = NULL
      for (i in seq_along(new.cols)) {
        obj[, new.cols[i]] = as.factor(obj[, new.cols[i]])
        levels(obj[, new.cols[i]]) = tab[order(as.character(tab[, 1])), i + 1] # set factor levels to conditional probabilities
        n = as.numeric(table(obj[, new.cols[i]])[as.character(obj[, new.cols[i]])]) # vector with absolute frequencies of each factor level
        obj[, new.cols[i]] = impactEncodingLambda(n, slope.param, trust.param) * as.numeric(as.character(obj[, new.cols[i]])) + (1 - impactEncodingLambda(n, slope.param, trust.param)) * prior.table[i]
      }
    }
  }
  return(obj)
}

# transfer rate for impact encoding between conditional and prior probabilities
impactEncodingLambda = function(n, slope.param, trust.param) {
  1 / (1 + exp(-((n - trust.param) / slope.param)))
}

brierObservationWise = function(pred) {

  positive = pred$task.desc$positive
  probabilities = pred$data[, paste0("prob.", positive)]

   y = as.numeric(pred$data$truth == positive)
   return((y - probabilities)^2)

}
