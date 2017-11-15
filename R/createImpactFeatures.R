#' @title Generate impact encoded numeric variables for factor features.
#'
#' @description
#' Replace all factor features with impact encoded versions. The encoding is defined by \code{fun}.
#' \code{fun} is called for each level in a factor and operates on the target values of these observations.
#' For regression a common choice is \code{mean()}, i.e., replace each factor level with the average response.
#' For binary classification the probability for class one might be a good choice.
#' Missing factor levels are imputed by the mean of over all other replacements.
#' Returned is a list with two slots: \code{data}, containing either a \code{data.frame}
#' or a \code{task}, depending on the passed object. A list named \code{value.table} containing
#' data.frames of the replacement values, where the first column are the feature levels, and all further
#' columns the results of \code{fun}.
#'
#' @param obj [\code{data.frame} | \code{\link{Task}}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name(s) of the target variable(s).
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param cols [\code{character}]\cr
#'   Columns to create impact features for. Default is to use all columns.
#' @param fun [\code{function}]\cr
#'   Function to apply to the response for each factor level, should always return a numeric vector.
#'   If \code{NULL}, \code{mean} is used for regression, \code{table(x)[1] / length(x)} for binary classification,
#'   and \code{table(x) / length(x)} for multiclass classification.
#' @return [\code{list}]: A list with two slots, see description for details.
#' @export
createImpactFeatures = function(obj, target = character(0L), cols = NULL) {
  mlr:::checkTargetPreproc(obj, target, cols)
  UseMethod("createImpactFeatures")
}

#' @export
createImpactFeatures.data.frame = function(obj, target, cols = NULL, slope.param = 10L, trust.param = 10L) {
  # get all factor feature names present in data
  work.cols = colnames(obj)[vlapply(obj, is.factor)]
  work.cols = setdiff(work.cols, target)

  # check that user requested cols are only factor cols
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
    work.cols = cols
  }
  
  #create learner to predict probs
  impact.lambda = function(n, f = slope.param, k = trust.param) {
    1 / (1 + exp((n - k) / f))
  }
  
  value.table = lapply(work.cols, function(col) {
    
    if (is.factor(obj[, target])) {
      lrn = makeLearner("classif.rpart", predict.type = "prob")
      rdesc = makeResampleDesc("CV", iters = 10L)
      task = makeClassifTask(data = obj[, c(col, target)], target = target)
      mod = resample(lrn, task, rdesc, models = TRUE)
      priors = table(obj[target]) / nrow(obj)
      probs = getPredictionProbabilities(mod$pred)
      prob.cols = colnames(probs)
      new.values = data.frame()
      for (prob.col in prob.cols) {
        for (i in 1:nrow(probs)) {
          n = as.numeric(table(obj[col])[as.character(obj[i, col])])
          new.values[i, prob.col] = impact.lambda(n) * probs[i, prob.col] +
            (1 - impact.lambda(n)) * priors[as.character(obj[i, target])]
        }
      }
      
      colnames(new.values) = paste0(col, ".imp.", colnames(probs))
      return(new.values)
    } else {
      task = makeRegrTask(data = obj[, c(col, target)], target = target)
      # FIXME
    }
  
  })
  names(value.table) = work.cols
  
  #replace values in the data
  for (wc in work.cols) {
    tab = value.table[[wc]]
    if (ncol(tab) == 1) {#regression or binary classif
      obj[, wc] = tab
    } else { # for multiclass classif
      new.cols = paste(colnames(tab)[-1], sep = ".")
      obj[, wc] = NULL
      for(i in seq_along(tab)[-1]) {
        obj[, new.cols[i-1]] = tab[, i]
      }
    }
  }

  return(list(
    data = obj,
    value.table = value.table))
}

#' @export
createImpactFeatures.Task = function(obj, target = character(0L), cols = NULL) {
  target = getTaskTargetNames(obj)
  tt = getTaskType(obj)

  if (tt %nin% c("classif", "regr"))
    stopf("Task must be 'classif or 'regr', but is %s", tt)

  d = createImpactFeatures.data.frame(obj = getTaskData(obj), target = target, cols = cols)
  data = mlr:::changeData(obj, d$data)

}
