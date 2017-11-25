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
createImpactFeatures = function(obj, target = character(0L), cols = NULL, fun = NULL, slope.param = 100L, trust.param = 100L) {
  mlr:::checkTargetPreproc(obj, target, cols)
  assertFunction(fun, null.ok = TRUE)
  UseMethod("createImpactFeatures")
}

#' @export
createImpactFeatures.data.frame = function(obj, target = character(0L), cols = NULL, fun = NULL, slope.param = 100L, trust.param = 100L) {
  # get all factor feature names present in data
  work.cols = colnames(obj)[vlapply(obj, is.factor)]
  work.cols = setdiff(work.cols, target)

  # aggregation function
  if (is.null(fun)) {
    if (is.numeric(obj[, target])) {
      fun = mean # regression
    } else if (length(unique(obj[, target])) > 2) {
      fun = function(x) table(x) / length(x) # multiclass classification
    } else { 
      fun = function(x) table(x)[1] / length(x) # binary classification
    }
  }
  
  # prior table
  if (is.numeric(obj[, target])) {
    prior.table = mean(obj[, target]) # regression
  } else if (length(unique(obj[, target])) > 2) {
    prior.table = table(obj[, target]) / nrow(obj) # multiclass classification
  } else { 
    prior.table = (table(obj[, target]) / nrow(obj))[1] # binary classification
  }

  # check that user requested cols are only factor cols
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
    work.cols = cols
  }

  value.table = lapply(work.cols, function(col) {

    res = aggregate(obj[, target], by = list(obj[, col]), fun)

    res = data.frame(lapply(res, as.data.frame)) #FIXME: Change all of that to data.table

    colnames(res) = if(ncol(res) <= 2) c(col, target) else c(col, levels(obj[, target]))
    
    #attach missing levels
    miss.frame = data.frame(setdiff(levels(res[,1]), res[,1]))
    colnames(miss.frame) = col
    res = merge(res, miss.frame, all = TRUE)
    res = impute(res, classes = list(numeric = imputeMean()))$data

  })
  names(value.table) = work.cols
  

  # replace values in the data
  for (wc in work.cols) {
    tab = value.table[[wc]]
    if (ncol(tab) == 2) {# regression or binary classif
      obj[, wc] = as.factor(obj[, wc])
      levels(obj[, wc]) = tab[, 2]
      n = as.numeric(table(obj[, wc])[as.character(obj[, wc])])
      obj[, wc] = impactEncodingLambda(n, slope.param, trust.param) * as.numeric(as.character(obj[, wc])) + (1 - impactEncodingLambda(n, slope.param, trust.param)) * prior.table
    } else { # multiclass classif
      new.cols = paste(wc, colnames(tab[, -1]), sep = ".")
      obj[, new.cols] = obj[, wc]
      obj[, wc] = NULL
      for(i in seq_along(new.cols)) {
        obj[, new.cols[i]] = as.factor(obj[, new.cols[i]])
        levels(obj[, new.cols[i]]) = tab[order(as.character(tab[, 1])), i + 1]
        n = as.numeric(table(obj[, new.cols[i]])[as.character(obj[, new.cols[i]])])
        obj[, new.cols[i]] = impactEncodingLambda(n, slope.param, trust.param) * as.numeric(as.character(obj[, new.cols[i]])) + (1 - impactEncodingLambda(n, slope.param, trust.param)) * prior.table[i]
      }
    }
  }
  

  
  return(list(
    data = obj,
    value.table = value.table,
    prior.table = prior.table,
    slope.param = slope.param,
    trust.param = trust.param))
}

#' @export
createImpactFeatures.Task = function(obj, target = character(0L), cols = NULL, fun = NULL, slope.param = 100L, trust.param = 100L) {
  target = getTaskTargetNames(obj)
  tt = getTaskType(obj)

  if (is.null(fun)) {
    if (tt == "regr") {
      fun = mean
    } else if (tt == "classif") {
      if (length(getTaskClassLevels(obj)) > 2) {
        fun = function(x) table(x) / length(x)
      } else {
        fun = function(x) table(x)[1] / length(x)
      }
    } else
      stopf("Task must be 'classif or 'regr', but is %s", tt)
  }

  d = createImpactFeatures.data.frame(obj = getTaskData(obj), target = target, 
    cols = cols, fun = fun, slope.param = slope.param, trust.param = trust.param)
  return(list(
    data = mlr:::changeData(obj, d$data),
    value.table = d$value.table,
    prior.table = d$prior.table,
    slope.param = d$slope.param,
    trust.param = d$trust.param))
}
