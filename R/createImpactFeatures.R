#' @title Generate impact encoded numeric variables for factor features.
#'
#' @description
#' Replace all factor features with impact encoded versions following the method of Daniele Micci-Barreca (see \url{http://dl.acm.org/citation.cfm?id=507538}).
#' For binary classification a factor level \eqn{x_i} of a factor feature \eqn{X} is replaced by
#' \deqn{x^\ast_i = \lambda(n_{x_i})\cdot P(Y|X=x_i) + (1-\lambda(n_{x_i}))\cdot P(Y),}
#' where \eqn{n_{x_i}=\#\{x_i\in X\}} and 
#' \deqn{\lambda(n) = \frac{1}{1 + \exp^{-\frac{(n - trust.param)}{slope.param}}}}
#' describes the transition rate between between the conditional probability of a factor level 
#' and the prior probability of the class levels (classification), or the average of the target 
#' variable for each factor level (regression).
#' For multiclass classification with \eqn{Y\in\{y_1,\ldots,y_k\}}, i.e. \eqn{k} class levels, 
#' this method is extended by creating a new input feature \eqn{X_j} for each class \eqn{j = 1,\ldots,k.}
#' Within each n new feature \eqn{X_j}, a factor level \eqn{x_i} is then replaced by
#' \deqn{x^\ast_{i,j} = \lambda(n_{x_i})\cdot P(Y=y_j|X=x_i) + (1-\lambda(n_{x_i}))\cdot P(Y=y_j).}
#' For regression tasks, we replace a factor level \eqn{x_i} by
#' \deqn{x^\ast_i = \lambda(n_{x_i})\cdot \bar{Y_{x_i}} + (1-\lambda(n_{x_i}))\cdot\bar{Y},}
#' where \eqn{\bar{Y_{x_i}}} is the mean of all \eqn{Y} for which \eqn{X=x_i}.
#' Missing factor levels are imputed by the mean of over all other replacements.
#' Returned is a list with four slots: \code{data}, containing either a \code{data.frame}
#' or a \code{task}, depending on the passed object. A list named \code{value.table} containing
#' data.frames of the conditional probabilities or means aggregated by \code{fun}.
#' Moreover, the prior probabilities of the class levels  are returned in the \code{prior.table}.
#' @param obj [\code{data.frame} | \code{\link{Task}}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name(s) of the target variable(s).
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param cols [\code{character}]\cr
#'   Columns to create impact features for. Default is to use all columns.
#' @param fun [\code{function}]\cr
#'   Function to apply to the response for each factor level in order to obtain the 
#'   conditional probabilities for each factor level, should always return a numeric vector.
#'   If \code{NULL}, \code{mean} is used for regression, \code{table(x)[1] / length(x)} for binary classification,
#'   and \code{table(x) / length(x)} for multiclass classification.
#' @param slope.param [\code{numeric}]\cr
#'   Controls the rate of transition \eqn{\lambda} (see \code{\link{createImpactFeatures}}). Default is \code{100L}.
#' @param trust.param [\code{integer}]\cr
#'   Determines half of the minimal sample size for which we completely "trust" the conditional 
#'   probability of a factor level. Default is \code{100L}
#' @return [\code{list}]: A list with three slots, see description for details.
#' @export
createImpactFeatures = function(obj, target = character(0L), cols = NULL,
  fun = NULL, slope.param = 100, trust.param = 100) {
  mlr:::checkTargetPreproc(obj, target, cols)
  assertFunction(fun, null.ok = TRUE)
  assertNumber(slope.param, lower = 0)
  assertNumber(trust.param, lower = 0)
  UseMethod("createImpactFeatures")
}

#' @export
createImpactFeatures.data.frame = function(obj, target = character(0L), 
  cols = NULL, fun = NULL, slope.param = 100, trust.param = 100) {
  # get all factor feature names present in data
  work.cols = colnames(obj)[vlapply(obj, is.factor)]
  work.cols = setdiff(work.cols, target)

  # aggregation function and prior table
  if (is.numeric(obj[, target])) { # regression
    prior.table = mean(obj[, target])
    if (is.null(fun))
      fun = mean 
  } else if (length(unique(obj[, target])) > 2) {# multiclass classification
    prior.table = table(obj[, target]) / nrow(obj)
    if (is.null(fun))
      fun = function(x) table(x) / length(x)
  } else {  # binary classification
    prior.table = classOneFraction(obj[, target])
    if (is.null(fun))
      fun = function(x) table(x)[1] / length(x)
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
  obj = impactEncoding(obj, work.cols, value.table, prior.table, slope.param, trust.param)
  
  return(list(
    data = obj,
    value.table = value.table,
    prior.table = prior.table))
}

#' @export
createImpactFeatures.Task = function(obj, target = character(0L), cols = NULL,
  fun = NULL, slope.param = 100, trust.param = 100) {
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
    prior.table = d$prior.table))
}
