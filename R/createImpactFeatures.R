#' @title Generate impact encoded numeric variables for factor features.
#'
#' @description
#' Replace all factor features with impact encoded versions. The encoding is defined by fun.
#' \code{fun} is called for each level in a factor and operates on the target values of these observations.
#' For regression a common choice is \code{mean()}, i.e., replace each factor level with the average response.
#' For binary classification the probability for class one might be a good choice.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param cols [\code{character}]\cr
#'   Columns to create impact features for. Default is to use all columns.
#' @param fun [\code{function}]\cr
#'   Function to apply on the response for each factor level, should always return one numeric value.
#' @return [\code{list}]\cr
#'   A list with two slot \code{data}, containing either a \code{data.frame} or a \code{task},
#'   depending on the object passed to the function. A list named \code{value.table} containing
#'   (named) numeric vectors of the replacement values.
#' @export
#' @family eda_and_preprocess
createImpactFeatures = function(obj, target = character(0L), cols = NULL, fun = NULL) {
  mlr:::checkTargetPreproc(obj, target, cols)
  UseMethod("createImpactFeatures")
}

createImpactFeatures.data.frame = function(obj, target = character(0L), cols = NULL, fun = NULL) {
  # get all factor feature names present in data
  work.cols = colnames(obj)[vlapply(obj, is.factor)]
  work.cols = setdiff(work.cols, target)

  # check that user requested cols are only factor cols
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
    work.cols = cols
  }

  value.table = lapply(work.cols, function(col) {
    res = aggregate(target.vec, by = list(obj[, col]), fun)
    r = res[,2]
    names(r) = res[,1]
    r
  })
  names(value.table) = work.cols

  for (wc in work.cols) {
    levels(obj[,wc]) = value.table[[wc]]
    obj[,wc] = as.numeric(as.character(obj[,wc]))
  }

  return(list(
    data = obj,
    value.table = value.table))
}

createImpactFeatures.Task = function(obj, target = character(0L), cols = NULL, fun = NULL) {
  target = getTaskTargetNames(obj)
  d = createImpactFeatures.data.frame(obj = getTaskData(obj), target = target, cols = cols)
  return(list(
    data = mlr:::changeData(obj, d$data),
    value.table = d$value.table))

}
