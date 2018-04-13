#' @title Add, list and remove feature information
#'
#' @description
#' Add additional information about features to a task. \code{\link{autoxgboost}} will
#' do feature engineering based on this information.
#'
#' @param task [\code{\link[mlr]{Task}}]\cr
#'   The task.
#' @param which [character(1)]\cr
#'   Kind of feature information can be \code{timestamp}, \code{categ.featureset}, \code{numeric.featureset}.
#' @param features [character()]\cr
#'   Features to be added to the kind of feature information.
#' @return [\code{\link[mlr]{Task}}]
#' @export
#' @examples
#' \donttest{
#' iris.task = makeClassifTask(data = iris, target = "Species")
#' iris.task = addFeatureInformation(iris.task, "numeric.featuresets",
#'   c("Sepal.Length", "Sepal.Width"))
#' listFeatureInformation(iris.task)
#' removeFeatureInformation(iris.task)
#' }
addFeatureInformation = function(task, which, features) {
  assertClass(task, "SupervisedTask")
  assertChoice(which, choices = c("timestamps", "categ.featuresets", "numeric.featuresets"))
  assertSubset(features, getTaskFeatureNames(task))
  assertCharacter(features)
  if(is.null(task$feature.information[[which]])) {
    task$feature.information[[which]] = list(features)
  } else if (list(features) %in% task$feature.information[[which]]) {
    stop("Feature information already present")
  }
  else {
    task$feature.information[[which]] = c(task$feature.information[[which]], list(features))
  }
  task
}

#' @describeIn addFeatureInformation list feature information
#' @export
listFeatureInformation = function(task) {
  assertClass(task, "SupervisedTask")
  for (which in c("timestamps", "categ.featuresets", "numeric.featuresets")) {
    x = task$feature.information[[which]]
    catf("%s: %s", which, collapse(unlist(lapply(x, paste, collapse = ", ")), sep = " and "))
  }
  invisible(NULL)
}

#' @describeIn addFeatureInformation delete feature information
#' @export
removeFeatureInformation = function(task) {
  task$feature.information = NULL
  task
}
