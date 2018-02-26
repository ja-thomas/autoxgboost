#' @title Fuse learner with integer feature creator.
#'
#' @description
#' Changes all categorical features to integer values.
#'
#' @param learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'   The learner.
#'   If you pass a string the learner will be created via \code{\link{makeLearner}}.
#' @param cols [\code{character}]\cr
#'   Columns to create integer features for. Default is to use all columns.
#' @return [\code{\link{Learner}}].
#' @export
makeIntegerFeaturesWrapper = function(learner, cols = NULL) {
  learner = mlr:::checkLearner(learner)
  args = list(cols = cols)
  rm(list = names(args))

  trainfun = function(data, target, args) {

    if (is.null(args$cols)) {
      work.cols = colnames(data)[vlapply(data, is.factor)]
      work.cols = setdiff(work.cols, target)
    } else {
      work.cols = args$cols
    }

    for (col in work.cols) {
      data[, col] = as.numeric(data[, col])
    }

    return(list(data = data, control = list(work.cols = work.cols)))
  }

  predictfun = function(data, target, args, control) {

    for (col in control$work.cols) {
      data[, col] = as.numeric(data[, col])
    }

    return(data)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
  lrn$id = stringi::stri_replace(lrn$id, replacement = ".integered", regex = "\\.preproc$")
  addClasses(lrn, "IntegerFeatureWrapper")

}

#' @export
getLearnerProperties.IntegerFeatureWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
