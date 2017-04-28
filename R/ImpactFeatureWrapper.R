#' @title Fuse learner with impact feature creator.
#'
#' @description
#' Fuses a base learner with the impact feature creator (see \code{\link{createImpactFeatures}}).
#' Returns a learner which can be used like any other learner.
#'
#' @template arg_learner
#' @inheritParams createImpactFeatures
#' @template ret_learner
#' @family wrapper
#' @export
makeImpactFeaturesWrapper = function(learner, cols = NULL, fun = NULL) {
  learner = checkLearner(learner)
  args = list(cols = cols, fun = fun)
  rm(list = names(args))

  trainfun = function(data, target, args = args) {
    data = createImpactFeatures(data, target, cols = args$cols, fun = args$fun)
    return(list(data = data$data, control = list(value.table = data$value.table)))
  }
  predictfun = function(data, target, args, control) {

    value.table = control$value.table
    work.cols = names(value.table)

    for (wc in work.cols) {
      levels(data[,wc]) = value.table[[wc]]
      data[,wc] = as.numeric(as.character(data[,wc]))
    }

    return(data)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun)
  lrn$id = stri_replace(lrn$id, replacement = ".impact", regex = "\\.preproc$")
  addClasses(lrn, "ImpactFeatureWrapper")

}

getLearnerProperties.ImpactFeatureWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
