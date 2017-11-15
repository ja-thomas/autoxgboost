#' @title Fuse learner with impact feature creator.
#'
#' @description
#' Fuses a base learner with the impact feature creator (see \code{\link{createImpactFeatures}}).
#' Returns a learner which can be used like any other learner.
#'
#' @param learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'   The learner.
#'   If you pass a string the learner will be created via \code{\link{makeLearner}}.
#' @inheritParams createImpactFeatures
#' @return [\code{\link{Learner}}].
#' @export
makeImpactFeaturesWrapper = function(learner, cols = NULL) {
  learner = mlr:::checkLearner(learner)
  args = list(cols = cols)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    data = createImpactFeatures(data, target, cols = args$cols)
    return(list(data = data$data, control = list(value.table = data$value.table)))
  }

  predictfun = function(data, target, args, control) {
    data = createImpactFeatures(data, target, cols = args$cols)
    return(data$data)
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
  lrn$id = stringi::stri_replace(lrn$id, replacement = ".impact", regex = "\\.preproc$")
  addClasses(lrn, "ImpactFeatureWrapper")

}

#' @export
getLearnerProperties.ImpactFeatureWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
