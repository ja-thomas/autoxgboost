#' @title Fuse learner with impact feature creator.
#'
#' @description
#' Fuses a base learner with the impact feature creator (see \code{\link{createImpactFeatures}}).
#' Returns a learner which can be used like any other learner.
#'
#' @param learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'   The learner.
#'   If you pass a string the learner will be created via \code{\link{makeLearner}}.
#' @param slope.param [\code{numeric}]\cr
#'   Controls the rate of transition \eqn{\lambda} (see \code{\link{createImpactFeatures}}). Default is \code{100L}.
#' @param trust.param [\code{numeric}]\cr
#'   Determines half of the minimal sample size for which we completely "trust" the conditional 
#'   probability of a factor level (see \code{\link{createImpactFeatures}}). Default is \code{100L}
#' @inheritParams createImpactFeatures
#' @return [\code{\link{Learner}}].
#' @export
makeImpactFeaturesWrapper = function(learner, cols = NULL, fun = NULL, slope.param = 100, trust.param = 100) {
  learner = mlr:::checkLearner(learner)

  args = list(cols = cols, fun = fun)

  assertNumber(slope.param, lower = 0)
  assertNumber(trust.param, lower = 0)
  args$slope.param = slope.param
  args$trust.param = trust.param

  rm(list = names(args))
  
  ps = makeParamSet(
    makeNumericParam(id = "slope.param", lower = 0, upper = Inf),
    makeNumericParam(id = "trust.param", lower = 0, upper = Inf)
  )
  
  trainfun = function(data, target, args) {
    data = createImpactFeatures(data, target, cols = args$cols, fun = args$fun,
      slope.param = args$slope.param, trust.param = args$trust.param)
    return(list(data = data$data, control = list(value.table = data$value.table,
      prior.table = data$prior.table)))
  }

  predictfun = function(data, target, args, control) {

    # replace values in the data
    data = impactEncoding(data, names(control$value.table), control$value.table,
      control$prior.table, args$slope.param, args$trust.param)
    return(data)
    
  }

  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.set = ps, par.vals = args)
  lrn$id = stringi::stri_replace(lrn$id, replacement = ".impact", regex = "\\.preproc$")
  addClasses(lrn, "ImpactFeatureWrapper")

}

#' @export
getLearnerProperties.ImpactFeatureWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
