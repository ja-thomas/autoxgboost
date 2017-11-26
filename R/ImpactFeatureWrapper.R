#' @title Fuse learner with impact feature creator.
#'
#' @description
#' Fuses a base learner with the impact feature creator (see \code{\link{createImpactFeatures}}).
#' Returns a learner which can be used like any other learner.
#'
#' @param learner [\code{\link{Learner}} | \code{character(1)}]\cr
#'   The learner.
#'   If you pass a string the learner will be created via \code{\link{makeLearner}}.
#' @param slope.param [\code{integer}]\cr
#'   Controls the rate of transition rate \eqn{\lambda}. Default is \code{100L}.
#' @param trust.param [\code{integer}]\cr
#'   Determines half of the minimal sample size for which we completely "trust" the conditional 
#'   probability of a factor level. Default is \code{100L}
#' @inheritParams createImpactFeatures
#' @return [\code{\link{Learner}}].
#' @export
makeImpactFeaturesWrapper = function(learner, cols = NULL, fun = NULL, slope.param = 100L, trust.param = 100L) {
  learner = mlr:::checkLearner(learner)
  args = list(cols = cols, fun = fun, slope.param = slope.param, trust.param = trust.param)
  rm(list = names(args))
  
  trainfun = function(data, target, args) {
    data = createImpactFeatures(data, target, cols = args$cols, fun = args$fun,
      slope.param = args$slope.param, trust.param = args$trust.param))
return(list(data = data$data, control = list(value.table = data$value.table,
  prior.table = data$prior.table)))
  }
  
  predictfun = function(data, target, args, control) {
    
    value.table = control$value.table
    prior.table = control$prior.table
    work.cols = names(value.table)
    
    for (wc in work.cols) {
      tab = value.table[[wc]]
      if (ncol(tab) == 2) {# regression or binary classif
        data[, wc] = as.factor(data[, wc])
        levels(data[, wc]) = tab[, 2]
        n = as.numeric(table(data[, wc])[as.character(data[, wc])])
        data[, wc] = impactEncodingLambda(n, args$slope.param, args$trust.param) * as.numeric(as.character(data[, wc])) + (1 - impactEncodingLambda(n, args$slope.param, args$trust.param)) * prior.table
      } else { # multiclass classif
        new.cols = paste(wc, colnames(tab[, -1]), sep = ".")
        data[, new.cols] = data[, wc]
        data[, wc] = NULL
        for(i in seq_along(new.cols)) {
          data[, new.cols[i]] = as.factor(data[, new.cols[i]])
          levels(data[, new.cols[i]]) = tab[order(as.character(tab[, 1])), i + 1]
          n = as.numeric(table(data[, new.cols[i]])[as.character(data[, new.cols[i]])])
          data[, new.cols[i]] = impactEncodingLambda(n, args$slope.param, args$trust.param) * as.numeric(as.character(data[, new.cols[i]])) + (1 - impactEncodingLambda(n, args$slope.param, args$trust.param)) * prior.table[i]
        }
      }
    }
    
    return(data)
  }
  
  lrn = makePreprocWrapper(learner, trainfun, predictfun, par.vals = args)
  lrn$id = stringi::stri_replace(lrn$id, replacement = ".impact", regex = "\\.preproc$")
  addClasses(lrn, "ImpactFeatureWrapper")
  
}

#' @export
getLearnerProperties.ImpactFeatureWrapper = function(learner) {
  union(getLearnerProperties(learner$next.learner), c("factors", "ordered"))
}
