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
makeImpactFeaturesWrapper = function(learner, cols = NULL, fun = NULL) {
  learner = mlr:::checkLearner(learner)
  args = list(cols = cols, fun = fun)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    data = createImpactFeatures(data, target, cols = args$cols, fun = args$fun)
    return(list(data = data$data, control = list(value.table = data$value.table)))
  }

  predictfun = function(data, target, args, control) {

    value.table = control$value.table
    work.cols = names(value.table)

    for (wc in work.cols) {
      tab = value.table[[wc]]
      if (ncol(tab) == 2) {
        levels(data[,wc]) = tab[,2]
        data[,wc] = as.numeric(as.character(data[,wc]))
      } else { # for multiclass classif
        new.cols = paste(wc, colnames(tab)[-1], sep = ".")
        data[, new.cols] = data[, wc]
        data[, wc] = NULL
        for(i in seq_along(new.cols)) {
          data[, new.cols[i]] = as.factor(data[, new.cols[i]])
          levels(data[, new.cols[i]]) = tab[, i + 1]
          data[,new.cols[i]] = as.numeric(as.character(data[, new.cols[i]]))
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
