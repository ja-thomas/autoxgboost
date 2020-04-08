# This generates a preprocessing pipeline to handle categorical features
# @param task: the task
# @param impact.encoding.boundary: See autoxgboost
# @return CPOpipeline to transform categorical features
generateCatFeatPipeline = function(task, impact.encoding.boundary) {

  cat.pipeline = cpoFixFactors()

  d = getTaskData(task, target.extra = TRUE)$data
  feat.cols = colnames(d)[vlapply(d, is.factor)]
  #categ.featureset = task$feature.information$categ.featureset
  #if (!is.null(categ.featureset)) {
  #  for(cf in categ.featureset)
  #    cat.pipeline %<>>% cpoFeatureHashing(affect.names = cf)
  # feat.cols = setdiff(feat.cols, unlist(categ.featureset))
  #}

  impact.cols = colnames(d)[vlapply(d, function(x) is.factor(x) && nlevels(x) > impact.encoding.boundary)]
  dummy.cols = setdiff(feat.cols, impact.cols)

  if (length(dummy.cols) > 0L)
      cat.pipeline %<>>% cpoDummyEncode(affect.names = dummy.cols, infixdot = TRUE)
  if (length(impact.cols) > 0L) {
    if (getTaskType(task) == "classif") {
      cat.pipeline %<>>% cpoImpactEncodeClassif(affect.names = impact.cols)
    } else {
      cat.pipeline %<>>% cpoImpactEncodeRegr(affect.names = impact.cols)
    }
  }
  return(cat.pipeline)
}
