#' @title Result of a autoxgboost call.
#'
#' @description
#' Result of \code{\link{autoxgboost}}. A list containing the following elements:
#' \describe{
#'   \item{optim.result [\code{\link[mlrMBO]{MBOSingleObjResult}}]}{Optimization result object. See: \code{\link[mlrMBO]{MBOSingleObjResult}}}
#'   \item{final.learner [\code{\link[mlr]{Learner}}]}{Xgboost learner with best found hyper paramater configuration.}
#'   \item{final.model [\code{\link[mlr]{WrappedModel}} | \code{NULL}]}{If \code{build.final.model=TRUE} in \code{\link{autoxgboost}} a \pkg{mlr} model build by the full dataset and \code{final.learner}.}
#'   \item{measure [\code{\link[mlr]{Measure}}]}{Measure used for optimization.}
#' }
#' @name AutoxgbResult
#' @rdname AutoxgbResult
NULL



#' @export
print.AutoxgbResult = function(x, ...) {
  op = x$optim.result$opt.path
  pars = trafoValue(op$par.set, x$optim.result$x)
  pars$nrounds = getBestNrounds(x$optim.result)
  catf("Autoxgboost tuning result\n")
  catf("Recommended parameters:")
  for (p in names(pars)) {
    if (p != "nrounds" && isNumeric(op$par.set$pars[[p]], include.int = FALSE)) {
      catf("%s: %.3f", stringi::stri_pad_left(p, width = 17), pars[p])
    } else {
      catf("%s: %i", stringi::stri_pad_left(p, width = 17), as.integer(pars[p]))
    }
  }
  catf("With tuning result: %s = %.3f\n", op$y.names[1], x$y)

}


#' @export
predict.AutoxgbResult = function(object, newdata, ...) {
  if (is.null(object$final.model))
    stop("Final model was not build, use best param configs to build the model yourself.")
  predict(object$final.model, newdata = newdata, ...)
}
