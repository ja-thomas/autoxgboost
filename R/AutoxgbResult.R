#' @title Result of a autoxgboost call.
#'
#' @description
#' \itemize{
#'   \item{optim.result [\code{\link[mlrMBO]{MBOResult}}]}{Optimization result object. See: \code{\link[mlrMBO]{MBOResult}}}
#'   \item{optim.result [\code{\link[mlr]{MBOResult}}]}{Optimization result object. See: \code{\link[mlrMBO]{MBOResult}}}
#'
#' }
#'
#'
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
      catf("%s: %.3f", stri_pad_left(p, width = 17), pars[p])
    } else {
      catf("%s: %i", stri_pad_left(p, width = 17), as.integer(pars[p]))
    }
  }
  catf("With tuning result: %s = %.3f\n", op$y.names[1], x$y)

}
