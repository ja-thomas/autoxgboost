#' @title autoxgboost default parameter set.
#'
#' @description
#'  This is the default parameter set for xgboost that is used for tuning if no costum parameter set is defined by the user.
#'  For a documentation of the parameter see \url{https://github.com/dmlc/xgboost/blob/master/doc/parameter.md}.
#'  By default this set is used:
#'  \describe{
#'    \item{eta}{Learning rate: Between \code{0.01} and \code{0.2}.}
#'    \item{gamma}{Minimum loss required for a split: Between \code{2^-7 = 0.0078125} and \code{2^6 = 64}.}
#'    \item{max_depth}{Maximum tree depth: Between \code{3} and \code{20}.}
#'    \item{colsample_bytree}{Subsample ratio of columns per tree: Between \code{0.5} and \code{1}.}
#'    \item{colsample_bylevel}{Subsample ratio of clumns per tree level: Between \code{0.5} and \code{1}.}
#'    \item{lambda}{L2 Regularization: Between \code{2^-10 = 0.00097...} and \code{2^10 = 1024}.}
#'    \item{alpha}{L1 Regularization: Between \code{2^-10 = 0.00097...} and \code{2^10 = 1024}.}
#'    \item{subsample}{Subsample fraction: Between \code{0.5} and \code{1}.}
#' }
#' @name autoxgbparset
#' @rdname autoxgbparset
#' @export
NULL

autoxgbparset = makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.2),
  makeNumericParam("gamma", lower = -7, upper = 6, trafo = function(x) 2^x),
  makeIntegerParam("max_depth", lower = 3, upper = 20),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
  makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("subsample", lower = 0.5, upper = 1)
)
