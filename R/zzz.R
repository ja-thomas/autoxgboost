#' @import mlr
#' @import ParamHelpers
#' @import smoof
#' @import mlrMBO
#' @import xgboost
#' @import BBmisc
#' @import checkmate
#' @importFrom stats predict
#' @importFrom stats runif
#' @importFrom stats aggregate


registerS3method("makeRLearner", "regr.autoxgboost", makeRLearner.regr.autoxgboost)
registerS3method("trainLearner", "regr.autoxgboost", trainLearner.regr.autoxgboost)
registerS3method("predictLearner", "regr.autoxgboost", predictLearner.regr.autoxgboost)

registerS3method("makeRLearner", "classif.autoxgboost", makeRLearner.classif.autoxgboost)
registerS3method("trainLearner", "classif.autoxgboost", trainLearner.classif.autoxgboost)
registerS3method("predictLearner", "classif.autoxgboost", predictLearner.classif.autoxgboost)
