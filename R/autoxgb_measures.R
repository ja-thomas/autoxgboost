###############################################################################
### Regression ###
###############################################################################

#' @export autoxgbSSE
#' @rdname autoxgbMeasures
#' @format none
autoxgbSSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureSSE(truth, preds)
  return(list(metric = "sse", value = err))
}

#' @export autoxgbMSE
#' @rdname autoxgbMeasures
#' @format none
autoxgbMSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMSE(truth, preds)
  return(list(metric = "mse", value = err))
}

#' @export autoxgbRMSE
#' @rdname autoxgbMeasures
#' @format none
autoxgbRMSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRMSE(truth, preds)
  return(list(metric = "rmse", value = err))
}

#' @export autoxgbMEDSE
#' @rdname autoxgbMeasures
#' @format none
autoxgbMEDSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMEDSE(truth, preds)
  return(list(metric = "medse", value = err))
}

#' @export autoxgbSAE
#' @rdname autoxgbMeasures
#' @format none
autoxgbSAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureSAE(truth, preds)
  return(list(metric = "sae", value = err))
}

#' @export autoxgbMAE
#' @rdname autoxgbMeasures
#' @format none
autoxgbMAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMAE(truth, preds)
  return(list(metric = "mae", value = err))
}
  
#' @export autoxgbMEDAE
#' @rdname autoxgbMeasures
#' @format none
autoxgbMEDAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMEDAE(truth, preds)
  return(list(metric = "medae", value = err))
}

 
#' @export autoxgbRSQ
#' @rdname autoxgbMeasures
#' @format none
autoxgbRSQ = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRSQ(truth, preds)
  return(list(metric = "rsq", value = err))
}


#' @export autoxgbEXPVAR
#' @rdname autoxgbMeasures
#' @format none
autoxgbEXPVAR = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureEXPVAR(truth, preds)
  return(list(metric = "expvar", value = err))
}

#' @export autoxgbADJRSQ
#' @rdname autoxgbMeasures
#' @format none
autoxgbADJRSQ = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  n = length(truth)
  p = length(dim(dtrain)[2] - 1)
  if (n == p + 1){
    warning("Adjusted R-squared is undefined if the number observations is equal to the number of independent variables plus one.")
    return(list(metric = "arsq", value = NA_real_))
  } else {
  err = 1 - (1 - measureRSQ(truth, preds)) * (p / (n - p - 1L))
  return(list(metric = "arsq", value = err))
  }
}

#' @export autoxgbRRSE
#' @rdname autoxgbMeasures
#' @format none
autoxgbRRSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRRSE(truth, preds)
  return(list(metric = "rrse", value = err))
}

#' @export autoxgbRAE
#' @rdname autoxgbMeasures
#' @format none
autoxgbRAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRAE(truth, preds)
  return(list(metric = "rae", value = err))
}

#' @export autoxgbMAPE
#' @rdname autoxgbMeasures
#' @format none
autoxgbMAPE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMAPE(truth, preds)
  return(list(metric = "mape", value = err))
}

#' @export autoxgbMSLE
#' @rdname autoxgbMeasures
#' @format none
autoxgbMSLE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMSLE(truth, preds)
  return(list(metric = "msle", value = err))
}

#' @export autoxgbRMSLE
#' @rdname autoxgbMeasures
#' @format none
autoxgbRMSLE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = sqrt(measureMSLE(truth, preds))
  return(list(metric = "rmsle", value = err))
}

#' @export autoxgbKENDALLTAU
#' @rdname autoxgbMeasures
#' @format none
autoxgbKENDALLTAU = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureKendallTau(truth, preds)
  return(list(metric = "kendalltau", value = err))
}

#' @export autoxgbSPEARMANRHO
#' @rdname autoxgbMeasures
#' @format none
autoxgbSPEARMANRHO = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureSpearmanRho(truth, preds)
  return(list(metric = "spearmanrho", value = err))
}

###############################################################################
### classif multi ###
###############################################################################
#' @export autoxgbMMCE
#' @rdname autoxgbMeasures
#' @format none
autoxgbMMCE = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  err = measureMMCE(truth, preds)
  return(list(metric = "mmce", value = err))
}

#' @export autoxgbACC
#' @rdname autoxgbMeasures
#' @format none
autoxgbACC = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  err = measureACC(truth, preds)
  return(list(metric = "acc", value = err))
}

#' @export autoxgbBER
#' @rdname autoxgbMeasures
#' @format none
autoxgbBER = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  # special case for predictions from FailureModel
  if (anyMissing(preds)) {
    return(list(metric = "ber", value = NA_real_))
  } else {
  n = length(levels(truth)) + 1L
  err = mean(calculateConfusionMatrix(pred, relative = TRUE)$relative.row[-n, n])
  }
  return(list(metric = "ber", value = err))
}

#' @export autoxgbMULTICLASS.AUNU
#' @rdname autoxgbMeasures
#' @format none
autoxgbMULTICLASS.AUNU = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAUNU(preds, truth)
  return(list(metric = "multiclass.aunu", value = err))
}

#' @export autoxgbMULTICLASS.AUNP
#' @rdname autoxgbMeasures
#' @format none
autoxgbMULTICLASS.AUNP = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAUNP(preds, truth)
  return(list(metric = "multiclass.aunp", value = err))
}

#' @export autoxgbMULTICLASS.AU1U
#' @rdname autoxgbMeasures
#' @format none
autoxgbMULTICLASS.AU1U = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAU1U(preds, truth)
  return(list(metric = "multiclass.au1u", value = err))
}

#' @export autoxgbMULTICLASS.AU1P
#' @rdname autoxgbMeasures
#' @format none
autoxgbMULTICLASS.AU1P = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAU1P(preds, truth)
  return(list(metric = "multiclass.au1p", value = err))
}

#' @export autoxgbMULTICLASS.BRIER
#' @rdname autoxgbMeasures
#' @format none
autoxgbMULTICLASS.BRIER = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureMulticlassBrier(preds, truth)
  return(list(metric = "multiclass.brier", value = err))
}

#' @export autoxgbLOGLOSS
#' @rdname autoxgbMeasures
#' @format none
autoxgbLOGLOSS = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureLogloss(preds, truth)
  return(list(metric = "logloss", value = err))
}

#' @export autoxgbSSR
#' @rdname autoxgbMeasures
#' @format none
autoxgbSSR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureSSR(preds, truth)
  return(list(metric = "ssr", value = err))
}

 
#' #' @export qsr
#' #' @rdname measures
#' #' @format none
#' qsr = makeMeasure(id = "qsr", minimize = FALSE, best = 1, worst = -1,
#'   properties = c("classif", "classif.multi", "req.truth", "req.prob"),
#'   name = "Quadratic Scoring Rule",
#'   note = "Defined as: 1 - (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), and p_ij is the predicted probablity of observation i for class j.
#'   This scoring rule is the same as 1 - multiclass.brier.
#'   See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic scoring rules. Decision Analysis, 4(2), 49-65.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureQSR(getPredictionProbabilities(pred, cl = pred$task.desc$class.levels), pred$data$truth)
#'   }
#' )
#' 
#' #' @export measureQSR
#' #' @rdname measures
#' #' @format none
#' measureQSR = function(probabilities, truth){
#'   #We add this line because binary tasks only output one probability column
#'   if (is.null(dim(probabilities))) probabilities = cbind(probabilities, 1 - probabilities)
#'   truth = factor(truth, levels = colnames(probabilities))
#'   1 - mean(rowSums((probabilities - createDummyFeatures(truth))^2))
#' }
#' 
#' #' @export lsr
#' #' @rdname measures
#' #' @format none
#' lsr = makeMeasure(id = "lsr", minimize = FALSE, best = 0, worst = -Inf,
#'   properties = c("classif", "classif.multi", "req.truth", "req.prob"),
#'   name = "Logarithmic Scoring Rule",
#'   note = "Defined as: mean(log(p_i)), where p_i is the predicted probability of the true class of observation i.
#'   This scoring rule is the same as the negative logloss, self-information or surprisal.
#'   See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic scoring rules. Decision Analysis, 4(2), 49-65.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureLSR(getPredictionProbabilities(pred, cl = pred$task.desc$class.levels), pred$data$truth)
#'   }
#' )
#' 
#' #' @export measureLSR
#' #' @rdname measures
#' #' @format none
#' measureLSR = function(probabilities, truth){
#'   -1 * measureLogloss(probabilities, truth)
#' }
#' 
#' #' @export kappa
#' #' @rdname measures
#' #' @format none
#' kappa = makeMeasure(id = "kappa", minimize = FALSE, best = 1, worst = -1,
#'   properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#'   name = "Cohen's kappa",
#'   note = "Defined as: 1 - (1 - p0) / (1 - pe). With: p0 = 'observed frequency of
#'   agreement' and pe = 'expected agremeent frequency under independence",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureKAPPA(pred$data$truth, pred$data$response)
#'   }
#' )
#' 
#' #' @export measureKAPPA
#' #' @rdname measures
#' #' @format none
#' measureKAPPA = function(truth, response) {
#'   # get confusion matrix
#'   conf.mat = table(truth, response)
#'   conf.mat = conf.mat / sum(conf.mat)
#'   
#'   # observed agreement frequency
#'   p0 = sum(diag(conf.mat))
#'   
#'   # get expected probs under independence
#'   rowsum = rowSums(conf.mat)
#'   colsum = colSums(conf.mat)
#'   pe = sum(rowsum * colsum) / sum(conf.mat)^2
#'   
#'   # calculate kappa
#'   1 - (1 - p0) / (1 - pe)
#' }
#' 
#' #' @export wkappa
#' #' @rdname measures
#' #' @format none
#' wkappa = makeMeasure(id = "wkappa", minimize = FALSE, best = 1, worst = -1,
#'   properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#'   name = "Mean quadratic weighted kappa",
#'   note = "Defined as: 1 - sum(weights * conf.mat) / sum(weights * expected.mat),
#'   the weight matrix measures seriousness of disagreement with the squared euclidean metric.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureWKAPPA(pred$data$truth, pred$data$response)
#'   }
#' )
#' 
#' #' @export measureWKAPPA
#' #' @rdname measures
#' #' @format none
#' measureWKAPPA = function(truth, response) {
#'   # get confusion matrix
#'   conf.mat = table(truth, response)
#'   conf.mat = conf.mat / sum(conf.mat)
#'   
#'   # get expected probs under independence
#'   rowsum = rowSums(conf.mat)
#'   colsum = colSums(conf.mat)
#'   expected.mat = rowsum %*% t(colsum)
#'   
#'   # get weights
#'   class.values = seq_along(levels(truth)) - 1L
#'   weights = outer(class.values, class.values, FUN = function(x, y) (x - y)^2)
#'   
#'   # calculate weighted kappa
#'   1 - sum(weights * conf.mat) / sum(weights * expected.mat)
#' }
#' 
#' ###############################################################################
#' ### classif binary ###
#' ###############################################################################
#' #' @export auc
#' #' @rdname measures
#' #' @format none
#' auc = makeMeasure(id = "auc", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth", "req.prob"),
#'   name = "Area under the curve",
#'   note = "Integral over the graph that results from computing fpr and tpr for many different thresholds.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     if (anyMissing(pred$data$response) || length(unique(pred$data$truth)) == 1L)
#'       return(NA_real_)
#'     measureAUC(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureAUC
#' #' @rdname measures
#' #' @format none
#' measureAUC = function(probabilities, truth, negative, positive) {
#'   if (is.factor(truth)) {
#'     i = as.integer(truth) == which(levels(truth) == positive)
#'   } else {
#'     i = truth == positive
#'   }
#'   if (length(unique(i)) < 2L) {
#'     stop("truth vector must have at least two classes")
#'   }
#'   #Use fast ranking function from data.table for larger vectors
#'   if (length(i) > 5000L) {
#'     r = frankv(probabilities)
#'   } else {
#'     r = rank(probabilities)
#'   }
#'   n.pos = as.numeric(sum(i))
#'   n.neg = length(i) - n.pos
#'   (sum(r[i]) - n.pos * (n.pos + 1) / 2) / (n.pos * n.neg)
#' }
#' 
#' #' @export brier
#' #' @rdname measures
#' #' @format none
#' brier = makeMeasure(id = "brier", minimize = TRUE, best = 0, worst = 1,
#'   properties = c("classif", "req.pred", "req.truth", "req.prob"),
#'   name = "Brier score",
#'   note = "The Brier score is defined as the quadratic difference between the probability and the value (1,0) for the class.
#'   That means we use the numeric representation 1 and 0 for our target classes. It is similiar to the mean squared error in regression.
#'   multiclass.brier is the sum over all one vs. all comparisons and for a binary classifcation 2 * brier.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureBrier(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureBrier
#' #' @rdname measures
#' #' @format none
#' measureBrier = function(probabilities, truth, negative, positive) {
#'   y = as.numeric(truth == positive)
#'   mean((y - probabilities)^2)
#' }
#' 
#' #' @export brier.scaled
#' #' @rdname measures
#' #' @format none
#' brier.scaled = makeMeasure(id = "brier.scaled", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth", "req.prob"),
#'   name = "Brier scaled",
#'   note = "Brier score scaled to [0,1], see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3575184/.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureBrierScaled(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureBrierScaled
#' #' @rdname measures
#' #' @format none
#' measureBrierScaled = function(probabilities, truth, negative, positive) {
#'   y = as.numeric(truth == positive)
#'   brier = mean((y - probabilities)^2)
#'   inc = mean(probabilities)
#'   brier.max = inc * (1 - inc)^2 + (1 - inc) * inc^2
#'   1 - brier / brier.max
#' }
#' 
#' #' @export bac
#' #' @rdname measures
#' #' @format none
#' bac = makeMeasure(id = "bac", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "Balanced accuracy",
#'   note = "Mean of true positive rate and true negative rate.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     mean(c(tp$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$positive),
#'       tn$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$negative)))
#'   }
#' )
#' 
#' #' @export measureBAC
#' #' @rdname measures
#' #' @format none
#' measureBAC = function(truth, response, negative, positive) {
#'   mean(c(
#'     measureTP(truth, response, positive) / sum(truth == positive),
#'     measureTN(truth, response, negative) / sum(truth == negative)
#'   ))
#' }
#' 
#' #' @export tp
#' #' @rdname measures
#' #' @format none
#' tp = makeMeasure(id = "tp", minimize = FALSE, best = Inf, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "True positives",
#'   note = "Sum of all correctly classified observations in the positive class.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureTP(pred$data$truth, pred$data$response, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureTP
#' #' @rdname measures
#' #' @format none
#' measureTP = function(truth, response, positive) {
#'   sum(truth == response & response == positive)
#' }
#' 
#' #' @export tn
#' #' @rdname measures
#' #' @format none
#' tn = makeMeasure(id = "tn", minimize = FALSE, best = Inf, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "True negatives",
#'   note = "Sum of correctly classified observations in the negative class. Also called correct rejections.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureTN(pred$data$truth, pred$data$response, pred$task.desc$negative)
#'   }
#' )
#' 
#' #' @export measureTN
#' #' @rdname measures
#' #' @format none
#' measureTN = function(truth, response, negative) {
#'   sum(truth == response & response == negative)
#' }
#' 
#' #' @export fp
#' #' @rdname measures
#' #' @format none
#' fp = makeMeasure(id = "fp", minimize = TRUE, best = 0, worst = Inf,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "False positives",
#'   note = "Sum of misclassified observations in the positive class. Also called false alarms.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureFP(pred$data$truth, pred$data$response, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureFP
#' #' @rdname measures
#' #' @format none
#' measureFP = function(truth, response, positive) {
#'   sum(truth != response & response == positive)
#' }
#' 
#' #' @export fn
#' #' @rdname measures
#' #' @format none
#' fn = makeMeasure(id = "fn", minimize = TRUE, best = 0, worst = Inf,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "False negatives",
#'   note = "Sum of misclassified observations in the negative class. Also called misses.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureFN(pred$data$truth, pred$data$response, pred$task.desc$negative)
#'   }
#' )
#' 
#' #' @export measureFN
#' #' @rdname measures
#' #' @format none
#' measureFN = function(truth, response, negative) {
#'   sum(truth != response & response == negative)
#' }
#' 
#' #' @export tpr
#' #' @rdname measures
#' #' @format none
#' tpr = makeMeasure(id = "tpr", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "True positive rate",
#'   note = "Percentage of correctly classified observations in the positive class. Also called hit rate or recall.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureTPR(pred$data$truth, pred$data$response, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureTPR
#' #' @rdname measures
#' #' @format none
#' measureTPR = function(truth, response, positive) {
#'   measureTP(truth, response, positive) / sum(truth == positive)
#' }
#' 
#' #' @export tnr
#' #' @rdname measures
#' #' @format none
#' tnr = makeMeasure(id = "tnr", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "True negative rate",
#'   note = "Percentage of correctly classified observations in the negative class. Also called specificity.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureTNR(pred$data$truth, pred$data$response, pred$task.desc$negative)
#'   }
#' )
#' 
#' #' @export measureTNR
#' #' @rdname measures
#' #' @format none
#' measureTNR = function(truth, response, negative) {
#'   measureTN(truth, response, negative) / sum(truth == negative)
#' }
#' 
#' #' @export fpr
#' #' @rdname measures
#' #' @format none
#' fpr = makeMeasure(id = "fpr", minimize = TRUE, best = 0, worst = 1,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "False positive rate",
#'   note = "Percentage of misclassified observations in the positive class. Also called false alarm rate or fall-out.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureFPR(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureFPR
#' #' @rdname measures
#' #' @format none
#' measureFPR = function(truth, response, negative, positive) {
#'   measureFP(truth, response, positive) / sum(truth == negative)
#' }
#' 
#' #' @export fnr
#' #' @rdname measures
#' #' @format none
#' fnr = makeMeasure(id = "fnr", minimize = TRUE, best = 0, worst = 1,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "False negative rate",
#'   note = "Percentage of misclassified observations in the negative class.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureFNR(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureFNR
#' #' @rdname measures
#' #' @format none
#' measureFNR = function(truth, response, negative, positive) {
#'   measureFN(truth, response, negative) / sum(truth == positive)
#' }
#' 
#' #' @export ppv
#' #' @rdname measures
#' #' @format none
#' ppv = makeMeasure(id = "ppv", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "Positive predictive value",
#'   note = "Defined as: tp / (tp + number of fp). Also called precision. If the denominator is 0, PPV is set to be either 1 or 0 depending on whether the highest probability prediction is positive (1) or negative (0).",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     if (pred$predict.type == "prob") {
#'       prob = getPredictionProbabilities(pred)
#'     } else {
#'       prob = NULL
#'     }
#'     measurePPV(pred$data$truth, pred$data$response, pred$task.desc$positive, prob)
#'   }
#' )
#' 
#' #' @export measurePPV
#' #' @rdname measures
#' #' @format none
#' 
#' measurePPV = function(truth, response, positive, probabilities = NULL) {
#'   denominator = sum(response == positive)
#'   ifelse(denominator == 0, measureEdgeCase(truth, positive, probabilities), measureTP(truth, response, positive) / denominator)
#' }
#' measureEdgeCase = function(truth, positive, prob) {
#'   if (!is.null(prob)) {
#'     rs = sort(prob, index.return = TRUE)
#'     erst = ifelse(truth[getLast(rs$ix)] == positive, 1, 0)
#'   } else {
#'     erst = NA
#'   }
#'   erst
#' }
#' 
#' 
#' #' @export npv
#' #' @rdname measures
#' #' @format none
#' npv = makeMeasure(id = "npv", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "Negative predictive value",
#'   note = "Defined as: (tn) / (tn + fn).",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureNPV(pred$data$truth, pred$data$response, pred$task.desc$negative)
#'   }
#' )
#' 
#' #' @export measureNPV
#' #' @rdname measures
#' #' @format none
#' measureNPV = function(truth, response, negative) {
#'   measureTN(truth, response, negative) / sum(response == negative)
#' }
#' 
#' #' @export fdr
#' #' @rdname measures
#' #' @format none
#' fdr = makeMeasure(id = "fdr", minimize = TRUE, best = 0, worst = 1,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "False discovery rate",
#'   note = "Defined as: (fp) / (tn + fn).",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureFDR(pred$data$truth, pred$data$response, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureFDR
#' #' @rdname measures
#' #' @format none
#' measureFDR = function(truth, response, positive) {
#'   measureFP(truth, response, positive) / sum(response == positive)
#' }
#' 
#' #' @export mcc
#' #' @rdname measures
#' #' @format none
#' mcc = makeMeasure(id = "mcc", minimize = FALSE,
#'   properties = c("classif", "req.pred", "req.truth"), best = 1, worst = -1,
#'   name = "Matthews correlation coefficient",
#'   note = "Defined as sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)), denominator set to 1 if 0",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureMCC(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureMCC
#' #' @rdname measures
#' #' @format none
#' measureMCC = function(truth, response, negative, positive) {
#'   tn = as.numeric(measureTN(truth, response, negative))
#'   tp = as.numeric(measureTP(truth, response, positive))
#'   fn = as.numeric(measureFN(truth, response, negative))
#'   fp = as.numeric(measureFP(truth, response, positive))
#'   
#'   denom = sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
#'   # According to Wikipedia, the denominator can be set arbitrarily if it's 0. 1 seems to make as much sense as anything else.
#'   if (denom == 0) denom = 1
#'   
#'   (tp * tn - fp * fn) / denom
#' }
#' 
#' #' @export f1
#' #' @rdname measures
#' #' @format none
#' f1 = makeMeasure(id = "f1", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "F1 measure",
#'   note = "Defined as: 2 * tp/ (sum(truth == positive) + sum(response == positive))",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureF1(pred$data$truth, pred$data$response, pred$task.desc$positive)
#'   }
#' )
#' 
#' measureF1 = function(truth, response, positive) {
#'   2 * measureTP(truth, response, positive) /
#'     (sum(truth == positive) + sum(response == positive))
#' }
#' 
#' #' @export gmean
#' #' @rdname measures
#' #' @format none
#' gmean = makeMeasure(id = "gmean", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "G-mean",
#'   note = "Geometric mean of recall and specificity.",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureGMEAN(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureGMEAN
#' #' @rdname measures
#' #' @format none
#' #' @references
#' #' He, H. & Garcia, E. A. (2009)
#' #' \emph{Learning from Imbalanced Data.}
#' #' IEEE Transactions on Knowledge and Data Engineering, vol. 21, no. 9. pp. 1263-1284.
#' measureGMEAN = function(truth, response, negative, positive) {
#'   sqrt(measureTPR(truth, response, positive) * measureTNR(truth, response, negative))
#' }
#' 
#' #' @export gpr
#' #' @rdname measures
#' #' @format none
#' gpr = makeMeasure(id = "gpr", minimize = FALSE, best = 1, worst = 0,
#'   properties = c("classif", "req.pred", "req.truth"),
#'   name = "Geometric mean of precision and recall.",
#'   note = "Defined as: sqrt(ppv * tpr)",
#'   fun = function(task, model, pred, feats, extra.args) {
#'     measureGPR(pred$data$truth, pred$data$response, pred$task.desc$positive)
#'   }
#' )
#' 
#' #' @export measureGPR
#' #' @rdname measures
#' #' @format none
#' measureGPR = function(truth, response, positive) {
#'   sqrt(measurePPV(truth, response, positive) * measureTPR(truth, response, positive))
#' }
