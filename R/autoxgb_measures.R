###############################################################################
### Regression ###
###############################################################################
autoxgbSSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureSSE(truth, preds)
  return(list(metric = "sse", value = err))
}

autoxgbMSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMSE(truth, preds)
  return(list(metric = "mse", value = err))
}

autoxgbRMSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRMSE(truth, preds)
  return(list(metric = "rmse", value = err))
}

autoxgbMEDSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMEDSE(truth, preds)
  return(list(metric = "medse", value = err))
}

autoxgbSAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureSAE(truth, preds)
  return(list(metric = "sae", value = err))
}

autoxgbMAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMAE(truth, preds)
  return(list(metric = "mae", value = err))
}

autoxgbMEDAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMEDAE(truth, preds)
  return(list(metric = "medae", value = err))
}

autoxgbRSQ = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRSQ(truth, preds)
  return(list(metric = "rsq", value = err))
}

autoxgbEXPVAR = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureEXPVAR(truth, preds)
  return(list(metric = "expvar", value = err))
}

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

autoxgbRRSE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRRSE(truth, preds)
  return(list(metric = "rrse", value = err))
}

autoxgbRAE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureRAE(truth, preds)
  return(list(metric = "rae", value = err))
}

autoxgbMAPE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMAPE(truth, preds)
  return(list(metric = "mape", value = err))
}

autoxgbMSLE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureMSLE(truth, preds)
  return(list(metric = "msle", value = err))
}

autoxgbRMSLE = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = sqrt(measureMSLE(truth, preds))
  return(list(metric = "rmsle", value = err))
}

autoxgbKENDALLTAU = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureKendallTau(truth, preds)
  return(list(metric = "kendalltau", value = err))
}

autoxgbSPEARMANRHO = function(preds, dtrain) {
  truth = getinfo(dtrain, "label")
  err = measureSpearmanRho(truth, preds)
  return(list(metric = "spearmanrho", value = err))
}

###############################################################################
### classif multi ###
###############################################################################
autoxgbMMCE = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  err = measureMMCE(truth, response = preds)
  return(list(metric = "mmce", value = err))
}

autoxgbACC = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  err = measureACC(truth, response = preds)
  return(list(metric = "acc", value = err))
}

autoxgbBER = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  # special case for predictions from FailureModel
  if (anyMissing(preds)) {
    return(list(metric = "ber", value = NA_real_))
  } else {
  n = length(levels(truth)) + 1L
  err = mean(mlr::calculateConfusionMatrix(pred, relative = TRUE)$relative.row[-n, n])
  }
  return(list(metric = "ber", value = err))
}

autoxgbMULTICLASS.AUNU = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAUNU(preds, truth)
  return(list(metric = "multiclass.aunu", value = err))
}

autoxgbMULTICLASS.AUNP = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAUNP(preds, truth)
  return(list(metric = "multiclass.aunp", value = err))
}

autoxgbMULTICLASS.AU1U = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAU1U(preds, truth)
  return(list(metric = "multiclass.au1u", value = err))
}

autoxgbMULTICLASS.AU1P = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureAU1P(preds, truth)
  return(list(metric = "multiclass.au1p", value = err))
}

autoxgbMULTICLASS.BRIER = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureMulticlassBrier(preds, truth)
  return(list(metric = "multiclass.brier", value = err))
}

autoxgbLOGLOSS = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureLogloss(preds, truth)
  return(list(metric = "logloss", value = err))
}

autoxgbSSR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureSSR(preds, truth)
  return(list(metric = "ssr", value = err))
}

autoxgbQSR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureQSR(preds, truth)
  return(list(metric = "qsr", value = err))
} 

autoxgbLSR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  err = measureLSR(preds, truth)
  return(list(metric = "lsr", value = err))
} 

autoxgbKAPPA = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  err = measureKAPPA(truth, response = preds)
  return(list(metric = "kappa", value = err))
} 

autoxgbWKAPPA = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  err = measureWKAPPA(truth, response = preds)
  return(list(metric = "wkappa", value = err))
} 
  
###############################################################################
### classif binary ###
###############################################################################
autoxgbAUC = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  lvl = levels(truth)
  err = measureAUC(preds, truth, negative = lvl[2], positive = lvl[1])
  return(list(metric = "auc", value = err))
} 

autoxgbBRIER = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  lvl = levels(truth)
  err = measureBrier(preds, truth, negative = lvl[2], positive = lvl[1])
  return(list(metric = "brier", value = err))
} 

autoxgbBRIER.SCALED = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  preds = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  lvl = levels(truth)
  err = measureBrierScaled(preds, truth, negative = lvl[2], positive = lvl[1])
  return(list(metric = "brier.scaled", value = err))
} 

autoxgbBAC = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureBAC(truth, response = preds, negative = lvl[2], positive = lvl[1])
  return(list(metric = "bac", value = err))
} 

autoxgbTP = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureTP(truth, response = preds, positive = lvl[1])
  return(list(metric = "tp", value = err))
} 

autoxgbTN = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureTN(truth, response = preds, negative = lvl[2])
  return(list(metric = "tn", value = err))
} 

autoxgbFP = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureFP(truth, response = preds, positive = lvl[1])
  return(list(metric = "fp", value = err))
} 

autoxgbFN = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureFN(truth, response = preds, negative = lvl[2])
  return(list(metric = "fn", value = err))
} 

autoxgbTPR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureTPR(truth, response = preds, positive = lvl[1])
  return(list(metric = "tpr", value = err))
} 

autoxgbTNR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureTNR(truth, response = preds, negative = lvl[2])
  return(list(metric = "tnr", value = err))
} 

autoxgbFPR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureFPR(truth, response = preds, negative = lvl[2], positive = lvl[1])
  return(list(metric = "fpr", value = err))
} 

autoxgbFNR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureFNR(truth, response = preds, negative = lvl[2], positive = lvl[1])
  return(list(metric = "fnr", value = err))
} 

autoxgbPPV = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  probs = matrix(preds, ncol = nlevels(truth), dimnames = list(NULL , levels(truth)))
  lvl = levels(truth)
  err = measurePPV(truth, response = preds, positive = lvl[1], probabilities = probs)
  return(list(metric = "ppv", value = err))
} 

autoxgbNPV = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureNPV(truth, response = preds, negative = lvl[2])
  return(list(metric = "npv", value = err))
} 

autoxgbFDR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureFDR(truth, response = preds, positive = lvl[1])
  return(list(metric = "fdr", value = err))
} 

autoxgbMCC = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureMCC(truth, response = preds, negative = lvl[2], positive = lvl[1])
  return(list(metric = "mcc", value = err))
} 

autoxgbF1 = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = mlr:::measureF1(truth, response = preds, positive = lvl[1])
  return(list(metric = "f1", value = err))
} 

autoxgbGMEAN = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureGMEAN(truth, response = preds, negative = lvl[2], positive = lvl[1])
  return(list(metric = "gmean", value = err))
} 

autoxgbGPR = function(preds, dtrain) {
  truth = as.factor(getinfo(dtrain, "label"))
  lvl = levels(truth)
  err = measureGPR(truth, response = preds, positive = lvl[1])
  return(list(metric = "gpr", value = err))
} 

