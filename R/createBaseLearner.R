createBaseLearner = function(boosting.backend, td, tt, measure, tune.threshold, nthread, early.stopping.rounds, max.nrounds) {

  if (boosting.backend == "xgboost") {
      pv = list()
      if (!is.null(nthread))
        pv$nthread = nthread
    if (tt == "classif") {

      predict.type = ifelse("req.prob" %in% measure$properties | tune.threshold, "prob", "response")
      if(length(td$class.levels) == 2) {
        objective = "binary:logistic"
        eval_metric = "error"
      } else {
        objective = "multi:softprob"
        eval_metric = "merror"
      }

      base.learner = makeLearner("classif.xgboost.earlystop", id = "classif.xgboost.earlystop", predict.type = predict.type,
        eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds,
        max.nrounds = max.nrounds, par.vals = pv)

    } else if (tt == "regr") {
      predict.type = NULL
      objective = "reg:linear"
      eval_metric = "rmse"

      base.learner = makeLearner("regr.xgboost.earlystop", id = "regr.xgboost.earlystop",
        eval_metric = eval_metric, objective = objective, early_stopping_rounds = early.stopping.rounds,
        max.nrounds = max.nrounds, par.vals = pv)

    } else {
      stop("Task must be regression or classification")
    }
  } else if (boosting.backend == "lightgbm") {
    pv = list()
    if (!is.null(nthread))
      pv$num_threads = num_threads

    if (tt == "classif") {
      predict.type = ifelse("req.prob" %in% measure$properties | tune.threshold, "prob", "response")
      base.learner = makeLearner("classif.lightgbm", predict.type = predict.type, early_stopping_rounds = early.stopping.rounds, nrounds = max.nrounds, par.vals = pv)
     } else if (tt == "regr") {
      base.learner = makeLearner("regr.lightgbm", early_stopping_rounds = early.stopping.rounds, nrounds = max.nrounds, par.vals = pv)
     } else {
       stop("Task must be regression or classification")
     }
  } else {
    stop("Boosting backend not supported")
  }

  return(base.learner)
}
