generateXgbEvalFun = function(measure, tasktype) {
  id = measure$id
  tt = tasktype
  eval_metric = match.fun(paste0("measure", toupper(id)))
  
  if (tt == "regr") {
  f = function(preds, dtrain) {
    truth = getinfo(dtrain, "label")
    err = eval_metric(truth, preds)
    return(list(metric = id, value = err))
  }
  return(f)
  } else if (tt == "classif") {
    f = function(preds, dtrain) {
      truth = getinfo(dtrain, "label")
      lvl = as.numeric(levels(factor(truth)))
      if (length(lvl) == 2) {
        preds = ifelse(preds < 0.5, lvl[1], lvl[2])
      } else {
        # preds = ifelse(max(preds), lvl[1], lvl[2])
      }
        err = eval_metric(truth, preds)
      return(list(metric = id, value = err))
    }
  }
}
