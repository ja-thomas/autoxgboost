createDMatrixFromTask = function(task, weights = NULL) {

  data = getTaskData(task, target.extra = TRUE)

  if (getTaskType(task) == "classif")  {
    cl = getTaskClassLevels(task)
    data$target =  match(as.character(data$target), cl) - 1
  }

  if (!is.null(weights))
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target, weight = weights)
  else if (!is.null(task$weights))
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target, weight = task$weights)
  else
    xgboost::xgb.DMatrix(data = data.matrix(data$data), label = data$target)
}
