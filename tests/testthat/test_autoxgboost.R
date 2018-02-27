context("autoxgboost")

checkAutoxgboost = function(task, build.final.model, control, mbo.learner, tune.threshold) {
    r = autoxgboost(task, build.final.model = build.final.model, max.nrounds = 1L, control = control,
      mbo.learner = mbo.learner, nthread = 1, tune.threshold = tune.threshold)
    td = getTaskDesc(task)

    expect_class(r, "AutoxgbResult")
    if (sum(td$n.feat[c("factors", "ordered")]) > 0) {
      expect_class(r$final.learner, "PreprocWrapper") # could be dummyFeaturesWrapper or ImpactFeatures Wrapper. PreprocWrapper is the parent object class
    } else {
      expect_class(r$final.learner, "RLearner")
    }
    expect_class(r$optim.result, c("MBOSingleObjResult", "MBOResult"))

    extras = names(r$optim.result$opt.path$env$extra[[11]])
    expect_subset("nrounds", extras) # check that nrounds is in extras
    expect_equal(16, nrow(as.data.frame(r$optim.result$opt.path))) # check that opt.path has right number of rows

    if (build.final.model) {
      expect_class(r$final.model, "WrappedModel")
      p = predict(r, newdata = getTaskData(task))
      expect_class(p, "Prediction")
    }

  }

context("Different Tasks")
test_that("autoxgboost works on different tasks",  {

  iris.fac = droplevels(iris[1:100,])
  iris.fac$bla = as.factor(sample(c("A", "B"), 100, T))
  iris.fac = makeClassifTask(data = iris.fac, target = "Species")

  iris.miss = iris
  iris.miss[4,3] = NA
  iris.miss = makeClassifTask(data = iris.miss, target = "Species")

  tasks = list(
    sonar.task, #binary classification
    iris.fac, #binary classification with factors
    iris.task, #multiclass classification
    subsetTask(bh.task, subset = 1:50),
    iris.fac
  )

  for (t in tasks) {
    checkAutoxgboost(task = t, build.final.model = TRUE, control = ctrl,
      mbo.learner = mbo.learner, tune.threshold = FALSE)
  }
  
})

context("Thresholds")
test_that("autoxgboost thresholding works",  {
  checkAutoxgboost(task = sonar.task, build.final.model = TRUE,
    control = ctrl, mbo.learner = mbo.learner, tune.threshold = TRUE)
  checkAutoxgboost(task = iris.task, build.final.model = TRUE,
    control = ctrl, mbo.learner = mbo.learner, tune.threshold = TRUE)
})

context("Weights")
test_that("weights work", {
  iris.weighted = makeClassifTask(data = iris, target = "Species", weights = sample(c(1,20), 150, replace = TRUE))
  bh.weighted = makeRegrTask(data = getTaskData(bh.task)[1:50, -4], target = "medv", weights = sample(c(1,20), 50, replace = TRUE))
  checkAutoxgboost(task = iris.weighted, build.final.model = FALSE, mbo.learner = mbo.learner, control = ctrl, tune.threshold = FALSE)
  checkAutoxgboost(task = bh.weighted, build.final.model = FALSE, mbo.learner = mbo.learner, control = ctrl, tune.threshold = FALSE)
})

context("Printer")
test_that("autoxgboost printer works", {
  mod = autoxgboost(iris.task, control = ctrl, mbo.learner = mbo.learner, tune.threshold = FALSE)
  expect_output(print(mod), "Autoxgboost tuning result")
  expect_output(print(mod), "Recommended parameters:")
  expect_output(print(mod), "eta:")
  expect_output(print(mod), "gamma:")
  expect_output(print(mod), "max_depth:")
  expect_output(print(mod), "colsample_bytree:")
  expect_output(print(mod), "colsample_bylevel:")
  expect_output(print(mod), "lambda:")
  expect_output(print(mod), "alpha:")
  expect_output(print(mod), "subsample:")
  expect_output(print(mod), "nrounds:")
  expect_output(print(mod), "With tuning result: mmce = ")

})
