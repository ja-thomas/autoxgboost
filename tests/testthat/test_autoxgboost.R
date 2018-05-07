checkAutoxgboost = function(task, build.final.model, upper.impact.encoding.boundary, control, mbo.learner, tune.threshold) {
    r = autoxgboost(task, build.final.model = build.final.model, max.nrounds = 1L,
      upper.impact.encoding.boundary = upper.impact.encoding.boundary, control = control,
      mbo.learner = mbo.learner, nthread = 1, tune.threshold = tune.threshold)
    td = getTaskDesc(task)

    expect_class(r, "AutoxgbResult")
    if (sum(td$n.feat[c("factors", "ordered")]) > 0) {
      expect_class(r$final.learner, "CPOLearner")
    } else {
      expect_class(r$final.learner, "CPOLearner")
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

  tasks = list(
    sonar.task, #binary classification
    iris.fac, #binary classification with factors
    iris.task, #multiclass classification
    subsetTask(bh.task, subset = 1:50),
    iris.fac
  )

  for (im in c(1L, 2L, 10L)) {
    for (t in tasks) {
      checkAutoxgboost(task = t, build.final.model = TRUE, upper.impact.encoding.boundary = im,
        control = ctrl, mbo.learner = mbo.learner, tune.threshold = FALSE)
    }
  }

})

context("Thresholds")
test_that("autoxgboost thresholding works",  {
  checkAutoxgboost(task = sonar.task, build.final.model = TRUE, upper.impact.encoding.boundary = .Machine$integer.max,
    control = ctrl, mbo.learner = mbo.learner, tune.threshold = TRUE)
  #FIXME: Wait for faster multiclass threshold tuning in mlr
  #checkAutoxgboost(task = iris.task, build.final.model = TRUE, upper.impact.encoding.boundary = .Machine$integer.max,
  #  control = ctrl, mbo.learner = mbo.learner, tune.threshold = TRUE)
})

#context("Weights")
#test_that("weights work", {
#  iris.weighted = makeClassifTask(data = iris, target = "Species", weights = sample(c(1,20), 150, replace = TRUE))
#  bh.weighted = makeRegrTask(data = getTaskData(bh.task)[1:50, -4], target = "medv", weights = sample(c(1,20), 50, replace = TRUE))
#  checkAutoxgboost(task = iris.weighted, build.final.model = FALSE, mbo.learner = mbo.learner, upper.impact.encoding.boundary = .Machine$integer.max, control = ctrl, tune.threshold = FALSE)
#  checkAutoxgboost(task = bh.weighted, build.final.model = FALSE, mbo.learner = mbo.learner, upper.impact.encoding.boundary = .Machine$integer.max, control = ctrl, tune.threshold = FALSE)
#})

#context("Timestamps")
#test_that("Timestamps work", {
#    iris.time = addFeatureInformation(iris.time, "timestamps", "time1")
#    checkAutoxgboost(task = iris.time, build.final.model = TRUE, upper.impact.encoding.boundary = .Machine$integer.max,
#    control = ctrl, mbo.learner = mbo.learner, tune.threshold = FALSE)
#})
#
#context("Featurehashing")
#test_that("Featurehashing work", {
#    iris.fac = addFeatureInformation(iris.fac, "categ.featuresets", c("bla", "bla2"))
#    checkAutoxgboost(task = iris.fac, build.final.model = TRUE, upper.impact.encoding.boundary = .Machine$integer.max,
#    control = ctrl, mbo.learner = mbo.learner, tune.threshold = FALSE)
#})


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
