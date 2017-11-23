context("autoxgboost")

checkAutoxgboost = function(task, build.final.model, impact.encoding.boundary, control, mbo.learner, tune.threshold) {
    r = autoxgboost(task, build.final.model = build.final.model, max.nrounds = 1L,
      impact.encoding.boundary = impact.encoding.boundary, control = control, mbo.learner = mbo.learner, nthread = 1, tune.threshold = tune.threshold)
    td = getTaskDesc(task)

    expect_class(r, "AutoxgbResult")
    if (sum(td$n.feat[c("factors", "ordered")]) > 0 & factor.encoder == "impact") {
      expect_class(r$final.learner, "ImpactFeatureWrapper")
    } else {
      expect_class(r$final.learner, "RLearner")
    }
    expect_class(r$optim.result, c("MBOSingleObjResult", "MBOResult"))
    expect_class(r$final.model, "WrappedModel")

    extras = names(r$optim.result$opt.path$env$extra[[11]])
    expect_subset("nrounds", extras) # check that nrounds is in extras
    expect_equal(16, nrow(as.data.frame(r$optim.result$opt.path))) # check that opt.path has right number of rows

    p = predict(r, newdata = getTaskData(task))
    expect_class(p, "Prediction")

  }

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

  for (im in c(0L, Inf)) {
    for (t in tasks) {
      checkAutoxgboost(task = t, build.final.model = TRUE, impact.encoding.boundary = im, control = ctrl, mbo.learner = mbo.learner, tune.threshold = FALSE)
    }
  }

})

test_that("autoxgboost thresholding works",  {
  checkAutoxgboost(task = sonar.task, build.final.model = TRUE, impact.encoding.boundary = 0L, control = ctrl, mbo.learner = mbo.learner, tune.threshold = TRUE)
  checkAutoxgboost(task = iris.task, build.final.model = TRUE, impact.encoding.boundary = 0L, control = ctrl, mbo.learner = mbo.learner, tune.threshold = TRUE)
})
