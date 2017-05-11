context("autoxgboost")

test_that("autoxgboost works on different tasks",  {

  checkAutoxgboost = function(task, build.final.model, control, mbo.learner) {
  r = autoxgboost(task, build.final.model = build.final.model, max.nrounds = 1L, control = control, mbo.learner = mbo.learner, nthread = 1)
  td = getTaskDesc(task)

  expect_class(r, "AutoxgbResult")
  if (sum(td$n.feat[c("factors", "ordered")]) > 0) {
    expect_class(r$final.learner, "ImpactFeatureWrapper")
  } else {
    expect_class(r$final.learner, "RLearner")
  }
  expect_class(r$optim.result, c("MBOSingleObjResult", "MBOResult"))
  expect_class(r$final.model, "WrappedModel")

  extras = names(r$optim.result$opt.path$env$extra[[11]])
  expect_subset("nrounds", extras) # check that nrounds is in extras
  expect_equal(16, nrow(as.data.frame(r$optim.result$opt.path))) # check that opt.path has right number of rows
  }

  iris.fac = droplevels(iris[1:100,])
  iris.fac$bla = as.factor(sample(c("A", "B"), 100, T))
  iris.fac = makeClassifTask(data = iris.fac, target = "Species")

  tasks = list(
    sonar.task, #binary classification
    iris.fac, #binary classification with factors
    iris.task, #multiclass classification
    subsetTask(bh.task, subset = 1:50)
  )

  for (t in tasks) {
      checkAutoxgboost(task = t, build.final.model = TRUE, control = ctrl, mbo.learner = mbo.learner) #check default
  }

})
