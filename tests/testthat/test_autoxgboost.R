context("autoxgboost")

test_that("autoxgboost works on different tasks",  {

  checkAutoxgboost = function(task, build.final.model, ...) {
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  r = autoxgboost(task, control = ctrl, build.final.model = build.final.model, max.nrounds = 1L, ...)

  expect_class(r, "AutoxgbResult")
  expect_class(r$final.learner, "RLearner")
  expect_class(r$optim.result, c("MBOSingleObjResult", "MBOResult"))
  if (build.final.model) {
    expect_class(r$final.model, "WrappedModel")
  } else {
    expect_null(r$final.model)
  }

  extras = names(r$optim.result$opt.path$env$extra[[11]])
  expect_subset("nrounds", extras) # check that nrounds is in extras
  expect_equal(16, nrow(as.data.frame(r$optim.result$opt.path))) # check that opt.path has right number of rows
  }


  tasks = list(
    sonar.task, #binary classification
    iris.task, #multiclass classification
    subsetTask(bh.task, subset = 1:50)
  )


  for(bfm in c(TRUE, FALSE)) {
      for (t in tasks) {
        checkAutoxgboost(task = t, build.final.model = bfm) #check default
        checkAutoxgboost(task = t, early.stopping.fraction = 0.2, early.stopping.rounds = 1, build.final.model = bfm)
    }
  }

})
