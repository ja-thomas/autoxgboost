context("autoxgboost")

test_that("autoxgboost works on different tasks",  {

  checkAutoxgboost = function(task, ...) {
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  r = autoxgboost(task, control = ctrl, build.final.model = "both", ...)

  extras = names(r$optim.result$opt.path$env$extra[[11]])
  expect_subset("nrounds", extras) # check that nrounds is in extras
  expect_equal(16, nrow(as.data.frame(r$optim.result$opt.path))) # check that opt.path has right number of rows
  }


  tasks = list(
    sonar.task, #binary classification
    iris.task, #multiclass classification
    subsetTask(bh.task, subset = 1:50, features = c(1:3, 5:12)), #regression
    subsetTask(bh.task, subset = 1:50) # regression with factors
  )

  fractions = c(0.2, 0.5, 0.8)
  earlystop = c(1, 10)


  for (f in fractions) {
    for (e in earlystop) {
        for (t in tasks)
         checkAutoxgboost(task = t, early.stopping.fraction = f, early.stopping.rounds = e)
    }
  }
})

test_that("autoxgboost output objects are ok", {

  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  r = autoxgboost(iris.task, control = ctrl, build.final.model = "both")

  expect_class(r, "list")
  expect_class(r$optim.result, c("MBOSingleObjResult", "MBOResult"))
  expect_class(r$model, "WrappedModel")

  expect_class(autoxgboost(iris.task, control = ctrl, build.final.model = "optim.result.only"), c("MBOSingleObjResult", "MBOResult"))
  expect_class(autoxgboost(iris.task, control = ctrl, build.final.model = "model.only"), "WrappedModel")

})
