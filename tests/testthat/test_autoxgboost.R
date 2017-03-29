test_that("autoxgboost",  {

  #check classification
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  r = autoxgboost(sonar.task, mmce, control = ctrl, max.nrounds = 10L, build.final.model = "optim.result.only")

  extras = names(r$opt.path$env$extra[[11]])
  expect_subset("nrounds", extras)

  r = autoxgboost(iris.task, mmce, control = ctrl, build.final.model = "optim.result.only")
  extras = names(r$opt.path$env$extra[[11]])
  expect_subset("nrounds", extras)

  #check regression
  r = autoxgboost(subsetTask(bh.task, subset = 1:50, features = c(1:3, 5:12)), mse, control = ctrl, max.nrounds = 10L, build.final.model = "optim.result.only")

  extras = names(r$opt.path$env$extra[[11]])
  expect_subset("nrounds", extras)

  #check regression with factor
  r = autoxgboost(subsetTask(bh.task, subset = 1:50), mse, control = ctrl, max.nrounds = 10L, build.final.model = "optim.result.only")

  extras = names(r$opt.path$env$extra[[11]])
  expect_subset("nrounds", extras)

})
