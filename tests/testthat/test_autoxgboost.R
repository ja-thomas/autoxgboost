test_that("autoxgboost",  {

  #check classification
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  r = autoxgboost(sonar.task, mmce, control = ctrl)

  extras = names(r$opt.path$env$extra[[11]])

  expect_subset("nrounds", extras)

  #check regression
  r = autoxgboost(subsetTask(bh.task, subset = 1:50, features = c(1:3, 5:12)), mse, control = ctrl)

})
