test_that("autoxgboost",  {
  
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  r = autoxgboost(sonar.task, mmce, control = ctrl)
  
  extras = names(r$opt.path$env$extra[[11]])
  
  expect_subset("nrounds", extras)
  
})