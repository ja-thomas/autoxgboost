context("RLearner autoxgboost")

test_that("classif.autoxgboost works", {
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  lrn = makeLearner("classif.autoxgboost", control = ctrl)
  res = holdout(lrn, iris.task)
  expect_equal(c(50,5), dim(res$pred$data))
})

test_that("regr.autoxgboost works", {
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1L)
  lrn = makeLearner("regr.autoxgboost", control = ctrl)
  res = holdout(lrn, subsetTask(bh.task, subset = 1:50, features = c(1:3, 5:12)))
  expect_equal(c(17,5), dim(res$pred$data))
})
