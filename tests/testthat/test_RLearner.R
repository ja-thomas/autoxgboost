context("RLearner autoxgboost")

test_that("classif.autoxgboost works", {

  #response + binary classif
  lrn = makeLearner("classif.autoxgboost", control = ctrl, mbo.learner = mbo.learner, predict.type = "response")
  res = holdout(lrn, sonar.task)
  expect_equal(c(70, 5), dim(res$pred$data))

  #response + multiclass classif
  res = holdout(lrn, iris.task)
  expect_equal(c(50, 5), dim(res$pred$data))

  #prob + binary
  lrn = makeLearner("classif.autoxgboost", control = ctrl, mbo.learner = mbo.learner, predict.type = "prob")
  res = holdout(lrn, sonar.task)
  expect_equal(c(70, 7), dim(res$pred$data))

  #prob + multiclass classif
  res = holdout(lrn, iris.task)
  expect_equal(c(50, 8), dim(res$pred$data))

})

test_that("regr.autoxgboost works", {
  lrn = makeLearner("regr.autoxgboost", control = ctrl, mbo.learner = mbo.learner)
  res = holdout(lrn, subsetTask(bh.task, subset = 1:50, features = c(1:3, 5:12)))
  expect_equal(c(17,5), dim(res$pred$data))
})
