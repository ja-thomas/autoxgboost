context("RLearner lightgbm")

test_that("classif.lighgbm works", {

  #response + binary classif
  lrn = makeLearner("classif.lightgbm")
  res = holdout(lrn, sonar.task)
  expect_equal(c(70, 5), dim(res$pred$data))

  #response + multiclass classif
  res = holdout(lrn, iris.task)
  expect_equal(c(50, 5), dim(res$pred$data))

  #prob + binary
  lrn = setPredictType(lrn, "prob")
  res = holdout(lrn, sonar.task)
  expect_equal(c(70, 7), dim(res$pred$data))

  #prob + multiclass classif
  res = holdout(lrn, iris.task)
  expect_equal(c(50, 8), dim(res$pred$data))

})

test_that("regr.lighgbm works", {
  lrn = makeLearner("regr.lightgbm")
  res = holdout(lrn, subsetTask(bh.task, subset = 1:50, features = c(1:3, 5:12)))
  expect_equal(c(17,5), dim(res$pred$data))
})
