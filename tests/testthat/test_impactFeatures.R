context("Impact Encoding")

test_that("Impact Encoding works", {

  #classification
  iris = droplevels(iris[1:100,])
  iris$bla = as.factor(sample(c("A", "B"), 100, T))
  iris$blub = as.factor(sample(c("Q", "R"), 100, T))
  task = makeClassifTask(data = iris, target = "Species")

  r1 = createImpactFeatures(task, fun = classOneFraction)

  expect_list(r1$value.table, types = "numeric", len = 2, any.missing = FALSE)
  expect_numeric(r1$value.table[[1]], len = 2, any.missing = FALSE, names = "named")
  expect_equal(names(r1$value.table[[1]]), levels(iris$bla))
  expect_numeric(r1$value.table[[2]], len = 2, any.missing = FALSE, names = "named")
  expect_equal(names(r1$value.table[[2]]), levels(iris$blub))

  r2 = createImpactFeatures(iris, target = "Species", fun = classOneFraction)
  expect_equal(r1$value.table, r2$value.table)
  expect_equal(getTaskData(r1$data), r2$data)



  #regression
  bh.data = getTaskData(bh.task)
  r1 = createImpactFeatures(bh.task, fun = mean)

  expect_list(r1$value.table, types = "numeric", len = 1, any.missing = FALSE)
  expect_numeric(r1$value.table[[1]], len = 2, any.missing = FALSE, names = "named")
  expect_equal(names(r1$value.table[[1]]), levels(bh.data$chas))

  r2 = createImpactFeatures(bh.data, target = "medv", fun = mean)
  expect_equal(r1$value.table, r2$value.table)
  expect_equal(getTaskData(r1$data), r2$data)
})



