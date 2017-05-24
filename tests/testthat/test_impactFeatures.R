context("Impact Encoding")

test_that("Impact Encoding works", {

  #multiclass classif
  iris$bla = as.factor(sample(c("A", "B"), 150, T))
  iris$blub = as.factor(sample(c("Q", "R"), 150, T))
  task = makeClassifTask(data = iris, target = "Species")

  r1 = createImpactFeatures(task)

  expect_list(r1$value.table, types = "data.frame", len = 2, any.missing = FALSE)
  expect_data_frame(r1$value.table[[1]], any.missing = FALSE, nrows = 2, ncols = 4)
  expect_equal(levels(r1$value.table[[1]][,1]), levels(iris$bla))
  expect_equal(colnames(r1$value.table[[1]][,-1]), levels(iris$Species))
  expect_data_frame(r1$value.table[[2]], any.missing = FALSE, nrows = 2, ncols = 4)
  expect_equal(colnames(r1$value.table[[2]][,-1]), levels(iris$Species))

  r2 = createImpactFeatures(iris, target = "Species")
  expect_equal(r1$value.table, r2$value.table)
  expect_equal(getTaskData(r1$data), r2$data)


  #classification
  iris = droplevels(iris[1:100,])
  iris$bla = as.factor(sample(c("A", "B"), 100, T))
  iris$blub = as.factor(sample(c("Q", "R"), 100, T))
  task = makeClassifTask(data = iris, target = "Species")

  r1 = createImpactFeatures(task)

  expect_list(r1$value.table, types = "data.frame", len = 2, any.missing = FALSE)
  expect_data_frame(r1$value.table[[1]], any.missing = FALSE, nrows = 2, ncols = 2)
  expect_equal(levels(r1$value.table[[1]][,1]), levels(iris$bla))
  expect_data_frame(r1$value.table[[2]], any.missing = FALSE, nrows = 2, ncols = 2)
  expect_equal(levels(r1$value.table[[2]][,1]), levels(iris$blub))

  r2 = createImpactFeatures(iris, target = "Species")
  expect_equal(r1$value.table, r2$value.table)
  expect_equal(getTaskData(r1$data), r2$data)



  #regression
  bh.data = getTaskData(bh.task)
  r1 = createImpactFeatures(bh.task)

  expect_list(r1$value.table, types = "data.frame", len = 1, any.missing = FALSE)
  expect_data_frame(r1$value.table[[1]], any.missing = FALSE, nrows = 2, ncols = 2)
  expect_equal(levels(r1$value.table[[1]][,1]), levels(bh.data$chas))

  r2 = createImpactFeatures(bh.data, target = "medv")
  expect_equal(r1$value.table, r2$value.table)
  expect_equal(getTaskData(r1$data), r2$data)
})



