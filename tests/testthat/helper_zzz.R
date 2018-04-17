configureMlr(show.learner.output = FALSE)
options(mlrMBO.show.info = FALSE, mlrMBO.debug.mode = TRUE)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 1L)
ctrl = setMBOControlInfill(ctrl,
  crit = makeMBOInfillCritMeanResponse(),
  opt.focussearch.maxit = 1,
  opt.focussearch.points = 100,
  opt.restarts = 1)

mbo.learner = makeLearner("regr.rpart")


iris.fac = droplevels(iris[1:100,])
iris.fac$bla = as.factor(sample(c("A", "B"), 100, T))
iris.fac$bla2 = as.factor(sample(c("C", "D", "E"), 100, T))
iris.fac = makeClassifTask(data = iris.fac, target = "Species")

iris.miss = iris
iris.miss[4,3] = NA
iris.miss = makeClassifTask(data = iris.miss, target = "Species")

#iris.time = iris
#iris.time$time1 = as.numeric(as.POSIXlt(runif(150, 0, 10^5), origin = "2017-02-03 08:00:00"))
#iris.time = makeClassifTask(data = iris.time, target = "Species")
