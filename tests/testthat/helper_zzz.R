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
