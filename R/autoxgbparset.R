autoxgbparset = makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.2),
  makeNumericParam("gamma", lower = -7, upper = 6, traf = function(x) 2^x),
  makeIntegerParam("max_depth", lower = 3, upper = 12),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
  makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("subsample", lower = 0.5, upper = 1)
)
