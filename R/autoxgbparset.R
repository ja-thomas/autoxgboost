autoxgbparset = makeParamSet(
  makeNumericParam("eta", lower = 0.001, upper = 0.3),
  makeNumericParam("gamma", lower = -10, upper = 10, traf = function(x) 2^x),
  makeIntegerParam("max_depth", lower = 1, upper = 15),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
  makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x)
)