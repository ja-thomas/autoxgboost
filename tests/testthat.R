Sys.setenv("R_TESTS" = "")

library(testthat)
library(checkmate)
library(autoxgboost)

test_check("autoxgboost")
