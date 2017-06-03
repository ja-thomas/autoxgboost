# autoxgboost - Automatic tuning and fitting of [xgboost](https://github.com/dmlc/xgboost).

[![Build Status](https://travis-ci.org/ja-thomas/autoxgboost.svg?branch=master)](https://travis-ci.org/ja-thomas/autoxgboost)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/autoxgboost)](https://CRAN.R-project.org/package=autoxgboost)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/autoxgboost)](https://cran.rstudio.com/web/packages/autoxgboost/index.html)


* Install the development version

    ```splus
    devtools::install_github("ja-thomas/autoxgboost")
    ```

# General overview

autoxgboost aims to find an optimal [xgboost](https://github.com/dmlc/xgboost) model automatically using the machine learning framework [mlr](https://github.com/mlr-org/mlr)
and the bayesian optimization framework [mlrMBO](https://github.com/mlr-org/mlrMBO).

**Work in progress**! This is a very early version, so a lot can change in the future.
