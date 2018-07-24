<img align="right" src="https://raw.githubusercontent.com/ja-thomas/autoxgboost/master/man/figures/hexagon.svg?sanitize=true" width="125px">
# autoxgboost - Automatic tuning and fitting of [xgboost](https://github.com/dmlc/xgboost).

[![Build Status](https://travis-ci.org/ja-thomas/autoxgboost.svg?branch=master)](https://travis-ci.org/ja-thomas/autoxgboost)
[![Coverage Status](https://coveralls.io/repos/github/ja-thomas/autoxgboost/badge.svg?branch=master)](https://coveralls.io/github/ja-thomas/autoxgboost?branch=master)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/autoxgboost)](https://CRAN.R-project.org/package=autoxgboost)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/autoxgboost)](https://cran.rstudio.com/web/packages/autoxgboost/index.html)


* Install the development version

    ```splus
    devtools::install_github("ja-thomas/autoxgboost")
    ```

# General overview

autoxgboost aims to find an optimal [xgboost](https://github.com/dmlc/xgboost) model automatically using the machine learning framework [mlr](https://github.com/mlr-org/mlr)
and the bayesian optimization framework [mlrMBO](https://github.com/mlr-org/mlrMBO).

**Work in progress**!

# Benchmark

|**Name**         |  **Factors**|  **Numerics**|  **Classes**|  **Train instances**|  **Test instances**
|-----------------|-------------|--------------|-------------|---------------------|--------------------
|Dexter           |       20 000|             0|            2|                  420|                 180
|GermanCredit     |           13|             7|            2|                  700|                 300
|Dorothea         |      100 000|             0|            2|                  805|                 345
|Yeast            |            0|             8|           10|                1 038|                 446
|Amazon           |       10 000|             0|           49|                1 050|                 450
|Secom            |            0|           591|            2|                1 096|                 471
|Semeion          |          256|             0|           10|                1 115|                 478
|Car              |            6|             0|            4|                1 209|                 519
|Madelon          |          500|             0|            2|                1 820|                 780
|KR-vs-KP         |           37|             0|            2|                2 237|                 959
|Abalone          |            1|             7|           28|                2 923|               1 254
|Wine Quality     |            0|            11|           11|                3 425|               1 469
|Waveform         |            0|            40|            3|                3 500|               1 500
|Gisette          |        5 000|             0|            2|                4 900|               2 100
|Convex           |            0|           784|            2|                8 000|              50 000
|Rot. MNIST + BI  |            0|           784|           10|               12 000|              50 000

Datasets used for the comparison benchmark of autoxgboost, Auto-WEKA and auto-sklearn.



|**Dataset**      |           **baseline**|         **autoxgboost**|           **Auto-WEKA**|        **auto-sklearn**
|-----------------|-----------------------|------------------------|------------------------|------------------------
|Dexter           |                  52,78|                   12.22|                    7.22|   <span>**5.56**</span>
|GermanCredit     |                  32.67|                   27.67|                   28.33|  <span>**27.00**</span>
|Dorothea         |                   6.09|   <span>**5.22**</span>|                    6.38|                    5.51
|Yeast            |                  68.99|  <span>**38.88**</span>|                   40.45|                   40.67
|Amazon           |                  99.33|                   26.22|                   37.56|  <span>**16.00**</span>
|Secom            |  <span>**7.87**</span>|   <span>**7.87**</span>|   <span>**7.87**</span>|   <span>**7.87**</span>
|Semeion          |                  92.45|                    8.38|   <span>**5.03**</span>|                    5.24
|Car              |                  29,15|                    1.16|                    0.58|   <span>**0.39**</span>
|Madelon          |                  50.26|                   16.54|                   21.15|  <span>**12.44**</span>
|KR-vs-KP         |                  48.96|                    1.67|   <span>**0.31**</span>|                    0.42
|Abalone          |                  84.04|                   73.75|  <span>**73.02**</span>|                   73.50
|Wine Quality     |                  55.68|  <span>**33.70**</span>|  <span>**33.70**</span>|                   33.76
|Waveform         |                  68.80|                   15.40|  <span>**14.40**</span>|                   14.93
|Gisette          |                  50.71|                    2.48|                    2.24|   <span>**1.62**</span>
|Convex           |                  50.00|                   22.74|                   22.05|  <span>**17.53**</span>
|Rot. MNIST + BI  |                  88.88|                   47.09|                   55.84|  <span>**46.92**</span>

Benchmark results are median percent error across 100 000 bootstrap samples (out of 25 runs) simulating 4 parallel runs. Bold numbers indicate best performing algorithms.

# autoxgboost - How to Cite

The **Automatic Gradient Boosting** framework was presented at the [ICML/IJCAI-ECAI 2018 AutoML Workshop](https://sites.google.com/site/automl2018icml/accepted-papers) ([poster](poster_2018.pdf)).  
Please cite our [ICML AutoML workshop paper on arxiv](https://arxiv.org/abs/1807.03873v2).
You can get citation info via `citation("autoxgboost")` or copy the following BibTex entry:

```bibtex
@article{autoxgboost,
  title = {Automatic Gradient Boosting},
  url = {https://arxiv.org/abs/1807.03873v2},
  shorttitle = {{{autoxgboost}}},
  archivePrefix = {arXiv},
  eprinttype = {arxiv},
  eprint = {1807.03873v2},
  primaryClass = {stat.ML},
  author = {Thomas, Janek and Coors, Stefan and Bischl, Bernd},
  date = {2017-07-13},
}
```
