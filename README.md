
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/jbkunst/irks.svg?branch=master)](https://travis-ci.org/jbkunst/irks) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jbkunst/irks?branch=master&svg=true)](https://ci.appveyor.com/project/jbkunst/irks)

irks
====

Is a set of tools for credit risk modelling.

Installation
------------

You can install the released version of irks from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("irks")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jbkunst/irks")
```

Model helpers
-------------

### `var_importance`

``` r
rf <- randomForest::randomForest(Species ~ ., data = iris)
var_importance(rf)
#> # A tibble: 4 x 2
#>   variable     importance
#>   <chr>             <dbl>
#> 1 Petal.Width       43.9 
#> 2 Petal.Length      42.6 
#> 3 Sepal.Length      10.3 
#> 4 Sepal.Width        2.43

rrf <- RRF::RRF(Species ~ ., data = iris)
var_importance(rrf)
#> # A tibble: 4 x 2
#>   variable     importance
#>   <chr>             <dbl>
#> 1 Petal.Width       50.0 
#> 2 Petal.Length      46.8 
#> 3 Sepal.Width        1.37
#> 4 Sepal.Length       1.02


suppressMessages(library(partykit))
ct <- ctree(Species ~ ., data = iris)
ct_rules(ct)
#>   node                                                          rule
#> 1    2                                           Petal.Length <= 1.9
#> 2    5 Petal.Length > 1.9 & Petal.Width <= 1.7 & Petal.Length <= 4.8
#> 3    6  Petal.Length > 1.9 & Petal.Width <= 1.7 & Petal.Length > 4.8
#> 4    7                        Petal.Length > 1.9 & Petal.Width > 1.7
```
