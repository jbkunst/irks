
<!-- README.md is generated from README.Rmd. Please edit that file -->
irks
====

The goal of irks is to ...

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
#> 1 Petal.Width       45.2 
#> 2 Petal.Length      41.6 
#> 3 Sepal.Length      10.00
#> 4 Sepal.Width        2.45

rrf <- RRF::RRF(Species ~ ., data = iris)
var_importance(rrf)
#> # A tibble: 4 x 2
#>   variable     importance
#>   <chr>             <dbl>
#> 1 Petal.Width       53.8 
#> 2 Petal.Length      42.8 
#> 3 Sepal.Width        1.41
#> 4 Sepal.Length       1.26


suppressMessages(library(partykit))
ct <- ctree(Species ~ ., data = iris)
ct_rules(ct)
#>   node                                                          rule
#> 1    2                                           Petal.Length <= 1.9
#> 2    5 Petal.Length > 1.9 & Petal.Width <= 1.7 & Petal.Length <= 4.8
#> 3    6  Petal.Length > 1.9 & Petal.Width <= 1.7 & Petal.Length > 4.8
#> 4    7                        Petal.Length > 1.9 & Petal.Width > 1.7
```
