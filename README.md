
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidytests

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of tidytests is to provide a tidy implementation of the output
from t-tests and proportional tests.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("pstraforelli/tidytests")
```

## Example

The package currently consists of two functions, `pairwise_t_test()` and
`pairwise_prop_test()`. Both take a raw data frame as input, and the
variables to be used for testing.

### `pairwise_t_test()`

``` r
library(tidytests)

pairwise_t_test(iris, Sepal.Length, Species)
#> # A tibble: 3 x 9
#>   higher_group lower_group  p_value higher_mean lower_mean higher_sd lower_sd
#>   <chr>        <chr>          <dbl>       <dbl>      <dbl>     <dbl>    <dbl>
#> 1 versicolor   setosa      1.75e-15        5.94       5.01     0.516    0.352
#> 2 virginica    setosa      6.64e-32        6.59       5.01     0.636    0.352
#> 3 virginica    versicolor  2.77e- 9        6.59       5.94     0.636    0.516
#> # ... with 2 more variables: higher_n <int>, lower_n <int>
```

### `pairwise_prop_test()`

``` r
mydf <- data.frame(smokers = c(rbinom(100, 1, .8),
                               rbinom(70, 1, 0.7),
                               rbinom(50, 1, 0.6)),
                   region = c(rep("A", 100), rep("B", 70), rep("C", 50)))
pairwise_prop_test(mydf, smokers, region)
#> # A tibble: 6 x 8
#>   smokers higher_group lower_group p_value higher_percenta~ lower_percentage
#>     <int> <fct>        <fct>         <dbl>            <dbl>            <dbl>
#> 1       0 B            A             0.238            0.329            0.21 
#> 2       0 C            A             0.227            0.36             0.21 
#> 3       0 C            B             0.871            0.36             0.329
#> 4       1 A            B             0.238            0.79             0.671
#> 5       1 A            C             0.227            0.79             0.64 
#> 6       1 B            C             0.871            0.671            0.64 
#> # ... with 2 more variables: higher_n <int>, lower_n <int>
```
