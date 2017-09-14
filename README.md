tourneyr
================

[![Travis-CI Build Status](https://travis-ci.org/beanumber/etl.svg?branch=master)](https://travis-ci.org/beanumber/tourneyr)

Simulate tournaments in R

``` r
# devtools::install_github("beanumber/tourneyr")
library(tourneyr)
n <- 8
teams <- data.frame(sport = "nba", id = 1:n,
                   mean_theta = sort(runif(n), decreasing = TRUE))

one_simulation(teams)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games  sport
    ##   <int>      <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.84210338      4         1    nba
    ## 2     2 0.76304284      3         2    nba
    ## 3     3 0.73346711      2         3    nba
    ## 4     4 0.62817682      4         1    nba
    ## 5     5 0.62694000      1         4    nba
    ## 6     6 0.17631253      4         1    nba
    ## 7     7 0.15634844      4         1    nba
    ## 8     8 0.02966646      3         2    nba

``` r
one_simulation(teams, series_length = 7)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games  sport
    ##   <int>      <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.84210338      3         2    nba
    ## 2     2 0.76304284      4         1    nba
    ## 3     3 0.73346711      4         1    nba
    ## 4     4 0.62817682      4         1    nba
    ## 5     5 0.62694000      2         3    nba
    ## 6     6 0.17631253      1         4    nba
    ## 7     7 0.15634844      3         2    nba
    ## 8     8 0.02966646      4         1    nba

``` r
one_simulation(teams, series_length = 99)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games  sport
    ##   <int>      <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.84210338      1         4    nba
    ## 2     2 0.76304284      4         1    nba
    ## 3     3 0.73346711      2         3    nba
    ## 4     4 0.62817682      4         1    nba
    ## 5     5 0.62694000      3         2    nba
    ## 6     6 0.17631253      4         1    nba
    ## 7     7 0.15634844      3         2    nba
    ## 8     8 0.02966646      4         1    nba

``` r
res <- many_simulations(teams, n = 10, series_length = 1)
res <- many_simulations(teams, n = 10, series_length = 7)
```
