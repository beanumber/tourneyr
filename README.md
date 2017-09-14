tourneyr
================

[![Travis-CI Build Status](https://travis-ci.org/beanumber/tourneyr.svg?branch=master)](https://travis-ci.org/beanumber/tourneyr)

Simulate tournaments in R

``` r
# devtools::install_github("beanumber/tourneyr")
library(tourneyr)
library(tidyverse)

nba <- bigfour_2016 %>%
  filter(sport == "nba")

one_simulation(nba)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games sport
    ##   <int>      <dbl>  <dbl>     <int> <chr>
    ## 1     1 0.28407551      4         1   nba
    ## 2     2 0.25984940      1         4   nba
    ## 3     3 0.25953228      3         2   nba
    ## 4     4 0.23932861      2         3   nba
    ## 5     5 0.15260894      4         1   nba
    ## 6     6 0.11709376      4         1   nba
    ## 7     7 0.11300606      4         1   nba
    ## 8     8 0.09243179      3         2   nba

``` r
one_simulation(nba, series_length = 7)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games sport
    ##   <int>      <dbl>  <dbl>     <int> <chr>
    ## 1     1 0.28407551      3         2   nba
    ## 2     2 0.25984940      2         3   nba
    ## 3     3 0.25953228      4         1   nba
    ## 4     4 0.23932861      1         4   nba
    ## 5     5 0.15260894      4         1   nba
    ## 6     6 0.11709376      3         2   nba
    ## 7     7 0.11300606      4         1   nba
    ## 8     8 0.09243179      4         1   nba

``` r
one_simulation(nba, series_length = 99)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games sport
    ##   <int>      <dbl>  <dbl>     <int> <chr>
    ## 1     1 0.28407551      3         2   nba
    ## 2     2 0.25984940      4         1   nba
    ## 3     3 0.25953228      1         4   nba
    ## 4     4 0.23932861      4         1   nba
    ## 5     5 0.15260894      2         3   nba
    ## 6     6 0.11709376      4         1   nba
    ## 7     7 0.11300606      3         2   nba
    ## 8     8 0.09243179      4         1   nba

``` r
res <- bigfour_2016 %>%
  group_by(sport) %>%
  do(many_simulations(., n = 100, series_length = 7))

res %>%
  group_by(sport) %>%
  summarize(cor(seed, finish))
```

    ## # A tibble: 4 x 2
    ##   sport `cor(seed, finish)`
    ##   <chr>               <dbl>
    ## 1   mlb         0.036774733
    ## 2   nba         0.163673457
    ## 3   nfl        -0.007251356
    ## 4   nhl        -0.025379745
