tourneyr
================

[![Travis-CI Build Status](https://travis-ci.org/beanumber/tourneyr.svg?branch=master)](https://travis-ci.org/beanumber/tourneyr)

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
    ## 1     1 0.99654745      3         2    nba
    ## 2     2 0.89734899      3         2    nba
    ## 3     3 0.85381382      4         1    nba
    ## 4     4 0.61697476      4         1    nba
    ## 5     5 0.44977822      1         4    nba
    ## 6     6 0.34013383      2         3    nba
    ## 7     7 0.26513376      4         1    nba
    ## 8     8 0.07781834      4         1    nba

``` r
one_simulation(teams, series_length = 7)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games  sport
    ##   <int>      <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.99654745      4         1    nba
    ## 2     2 0.89734899      4         1    nba
    ## 3     3 0.85381382      4         1    nba
    ## 4     4 0.61697476      4         1    nba
    ## 5     5 0.44977822      3         2    nba
    ## 6     6 0.34013383      1         4    nba
    ## 7     7 0.26513376      3         2    nba
    ## 8     8 0.07781834      2         3    nba

``` r
one_simulation(teams, series_length = 99)
```

    ## # A tibble: 8 x 5
    ##    seed      theta finish num_games  sport
    ##   <int>      <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.99654745      4         1    nba
    ## 2     2 0.89734899      3         2    nba
    ## 3     3 0.85381382      1         4    nba
    ## 4     4 0.61697476      4         1    nba
    ## 5     5 0.44977822      3         2    nba
    ## 6     6 0.34013383      4         1    nba
    ## 7     7 0.26513376      4         1    nba
    ## 8     8 0.07781834      2         3    nba

``` r
res <- many_simulations(teams, n = 10, series_length = 1)
res <- many_simulations(teams, n = 100, series_length = 7)

library(tidyverse)
res %>%
  summarize(cor(seed, finish))
```

    ## # A tibble: 1 x 1
    ##   `cor(seed, finish)`
    ##                 <dbl>
    ## 1           0.3667114
