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
    ##    seed     theta finish num_games  sport
    ##   <int>     <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.9488103      3         2    nba
    ## 2     2 0.9261089      3         2    nba
    ## 3     3 0.6397047      4         1    nba
    ## 4     4 0.4523190      4         1    nba
    ## 5     5 0.3838534      1         4    nba
    ## 6     6 0.3501573      2         3    nba
    ## 7     7 0.3391250      4         1    nba
    ## 8     8 0.2995515      4         1    nba

``` r
one_simulation(teams, series_length = 7)
```

    ## # A tibble: 8 x 5
    ##    seed     theta finish num_games  sport
    ##   <int>     <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.9488103      3         2    nba
    ## 2     2 0.9261089      3         2    nba
    ## 3     3 0.6397047      2         3    nba
    ## 4     4 0.4523190      4         1    nba
    ## 5     5 0.3838534      1         4    nba
    ## 6     6 0.3501573      4         1    nba
    ## 7     7 0.3391250      4         1    nba
    ## 8     8 0.2995515      4         1    nba

``` r
one_simulation(teams, series_length = 99)
```

    ## # A tibble: 8 x 5
    ##    seed     theta finish num_games  sport
    ##   <int>     <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.9488103      3         2    nba
    ## 2     2 0.9261089      1         4    nba
    ## 3     3 0.6397047      3         2    nba
    ## 4     4 0.4523190      2         3    nba
    ## 5     5 0.3838534      4         1    nba
    ## 6     6 0.3501573      4         1    nba
    ## 7     7 0.3391250      4         1    nba
    ## 8     8 0.2995515      4         1    nba

``` r
res <- many_simulations(teams, n = 10, series_length = 1)
res <- many_simulations(teams, n = 10, series_length = 7)
```
