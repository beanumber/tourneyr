tourneyr
================

[![Travis-CI Build Status](https://travis-ci.org/beanumber/tourneyr.svg?branch=master)](https://travis-ci.org/beanumber/tourneyr)

Simulate tournaments in R

``` r
# devtools::install_github("beanumber/tourneyr")
library(tourneyr)
library(tidyverse)

nba <- best16_2016 %>%
  filter(sport == "nba")

one_simulation(nba)
```

    ## # A tibble: 16 x 5
    ##     seed     theta finish num_games sport
    ##    <int>     <dbl>  <dbl>     <int> <chr>
    ##  1     1 1.2860679      5         1   nba
    ##  2     2 1.2551201      5         1   nba
    ##  3     3 1.2322665      1         5   nba
    ##  4     4 1.0483465      5         1   nba
    ##  5     5 0.7227860      2         4   nba
    ##  6     6 0.5454893      4         2   nba
    ##  7     7 0.5248155      5         1   nba
    ##  8     8 0.4429909      5         1   nba
    ##  9     9 0.4291082      4         2   nba
    ## 10    10 0.4176675      3         3   nba
    ## 11    11 0.3667088      5         1   nba
    ## 12    12 0.3315770      5         1   nba
    ## 13    13 0.2421223      4         2   nba
    ## 14    14 0.2179563      5         1   nba
    ## 15    15 0.1828562      4         2   nba
    ## 16    16 0.1058669      3         3   nba

``` r
one_simulation(nba, series_length = 7)
```

    ## # A tibble: 16 x 5
    ##     seed     theta finish num_games sport
    ##    <int>     <dbl>  <dbl>     <int> <chr>
    ##  1     1 1.2860679      1         5   nba
    ##  2     2 1.2551201      4         2   nba
    ##  3     3 1.2322665      4         2   nba
    ##  4     4 1.0483465      3         3   nba
    ##  5     5 0.7227860      5         1   nba
    ##  6     6 0.5454893      2         4   nba
    ##  7     7 0.5248155      3         3   nba
    ##  8     8 0.4429909      4         2   nba
    ##  9     9 0.4291082      5         1   nba
    ## 10    10 0.4176675      5         1   nba
    ## 11    11 0.3667088      5         1   nba
    ## 12    12 0.3315770      4         2   nba
    ## 13    13 0.2421223      5         1   nba
    ## 14    14 0.2179563      5         1   nba
    ## 15    15 0.1828562      5         1   nba
    ## 16    16 0.1058669      5         1   nba

``` r
one_simulation(nba, series_length = 999)
```

    ## # A tibble: 16 x 5
    ##     seed     theta finish num_games sport
    ##    <int>     <dbl>  <dbl>     <int> <chr>
    ##  1     1 1.2860679      1         5   nba
    ##  2     2 1.2551201      3         3   nba
    ##  3     3 1.2322665      2         4   nba
    ##  4     4 1.0483465      3         3   nba
    ##  5     5 0.7227860      4         2   nba
    ##  6     6 0.5454893      4         2   nba
    ##  7     7 0.5248155      4         2   nba
    ##  8     8 0.4429909      4         2   nba
    ##  9     9 0.4291082      5         1   nba
    ## 10    10 0.4176675      5         1   nba
    ## 11    11 0.3667088      5         1   nba
    ## 12    12 0.3315770      5         1   nba
    ## 13    13 0.2421223      5         1   nba
    ## 14    14 0.2179563      5         1   nba
    ## 15    15 0.1828562      5         1   nba
    ## 16    16 0.1058669      5         1   nba

``` r
res <- best16_2016 %>%
  group_by(sport) %>%
  do(many_simulations(., n = 100, series_length = 7))

res %>%
  group_by(sport) %>%
  summarize(cor(seed, finish))
```

    ## # A tibble: 4 x 2
    ##   sport `cor(seed, finish)`
    ##   <chr>               <dbl>
    ## 1   mlb           0.3272543
    ## 2   nba           0.7905059
    ## 3   nfl           0.7600452
    ## 4   nhl           0.3160438
