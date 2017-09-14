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
    ## 1     1 0.9526074      4         1    nba
    ## 2     2 0.7215296      4         1    nba
    ## 3     3 0.6888383      2         3    nba
    ## 4     4 0.6045824      4         1    nba
    ## 5     5 0.5583704      3         2    nba
    ## 6     6 0.3918751      4         1    nba
    ## 7     7 0.3487156      3         2    nba
    ## 8     8 0.3039493      1         4    nba

``` r
one_simulation(teams, series_length = 7)
```

    ## # A tibble: 8 x 5
    ##    seed     theta finish num_games  sport
    ##   <int>     <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.9526074      3         2    nba
    ## 2     2 0.7215296      3         2    nba
    ## 3     3 0.6888383      2         3    nba
    ## 4     4 0.6045824      1         4    nba
    ## 5     5 0.5583704      4         1    nba
    ## 6     6 0.3918751      4         1    nba
    ## 7     7 0.3487156      4         1    nba
    ## 8     8 0.3039493      4         1    nba

``` r
one_simulation(teams, series_length = 99)
```

    ## # A tibble: 8 x 5
    ##    seed     theta finish num_games  sport
    ##   <int>     <dbl>  <dbl>     <int> <fctr>
    ## 1     1 0.9526074      3         2    nba
    ## 2     2 0.7215296      4         1    nba
    ## 3     3 0.6888383      4         1    nba
    ## 4     4 0.6045824      4         1    nba
    ## 5     5 0.5583704      1         4    nba
    ## 6     6 0.3918751      2         3    nba
    ## 7     7 0.3487156      3         2    nba
    ## 8     8 0.3039493      4         1    nba

``` r
res <- many_simulations(teams, n = 10, series_length = 1)
res <- many_simulations(teams, n = 100, series_length = 7)

library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
res %>%
  summarize(cor(seed, finish))
```

    ## # A tibble: 1 x 1
    ##   `cor(seed, finish)`
    ##                 <dbl>
    ## 1           0.2398127
