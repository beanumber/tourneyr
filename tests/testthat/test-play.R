context("play works")

test_that("play works", {
  if (require(dplyr)) {
    nba <- best16_2016 %>%
      filter(sport == "nba")
    expect_s3_class(nba, "tbl_df")
    expect_equal(nrow(nba), nrow(one_simulation(nba)))

    x <- spurs_bulls %>%
      rename(theta = mean_theta, alpha = mean_alpha)
    expect_true(
      all(
        sapply(seq(1, 99, by = 2), series_probability_df,
               data = arrange(x, desc(theta))) %>%
        diff() > 0
      )
    )
    expect_true(
      all(
        sapply(seq(1, 99, by = 2), series_probability_df,
               data = arrange(x, theta)) %>%
          diff() < 0
      )
    )

    y <- bigfour_end %>%
      filter(sport == "nba" & season == 4) %>%
      arrange(desc(mean_theta)) %>%
      head(4)

    if (FALSE) {
      # check that 2 seed outperforms 3 seed
      many_simulations(y, n = 100, series_length = 7) %>%
        group_by(seed) %>%
        summarize(N = n(), mean_finish = mean(finish)) %>%
        pull(mean_finish) %>%
        diff() > 0
    }






    dominant <- data.frame(
      sport = "nba",
      mean_theta = c(1, 0),
      mean_alpha = c(0, 0),
      alpha_sport = 0
    )

    parity <- data.frame(
      sport = "nba",
      mean_theta = c(0.01, 0),
      mean_alpha = c(0, 0),
      alpha_sport = 0
    )

    expect_equal(series_probability(0, 0, 0, 0, 0, series_length = 1), 0.5)
    expect_equal(series_probability(1, 1, 0, 0, 0, series_length = 1), 0.5)
    expect_equal(series_probability(-1, -1, 0, 0, 0, series_length = 1), 0.5)
    expect_gt(series_probability(0.0001, 0, 0, 0, 0, series_length = 1), 0.5)
    expect_gt(series_probability(1, 0, 0, 0, 0, series_length = 1), 0.73)
    expect_gt(series_probability(1, 0, 0, 0, 0, series_length = 7),
              series_probability(1, 0, 0, 0, 0, series_length = 1))

    expect_gt(long_run_prob(dominant, n = 100, series_length = 1)$wpct, 0.5)
    expect_gt(long_run_prob(dominant, n = 100, series_length = 99)$wpct, 0.9)

    expect_gte(long_run_prob(parity, n = 100, series_length = 1)$wpct, 0.4)
    expect_gte(long_run_prob(parity, n = 100, series_length = 99)$wpct, 0.4)
  }
})
