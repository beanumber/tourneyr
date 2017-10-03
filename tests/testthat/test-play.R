context("play works")

test_that("play works", {
  if (require(dplyr)) {
    nba <- best16_2016 %>%
      filter(sport == "nba")
    expect_s3_class(nba, "tbl_df")
    expect_equal(nrow(nba), nrow(one_simulation(nba)))

    expect_true(
      all(
        sapply(seq(1, 99, by = 2), series_probability_df,
               data = arrange(spurs_bulls, desc(mean_theta))) %>%
        diff() > 0
      )
    )
    expect_true(
      all(
        sapply(seq(1, 99, by = 2), series_probability_df,
               data = spurs_bulls) %>%
          diff() < 0
      )
    )

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
