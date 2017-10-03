

thetas <- download.file("https://github.com/bigfour/competitiveness/blob/master/data/tidy_thetas.R1.rda?raw=true", destfile = "data-raw/tidy_thetas.rda")

alphas <- download.file("https://github.com/bigfour/competitiveness/blob/master/data/tidy_alphas.R1.rda?raw=true", destfile = "data-raw/tidy_alphas.rda")

load("data-raw/tidy_thetas.rda")
load("data-raw/tidy_alphas.rda")

# team strengths
team_strengths <- tidy_thetas %>%
  select(-primary, -secondary, -tertiary, -quaternary)
save(team_strengths, file = "data/team_strengths.rda", compress = "xz")

# home advantages
home_advantages <- tidy_alphas %>%
  select(-primary, -secondary, -tertiary, -quaternary)
save(home_advantages, file = "data/home_advantages.rda", compress = "xz")


# end of season thetas and alphas
library(tidyverse)
bigfour_end <- tidy_thetas %>%
  left_join(select(tidy_alphas, sport, team_id, alpha.team, alpha.sport, alpha.team.overall),
            by = c("sport", "team_id")) %>%
  mutate(end_season = max.week - week <= 4) %>%
  filter(end_season) %>%
  group_by(sport, season, team_id, name) %>%
  summarize(N = n(), mean_theta = mean(theta),
            mean_alpha = mean(alpha.team), alpha_sport = mean(alpha.sport))

save(bigfour_end, file = "data/bigfour_end.rda")

# playoff teams, 2016
best_teams <- function(data, k = 8) {
  data %>%
    arrange(desc(mean_theta)) %>%
    head(k)
}

best16_2016 <- bigfour_end %>%
  filter(season == 10) %>%
  group_by(sport, season) %>%
  do(best_teams(., k = 16))

save(best16_2016, file = "data/best16_2016.rda", compress = "xz")

# Spurs vs. Bulls, 2016
spurs_bulls <- bigfour_end %>%
  filter(sport == "nba", season == 10,
         team_id %in% c(27, 5))

save(spurs_bulls, file = "data/spurs_bulls.rda", compress = "xz")
