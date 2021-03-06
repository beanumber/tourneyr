% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tournament.R
\name{long_run_prob}
\alias{long_run_prob}
\title{Calculate long run probabilities}
\usage{
long_run_prob(...)
}
\arguments{
\item{...}{arguments passed to \code{\link{many_simulations}}}
}
\description{
Calculate long run probabilities
}
\examples{

parity <- data.frame(
  sport = "nba",
  mean_theta = c(0.01, 0),
  mean_alpha = c(0, 0),
  alpha_sport = 0
)

long_run_prob(parity, n = 100, series_length = 99)

if (require(dplyr) && require(tidyr)) {
  best16_2016 \%>\%
    group_by(sport) \%>\%
    do(wpct = long_run_prob(., n = 10, series_length = 7)) \%>\%
    unnest()
}


}
