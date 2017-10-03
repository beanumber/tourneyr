#' End-of-season parameters for four major sports
#' @docType data
#' @format
#' \describe{
#'  \item{sport}{which sport}
#'  \item{season}{which season, 1 = 2004}
#'  \item{team_id}{id of the team within each sport}
#'  \item{name}{team name}
#'  \item{N}{number of observations aggregated}
#'  \item{mean_theta}{average value of theta (team strength) over the final
#'  \code{N} weeks of the \code{season}}
#'  \item{mean_alpha}{average value of alpha (home advantage)}
#'  \item{alpha_sport}{sport-specific home advantage}
#' }
"bigfour_end"

#' Best 16 teams in each sport for 2016
#' @rdname bigfour_end
#' @docType data
"best16_2016"

#' Spurs vs. Bulls in 2016
#' @rdname bigfour_end
#' @docType data
"spurs_bulls"

#' Team strength estimates
#' @docType data
#' @format
#' \describe{
#'  \item{theta}{team strength estimate}
#'  \item{season}{which season, 1 = 2004}
#'  \item{week}{week within each season}
#'  \item{team_id}{id of the team within each sport}
#'  \item{cumweek}{cumulative weeks for each team across all seasons}
#'  \item{sport}{which sport}
#'  \item{max.week}{maximum number of weeks in each season, in each sport}
#'  \item{time_val}{numerical measure of time}
#'  \item{name}{team name}
#' }
"team_strengths"

#' Home advantage estimates
#' @docType data
#' @format
#' \describe{
#'  \item{alpha.team}{team-specific home advantage estimate}
#'  \item{alpha.lower}{5\% bound for team-specific home advantage estimate}
#'  \item{alpha.upper}{95\% bound for team-specific home advantage estimate}
#'  \item{team}{team name}
#'  \item{sport}{which sport}
#'  \item{alpha.sport}{sport-specific home advantage estimate}
#'  \item{alpha.team.overall}{sum of \code{alpha.team} and \code{alpha.sport}}
#'  \item{alpha.team.lower}{5\% bound for team-specific total home advantage estimate}
#'  \item{alpha.team.upper}{95\% bound for team-specific total home advantage estimate}
#'  \item{team_id}{id of the team within each sport}
#' }
"home_advantages"
