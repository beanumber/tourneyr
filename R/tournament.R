
#' Play simulated tournament games
#' @param g a tournament
#' @param node index of node to play
#' @param series_length length of each series
#' @param ... currently ignored
#' @importFrom igraph vertex_attr neighbors
#' @importFrom stats runif
#' @export
#'
#'
#'

play <- function(g, node = 1, series_length = 1, ...) {
#  cat(paste("checking node", node, "\n"))
  self_theta <- igraph::vertex_attr(g, "theta", index = node)
#  cat(paste("...node value is", self_theta, "\n"))
  if (!is.na(self_theta)) {
    return(g)
  } else {
    children <- igraph::neighbors(g, v = node, mode = "out")
    g1 <- play(g, node = children[1], series_length)
#    cat(paste("completed left search\n"))
    g2 <- play(g1, node = children[2], series_length)
#    cat(paste("completed right search\n"))
    l_theta <- igraph::vertex_attr(g2, "theta", index = children[1])
    r_theta <- igraph::vertex_attr(g2, "theta", index = children[2])
    l_alpha <- igraph::vertex_attr(g2, "alpha", index = children[1])
    r_alpha <- igraph::vertex_attr(g2, "alpha", index = children[2])
    l_seed <- igraph::vertex_attr(g2, "seed", index = children[1])
    r_seed <- igraph::vertex_attr(g2, "seed", index = children[2])
    alpha <- igraph::vertex_attr(g2, "alpha_sport", index = children[1])

    series_prob <- series_probability(l_theta, r_theta, l_alpha, r_alpha, alpha, series_length)
#    cat(paste(l_seed, ":", l_theta, "vs.", r_seed, ":", r_theta, "series_prob = ", series_prob, "\n"))
    if (stats::runif(1) < series_prob) {
      g2 <- g2 %>%
        igraph::set_vertex_attr("theta", index = node, value = l_theta) %>%
        igraph::set_vertex_attr("alpha", index = node, value = l_alpha) %>%
        igraph::set_vertex_attr("seed", index = node, value = l_seed) %>%
        igraph::set_vertex_attr("alpha_sport", index = node, value = alpha)
    } else {
      g2 <- g2 %>%
        igraph::set_vertex_attr("theta", index = node, value = r_theta) %>%
        igraph::set_vertex_attr("alpha", index = node, value = r_alpha) %>%
        igraph::set_vertex_attr("seed", index = node, value = r_seed) %>%
        igraph::set_vertex_attr("alpha_sport", index = node, value = alpha)
    }
    return(g2)
  }
}

#' Calculate the probability of winning a series
#' @param theta1 value of theta
#' @param theta2 value of theta for other team
#' @param alpha1 HFA for team1
#' @param alpha2 HFA for team2
#' @param alpha HFA for overall sport
#' @param series_length number of games in the series
#' @param ... currently ignored
#' @importFrom stats dnbinom runif
#' @export
#' @examples
#' if (require(dplyr)) {
#'
#'   # should be around 54%
#'   series_probability(spurs_bulls$mean_theta[1], spurs_bulls$mean_theta[2],
#'                      spurs_bulls$mean_alpha[1], spurs_bulls$mean_alpha[2],
#'                      spurs_bulls$alpha_sport[1], series_length = 1)
#'   long_run_prob(spurs_bulls, n = 10)
#'
#'   series_probability(spurs_bulls$mean_theta[1], spurs_bulls$mean_theta[2],
#'                      spurs_bulls$mean_alpha[1], spurs_bulls$mean_alpha[2],
#'                      spurs_bulls$alpha_sport[1], series_length = 7)
#'   long_run_prob(spurs_bulls, n = 10, series_length = 7)
#'
#'   # should be nearly certain
#'   series_probability(spurs_bulls$mean_theta[1], spurs_bulls$mean_theta[2],
#'                      spurs_bulls$mean_alpha[1], spurs_bulls$mean_alpha[2],
#'                      spurs_bulls$alpha_sport[1], series_length = 99)
#'   long_run_prob(spurs_bulls, n = 10, series_length = 99)
#' }

series_probability <- function(theta1, theta2, alpha1, alpha2, alpha, series_length = 1, ...) {
#  cat(paste("series_length =", series_length, "\n"))

  # define a home and away probability of winning for the first team
  p <- c(theta1 - theta2 + alpha + alpha1,
         theta1 - theta2 + alpha - alpha2)

  # ilogit
  bt_prob <- exp(p) / (1 + exp(p))
  # Max number of games played at home and away for higher seeded team
  win <- c(ceiling(series_length / 2), floor(series_length / 2))

  # Joint distribution of games won at home and games won away.
  joint <- outer(stats::dbinom(0:win[1], win[1], bt_prob[1]),
                 stats::dbinom(0:win[2], win[2], bt_prob[2]))

  # Find the cells that we want to add up in joint
  #Probability that the team associated with theta1 wins.
  sum(joint[row(joint) + col(joint) - 2 >= win[1]])

  # cumulative probability of fewer than `win` failures
  # This assume each probability is the same for all games
  # sum(stats::dnbinom(0:(win - 1), win, prob = bt_prob))
}

#' @rdname series_probability
#' @param data a data frame like \code{\link{bigfour_end}}
#' @export
#' @examples
#'
#' # Spurs at home, single game
#' series_probability_df(arrange(spurs_bulls, desc(mean_theta)), 1)
#'
#' # Bulls at home, seven game series
#' series_probability_df(spurs_bulls, 7)
#'
#' # Bulls at home, 99 game series
#' series_probability_df(spurs_bulls, 99)
#'
#' # monotonic increase for Spurs
#' sapply(seq(1, 15, by = 2), series_probability_df,
#'        data = arrange(spurs_bulls, desc(mean_theta)))
#'
#' # monotonic decrease for Bulls
#' sapply(seq(1, 15, by = 2), series_probability_df,
#'        data = spurs_bulls)

series_probability_df <- function(data, series_length = 1, ...) {
  if (nrow(data) != 2) {
    stop("data frame must have exactly two rows")
  }
  series_probability(data$mean_theta[1], data$mean_theta[2],
                     data$mean_alpha[1], data$mean_alpha[2],
                     data$alpha_sport[1], series_length = series_length)
}


#' Seed a tournament
#' @param data a data frame
#' @param ... currently ignored
#' @importFrom igraph make_tree set_vertex_attr degree
#' @export

seed_tournament <- function(data, ...) {
  n <- nrow(data)
  i <- 1:n
  num_games <- n - 1
  t <- igraph::make_tree(num_games + n)
  leaves <- igraph::degree(t, mode = "out") == 0
  t_idx <- tournament_ordering(i)
  t <- t %>%
    set_vertex_attr("theta", index = leaves, value = data$mean_theta[t_idx]) %>%
    set_vertex_attr("alpha", index = leaves, value = data$mean_alpha[t_idx]) %>%
    set_vertex_attr("alpha_sport", index = leaves, value = data$alpha_sport[t_idx]) %>%
    set_vertex_attr("seed", index = leaves, value = t_idx)
  return(t)
}

#' Set tournament ordering
#' @param x a vector of potential seeds
#' @importFrom utils head
#' @export
#' @examples
#' tournament_ordering(1:16)

tournament_ordering <- function(x) {
  if (length(x) <= 2) {
    return(sort(x))
  }
  if (length(x) == 4) {
    return(c(1, 4, 3, 2))
  }
  # match all the pairs
  y <- utils::head(c(rbind(x, rev(x))), length(x))

  odds <- y[seq(from = 1, to = length(y), by = 2)]
  idx <- tournament_ordering(odds)
  new_idx <- rep(idx + idx - 1, each = 2) + rep(c(0, 1), by = 2)
  return(y[new_idx])
}

#' Finalize tournament results
#' @param g a tournament
#' @param ... currently ignored
#' @importFrom igraph vcount as_data_frame
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate group_by summarize n
#' @export

finish_tournament <- function(g, ...) {
  n <- igraph::vcount(g)
  g %>%
    igraph::as_data_frame(what = "vertices") %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate_(finish = ~ceiling(log2(as.numeric(rowname) + 1))) %>%
    dplyr::group_by_(~seed) %>%
    dplyr::summarize_(theta = ~mean(theta), finish = ~min(finish), num_games = ~n())
}

#' Simulate a single tournament
#' @param data a data frame
#' @param series_length number of games in each series
#' @param ... arguments passed to \code{\link{play}}
#' @rdname many_simulations
#' @export
#' @importFrom dplyr mutate %>%
#' @examples
#'
#' if (require(dplyr)) {
#' nba <- bigfour_end %>%
#'   filter(sport == "nba", season == 10) %>%
#'   arrange(desc(mean_theta)) %>%
#'   head(16)
#'
#' one_simulation(nba)
#' one_simulation(nba, series_length = 7)
#' one_simulation(nba, series_length = 99)
#'
#' res <- best16_2016 %>%
#'   group_by(sport) %>%
#'   do(many_simulations(., n = 10, series_length = 7))
#
#' res %>%
#'   group_by(sport) %>%
#'   summarize(cor(seed, finish))
#' }
#'

one_simulation <- function(data, series_length = 1, ...) {
  data %>%
    seed_tournament() %>%
    play(series_length = series_length, ...) %>%
    finish_tournament() %>%
    dplyr::mutate_(sport = ~unique(data$sport))
}

#' Simulate many tournaments
#' @param n number of simulations
#' @importFrom tibble as.tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate
#' @export

many_simulations <- function(data, n = 2, series_length = 1, ...) {
  replicate(n, expr = one_simulation(data, series_length = series_length)) %>%
    t() %>%
    tibble::as.tibble() %>%
    tidyr::unnest() %>%
    dplyr::mutate(rep_id = rep(1:n, each = nrow(data)))
}

#' Calculate long run probabilities
#' @param ... arguments passed to \code{\link{many_simulations}}
#' @importFrom dplyr group_by_ summarize_ mutate_ filter_
#' @export
#' @examples
#'
#' parity <- data.frame(
#'   sport = "nba",
#'   mean_theta = c(0.01, 0),
#'   mean_alpha = c(0, 0),
#'   alpha_sport = 0
#' )
#'
#' long_run_prob(parity, n = 100, series_length = 99)
#'
#' if (require(dplyr) && require(tidyr)) {
#'   best16_2016 %>%
#'     group_by(sport) %>%
#'     do(wpct = long_run_prob(., n = 10, series_length = 7)) %>%
#'     unnest()
#' }
#'
#'

long_run_prob <- function(...) {
  many_simulations(...) %>%
    dplyr::group_by_(~seed) %>%
    dplyr::summarize_(N = ~n(), wins = ~sum(finish == 1)) %>%
    dplyr::mutate_(wpct = ~wins / N) %>%
    dplyr::filter_(~seed == 1)
}



