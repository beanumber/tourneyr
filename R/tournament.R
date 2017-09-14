
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
    l_seed <- igraph::vertex_attr(g2, "seed", index = children[1])
    r_seed <- igraph::vertex_attr(g2, "seed", index = children[2])

    series_prob <- series_probability(l_theta, r_theta, series_length)
#    cat(paste(l_seed, ":", l_theta, "vs.", r_seed, ":", r_theta, "series_prob = ", series_prob, "\n"))
    if (stats::runif(1) < series_prob) {
      g2 <- g2 %>%
        igraph::set_vertex_attr("theta", index = node, value = l_theta) %>%
        igraph::set_vertex_attr("seed", index = node, value = l_seed)
    } else {
      g2 <- g2 %>%
        igraph::set_vertex_attr("theta", index = node, value = r_theta) %>%
        igraph::set_vertex_attr("seed", index = node, value = r_seed)
    }
    return(g2)
  }
}

#' Calculate the probability of winning a series
#' @param theta1 value of theta
#' @param theta2 value of theta for other team
#' @param series_length number of games in the series
#' @param ... currently ignored
#' @importFrom stats dnbinom runif
#' @export
#' @examples
#' if (require(dplyr)) {
#'   spurs_bulls <- bigfour_2016 %>%
#'     filter(sport == "nba", season == 10,
#'            team_id %in% c(27, 5))
#'
#'   # should be around 54%
#'   series_probability(spurs_bulls$mean_theta[1], spurs_bulls$mean_theta[2], 1)
#'   long_run_prob(spurs_bulls, n = 10)
#'
#'   series_probability(spurs_bulls$mean_theta[1], spurs_bulls$mean_theta[2], 7)
#'   long_run_prob(spurs_bulls, n = 10, series_length = 7)
#'
#'   # should be nearly certain
#'   series_probability(spurs_bulls$mean_theta[1], spurs_bulls$mean_theta[2], 99)
#'   long_run_prob(spurs_bulls, n = 10, series_length = 99)
#' }

series_probability <- function(theta1, theta2, series_length = 1, ...) {
#  cat(paste("series_length =", series_length, "\n"))
  p <- theta1 - theta2
  # ilogit
  bt_prob <- exp(p) / (1 + exp(p))
  win <- ceiling(series_length / 2)
  # cumulative probability of fewer than `win` failures
  sum(stats::dnbinom(0:(win - 1), win, prob = bt_prob))
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
#' nba <- bigfour_2016 %>%
#'   filter(sport == "nba")
#'
#' one_simulation(nba)
#' one_simulation(nba, series_length = 7)
#' one_simulation(nba, series_length = 99)
#'
#' res <- bigfour_2016 %>%
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
#'   mean_theta = c(0.01, 0)
#' )
#'
#' long_run_prob(parity, n = 100, series_length = 99)
#'
#' if (require(dplyr) && require(tidyr)) {
#'   bigfour_2016 %>%
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



