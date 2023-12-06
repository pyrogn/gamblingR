scale_p_distr <- function(vec) vec / sum(vec) # sum(vec) == 1

winning_p_distr <- c(4, 1.5, 5, 2.5, 0.13, .05) |>
  scale_p_distr()

losing_p_distr <- c(5, 2, 2, 1, .1, .03) |>
  scale_p_distr()

# constants
price <- 10
cash <- 1000
n_bandits <- 5

n_steps <- 1000
n_iters <- 50

winning_sizes <- c(
  0, price %/% 2, price, price * 2, price * 10, price * 100
)
