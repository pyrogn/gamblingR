# styler:::style_active_file()


price <- 10 # price of one game
cash <- 300 # cash of player at the start
n_bandits <- 10 # total number of machines

# change distribution after this number of games
n_games_change_p <- 500

n_steps <- 3000 # steps in one simulation with one strategy
n_iters <- 500 # iterations for evaluating one strategy

# nicer table is in qmd file
scale_p_distr <- function(vec) vec / sum(vec) # sum(vec) == 1

winning_p_distr <- c(4, 1.5, 5, 2.5, 0.13, .05) |>
  scale_p_distr()

losing_p_distr <- c(5, 2, 2, 1, .1, .03) |>
  scale_p_distr()

winning_sizes <- c(
  0, price %/% 2, price, price * 2, price * 10, price * 100
)

# where to store data from simulations
repo_path = "/Users/pyro/R/gamblingR/"
name_strategy <- function(x) paste0(repo_path, "data/s", x, ".RData")

# validations
exp_losing <- sum(losing_p_distr * winning_sizes)
exp_winning <- sum(winning_p_distr * winning_sizes)
p_losing <- (n_bandits - 1) / n_bandits
p_winning <- 1 / n_bandits

stopifnot(
  # you lose on losing machine
  exp_losing < price,
  # you win on winning machine
  exp_winning > price,
  # you still lose on average playing randomly
  p_losing * exp_losing + p_winning * exp_winning < price
)

rm(exp_losing, exp_winning, p_losing, p_winning)
