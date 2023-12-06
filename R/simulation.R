styler:::style_active_file()

library(rlang)
library(R6)
library(collections)
library(tidyverse)
# right now it is quite messy
# and I need some base functions for graphs and testing methods

# basic functions and constants --------------------------------------

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

pull_bandit <- function(p_distr) { # random
  sample(winning_sizes, size = 1, prob = p_distr)
}

get_winning_bandit <- function(last_winner = 0) { # random
  sample_from <- 1:n_bandits
  sample_from <- sample_from[sample_from != last_winner]
  winning_bandit <- sample(sample_from, size = 1)
  winning_bandit
}

get_distrs <- function(winner) {
  p_distrs <- replicate(n_bandits, losing_p_distr, simplify = FALSE)
  p_distrs[[winner]] <- winning_p_distr
  p_distrs
}

get_winning_distrs <- function(last_winner = 0) {
  winning <- get_winning_bandit(last_winner)
  p_distrs <- get_distrs(winning)
  list("winning" = winning, "p_distrs" = p_distrs)
}

sigmoid <- function(x, c1, c2) {
  1 / (1 + 2.7^((-c1) * (x - c2)))
}

sigmoid_paramed <- \(x) sigmoid(x, c1 = .001, c2 = 5000)
softmax <- function(x) {
  e <- 2.71828
  x_exp <- e^(x)
  x_exp / sum(x_exp)
}


# class with Gaming environment --------------------------------------

GameEnvironment <- R6Class("GameEnvironment",
  public = list(
    # attributes from player
    n = 1,
    rewards = NULL,
    indices = NULL,

    # attributes for casino # should be private I guess
    p_distrs = NULL,
    winner = NULL,
    changes = NULL, # на каком шаге какой автомат был выигрышным

    history_prizes = list(),
    last_change = 0,
    initialize = function(n_steps) {
      self$rewards <- vector("list", n_steps)
      self$indices <- vector("list", n_steps)

      self$winner <- get_winning_bandit()
      self$p_distrs <- get_distrs(self$winner)

      self$changes <- list(c(self$winner, 1))
      self$history_prizes <- vector("numeric", length = n_bandits)
    },
    pull_machine = function(index) {
      reward <- pull_bandit(self$p_distrs[[index]])

      self$indices[[self$n]] <- index
      self$rewards[[self$n]] <- reward
      self$n <- self$n + 1 # maybe check for n <= n_steps

      private$update_machines()
      self$history_prizes[[index]] <- self$history_prizes[[index]] +
        reward

      reward
    }
  ),
  private = list(
    update_machines = function() {
      self$last_change <- self$last_change + 1

      if ( # если автомат больше всего приносит выгоды и >= 200 игр
        (self$winner == which.max(self$history_prizes)) &
          (self$last_change >= 200)
      ) {
        # значение на сигмоиде -> 1, чем больше прибыли от автомата
        p <- sigmoid_paramed(self$history_prizes[[self$winner]])
        is_change <- sample(0:1, size = 1, prob = c(1 - p, p))

        if (is_change) {
          # print(p)
          self$winner <- get_winning_bandit(self$winner)
          self$p_distrs <- get_distrs(self$winner)
          self$history_prizes <- vector("numeric", length = n_bandits)
          self$changes <- append(
            self$changes, list(c(self$winner, self$n))
          )
          self$last_change <- 0
        }
      }
    }
  )
)

init_pull_bandit_player <- function(game_environment) {
  pull_bandit_player <- function(index) {
    game_environment$pull_machine(index)
  }
  pull_bandit_player
}

run_iter <- function(strategy) {
  g <- GameEnvironment$new(n_steps)
  str1 <- strategy()
  pull_ <- init_pull_bandit_player(g)
  walk(1:n_steps, ~ str1(pull_))
  list(
    ind = g$indices, rewards = g$rewards, changes = g$changes
  )
}

repeate_iters <- function(strategy) {
  map(1:n_iters, ~ run_iter(strategy))
}


get_rewards_simulations <- function(results) {
  map(results, ~ .x$rewards |> unlist())
}


# strategies ----------------------------------------------------------

# they might be classes
# and they will be validated to enforce structure

# random play

strategy1 <- function() {
  engine <- function(func) {
    throwaway <- func(sample(1:n_bandits, size = 1))
  }
  engine
}

# play where max avg mean of winnings
strategy2 <- function() {
  last_winnings <- map(1:n_bandits, ~ deque(rep(price, 50)))
  engine <- function(func) {
    avg_mean <- map_dbl(
      map(last_winnings, ~ .x$as_list() |> unlist()),
      mean
    )
    # print(avg_mean)
    to_pull <- which.max(avg_mean)
    result <- func(to_pull)

    last_winnings[[to_pull]]$push(result)
    last_winnings[[to_pull]]$popleft()
  }
  engine
}

# maybe add different alpha in t statistics
# t stat
strategy3 <- function() {
  get_lower_boundary <- function(x) {
    t.test(x, mu = price, alternative = "greater")$conf.int[[1]]
  }

  lower_boundaries <- rep(9.8, n_bandits)

  last_winnings <- map(1:n_bandits, ~ deque(c(9.8, 10.2))) # var != 0
  max_history <- 50

  engine <- function(pull_func) {
    avg_mean <- map_dbl(
      map(last_winnings, ~ .x$as_list() |> unlist()),
      mean
    )
    p <- lower_boundaries |> softmax()
    if (sample(0:1, size = 1) == 1) { # random strategy
      to_pull <- sample(1:n_bandits, size = 1)
    } else { # intelligent strategy
      to_pull <- sample(1:n_bandits, p = p, size = 1)
    }
    result <- pull_func(to_pull)

    last_winnings[[to_pull]]$push(result)
    if (last_winnings[[to_pull]]$size() > max_history) {
      last_winnings[[to_pull]]$popleft()
    }
    lower_boundaries[[to_pull]] <<- last_winnings[[to_pull]]$as_list() |>
      unlist() |>
      get_lower_boundary()
  }
  engine
}

# write strategy based on Bayesian statistics

# plays generation ---------------------------------------------------

# for debug purposes:

# g <- GameEnvironment$new(n_steps)
# str1 <- strategy3()
# pull_ <- init_pull_bandit_player(g)
# walk(1:10, ~ str1(pull_))

# it1 <- run_iter(strategy3)
# I feel uneasy that strategy can call pull more than once per step

# it100 <- repeate_iters(strategy3)
#
# r1 <- get_rewards_simulations(it100)


# visualization -------------------------------------------------------

# Остаток на балансе под конец игры
plot_end_cash <- function(cash_left) {
  data <- tibble(
    x = cash + map(cash_left, sum) |> unlist() - price * n_steps
  )
  ggplot(data = data, aes(x = x)) +
    geom_histogram(color = "black", fill = "white")
}
# plot_end_cash(r1)


# Остаток на балансе в ходе игры
plot_left_on_step <- function(detail_wins) {
  data_on_step <- map(
    detail_wins, \(x) cumsum(x) - cumsum(rep(price, n_steps))
  ) |>
    transpose() |>
    map(unlist)
  data_reduced <- map(data_on_step, mean) |> unlist()
  data_df <- tibble(y = data_reduced + cash) |> mutate(x = row_number())
  ggplot(data = data_df, aes(x = x, y = y)) +
    geom_line()
}
# plot_left_on_step(r1)
# add ghost individual lines, would be good,
# need to think how to convert list of vectors to tibble
# (using explode maybe)


# data export ------------------------------------------------------

strategies <- c(
  strategy1,
  strategy2,
  strategy3
)

iterations_from_strategies <- map(strategies, repeate_iters)

name_strategy <- function(x) paste0("data/s", x, ".RData")

iwalk(
  iterations_from_strategies,
  ~ write_rds(.x, name_strategy(.y), compress = "gz")
)

# read it back
# r2 = read_rds("data/s1.RData")
