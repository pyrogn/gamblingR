styler:::style_active_file()

library(rlang)
library(R6)
library(collections)
library(tidyverse)
source("R/constants.R")


# library(furrr) # for parallel computation, but doesn't work
# plan(multisession, workers = 4)

# TODO --------------------------------------------------------------

# 1. Make GameEnvironment resilient to disrespectful strategies
# 2. Make some attributes private in GameEnvironment
# 3. Add validators for strategies
# 4. Pass somehow names of strategies to plots.R

# Поработать над стратегиями

# basic functions --------------------------------------------------

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

sigmoid_paramed <- function(x) sigmoid(x, c1 = .001, c2 = 5000)

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

      if ( # если автомат принес больше всего выгоды и >= 200 игр
        (self$winner == which.max(self$history_prizes)) &
          (self$last_change >= n_games_change_p)
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
  last_winnings <- map(1:n_bandits, ~ deque(rep(price, 100)))
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
    if (sample(1:4, size = 1) == 1) { # random strategy
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

# sequential player
strategy4 <- function() {
  get_higher_boundary <- function(x) {
    t.test(x, mu = price)$conf.int[[2]]
  }

  higher_boundaries <- rep(9.8, n_bandits)

  last_winnings <- map(1:n_bandits, ~ deque(c(9.8, 10.2))) # var != 0
  max_history <- 50
  to_pull <- 1

  engine <- function(pull_func) {
    result <- pull_func(to_pull)

    last_winnings[[to_pull]]$push(result)
    if (last_winnings[[to_pull]]$size() > max_history) {
      last_winnings[[to_pull]]$popleft()
    }

    higher_boundaries[[to_pull]] <<- last_winnings[[
      to_pull
    ]]$as_list() |>
      unlist() |>
      get_higher_boundary()

    # print(higher_boundaries)

    if (higher_boundaries[[to_pull]] < price) {
      to_pull <<- (to_pull) %% n_bandits + 1
    }
  }
  engine
}

# chi squared player
strategy5 <- function() {
  # get_higher_boundary <- function(x) {
  #   t.test(x, mu = price)$conf.int[[2]]
  # }

  get_info <- function(vec) {
    vec_fac <- factor(vec, levels = winning_sizes)
    data <- list()
    get_p_val <- function(distr) {
      chisq.test(table(vec_fac), p = distr)$p.value
    }
    data$p_los <- get_p_val(losing_p_distr)
    data$p_win <- get_p_val(winning_p_distr)
    data$mean <- mean(vec)
    data
  }


  last_winnings <- map(1:n_bandits, ~ deque(c(9.8, 10.2))) # var != 0
  max_history <- 100
  to_pull <- 1

  engine <- function(pull_func) {
    result <- pull_func(to_pull)

    last_winnings[[to_pull]]$push(result)
    if (last_winnings[[to_pull]]$size() > max_history) {
      last_winnings[[to_pull]]$popleft()
    }

    # higher_boundaries[[to_pull]] <<- last_winnings[[
    #   to_pull
    # ]]$as_list() |>
    #   unlist() |>
    #   get_higher_boundary()

    data <- get_info(last_winnings[[to_pull]]$as_list())

    # print(higher_boundaries)

    if ((last_winnings[[to_pull]]$size() >= 10) &
      (data$p_los > data$p_win)) {
      to_pull <<- (to_pull) %% n_bandits + 1
    }
  }
  engine
}


# write strategy based on Bayesian statistics

# plays generation ---------------------------------------------------

get_attr_data <- function(data, attribute) {
  map(data, ~ .x[[attribute]] |> unlist())
}

get_rewards_simulations <- function(data) {
  get_attr_data(data, "rewards")
}

# for debug purposes:

# g <- GameEnvironment$new(n_steps)
# str1 <- strategy5()
# pull_ <- init_pull_bandit_player(g)
# walk(1:10, ~ str1(pull_))

# it1 <- run_iter(strategy5)
# I feel uneasy that strategy can call pull more than once per step

it100 <- repeate_iters(strategy5)

r1 <- get_rewards_simulations(it100)
map_dbl(r1, mean)
map_dbl(r1, mean) |> mean()

# data export ------------------------------------------------------

strategies <- c(
  strategy1,
  strategy2,
  strategy3,
  strategy4,
  strategy5
)
strategy_names <- c("Random", "s2", "s3", "s4", "s5")

iterations_from_strategies <- map(strategies, repeate_iters)

iwalk(
  iterations_from_strategies,
  ~ write_rds(.x, name_strategy(.y), compress = "gz")
)
