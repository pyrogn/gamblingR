styler:::style_active_file()

library(rlang)
library(R6)
library(collections)
library(tidyverse)
# right now it is quite messy
# and I need some base functions for graphs and testing methods

# bandits definition -------------------------------------------------

scale_p_distr <- function(vec) vec / sum(vec) # sum(vec) == 1

winning_p_distr <- c(4, 1.5, 2, 2.5, .13, .05) |>
  scale_p_distr()

losing_p_distr <- c(5, 2, 2, 1, .1, .03) |>
  scale_p_distr()

price <- 10
cash <- 10000
n_bandits <- 20 # constant

n_steps <- 1000
n_iters <- 100

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
      self$n <- self$n + 1

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

g1 <- GameEnvironment$new(n_steps)
# g1$indices
# g1$pull_machine(20)
# g1$last_change
# g1$n
#
# g1$history_prizes
#
# g1$changes
# g1$rewards[1:20]
#
# g1$winner
# g1$history_prizes

init_pull_bandit_player <- function(game_environment) {
  pull_bandit_player <- function(index) {
    game_environment$pull_machine(index)
  }
  pull_bandit_player
}

strategy1 <- function() {
  engine <- function(func) {
    throwaway <- func(sample(1:n_bandits, size = 1))
  }
  engine
}



run_iter <- function(strategy) {
  g <- GameEnvironment$new(n_steps)
  str1 <- strategy()
  pull_ <- init_pull_bandit_player(g)
  for (step_ in 1:n_steps) {
    str1(pull_)
  }
  list(
    ind = g$indices, rewards = g$rewards, changes = g$changes
  )
}

l1 <- run_iter(strategy1)
ll1 =  map(1:n_iters, run_iter, strategy=strategy1)

repeate_iters <- function(func_iter, n_iters) {
  map(1:n_iters, func_iter)
}

l2 <- list(l1, l1)

















iter_play <- 0
change_p_distrs <- function(history_prizes, winner) {
  # print('hello')
  .GlobalEnv$iter_play <- .GlobalEnv$iter_play + 1
  if (winner == which.max(history_prizes)) {
    # print('max winner condition')
    p <- sigmoid_paramed(history_prizes[[winner]])
    is_change <- sample(0:1, size = 1, prob = c(1 - p, p))
    # print(is_change)

    if (is_change) {
      winner <- get_winning_bandit(winner)
      .GlobalEnv$winner <- winner
      p_distrs <- get_distrs(winner)
      .GlobalEnv$p_distrs <- p_distrs
      history_prizes <- vector("numeric", length = n_bandits)
      .GlobalEnv$history_prizes <- history_prizes
      .GlobalEnv$changes <- append(
        .GlobalEnv$changes, list(c(winner, iter_play))
      )
    }
  }
  # list(p_distrs)
}

# plot(s, 0, 10000)

winner <- get_winning_bandit()
winner
changes <- list(c(winner, 1))
p_distrs <- get_distrs(winner)

# estimation experiments ---------------------------------------------
# sum(replicate(1000, pull_bandit(10, losing_p_distr))) / 1000
#
# t.test(
#   replicate(1000, pull_bandit(10, winning_p_distr)),
#   mu=10,
#   alternative='greater'
#   )

# player --------------------------------------------------------------

# player knows number of machines and prices to play

my_zip <- function(l1, l2) transpose(list(l1, l2))

history_prizes <- vector("numeric", length = n_bandits)


pull_bandit_player <- function(index) {
  winning <- pull_bandit(p_distrs[[index]])
  history_prizes[[index]] <<- history_prizes[[index]] + winning # sideeff
  change_p_distrs(history_prizes, winner)
  winning
}



# # we can reuse old t statistics (in jupyterlite)
# p_vals = sapply(
#   bandit_results,
#   \(x) t.test(x, mu=price, alternative='greater')$p.value
#   )

# which is the best


# basic functions -----------------------------------------------------

iters <- 1000
# Остаток на балансе под конец игры
plot_end_cash <- function(cash_left) {
  data <- tibble(
    x = cash + map(cash_left, sum) |> unlist() - price * iters
  )
  ggplot(data = data, aes(x = x)) +
    geom_histogram(color = "black", fill = "white")
}
# plot_end_cash(bandit_results)


# Остаток на балансе в ходе игры
plot_left_on_step <- function(detail_wins) {
  data_on_step <- map(
    bandit_results, \(x) cumsum(x) - cumsum(rep(price, iters))
  ) |>
    transpose() |>
    map(unlist)
  data_reduced <- map(data_on_step, mean) |> unlist()
  data_df <- tibble(y = data_reduced + cash) |> mutate(x = row_number())
  ggplot(data = data_df, aes(x = x, y = y)) +
    geom_line()
}
# plot_left_on_step(bandit_results)
# add ghost individual lines, would be good,
# need to think how to convert list of vectors to tibble

strategy1 <- function() {
  # results = vector('list', length = n_bandits)
  results <- map(1:n_bandits, ~10) # avoiding NULLs
  # likely I need it to change to sum, not results on every machine, no?

  make_step <- function() {
    last_elems <- map(results, ~ tail(.x, 4)) # ineffective
    probs_bandit_to_play <- exp(last_elems) / sum(exp(last_elems))
    index_choice <- sample(
      1:n_bandits,
      size = 1, prob = probs_bandit_to_play
    )
    result <- pull_bandit_player(index_choice)
    # update vars in closure
    results[[index_choice]] <<- results[[index_choice]] + result
  }
  make_step
}

run_strategy_one_iter <- function(strategy_func) {
  strategy_func
}

# strategies ----------------------------------------------------------

cash <- 100
bandit_results <- list() # should be local?
iters <- 100
steps <- 1000

# random play

for (iter in 1:100) {
  bandit_results[[iter]] <- replicate(
    1000,
    pull_bandit_player(sample(1:n_bandits, size = 1))
  )
}

# play where more expectancy to win

# different alpha in t statistics
# write strategy based on Bayesian statistics

# visualization -------------------------------------------------------


# demo structure ------------------------------------------------------

iter_play <- 0
change_p_distrs <- function(history_prizes, winner) {
  # print('hello')
  .GlobalEnv$iter_play <- .GlobalEnv$iter_play + 1
  if (winner == which.max(history_prizes)) {
    # print('max winner condition')
    p <- sigmoid_paramed(history_prizes[[winner]])
    is_change <- sample(0:1, size = 1, prob = c(1 - p, p))
    # print(is_change)

    if (is_change) {
      winner <- get_winning_bandit(winner)
      .GlobalEnv$winner <- winner
      p_distrs <- get_distrs(winner)
      .GlobalEnv$p_distrs <- p_distrs
      history_prizes <- vector("numeric", length = n_bandits)
      .GlobalEnv$history_prizes <- history_prizes
      .GlobalEnv$changes <- append(
        .GlobalEnv$changes, list(c(winner, iter_play))
      )
    }
  }
  # list(p_distrs)
}

GameEnvironment <- R6Class("GameEnvironment",
  public = list(
    # attributes from player
    n = 1,
    rewards = list(),
    indices = list(),

    # attributes for casino
    p_distrs = NULL,
    winner = NULL,
    changes = NULL, # на каком шаге какой автомат был выигрышным

    history_prizes = list(),
    last_change = 0,
    initialize = function(n_steps) {
      self$b <- b
      self$rewards <- vector("list", n_steps)
      self$indices <- vector("list", n_steps)

      self$winner <- get_winning_bandit()
      self$p_distrs <- get_distrs(self$winner)

      self$changes <- list(c(self$winner, 1))
    },
    pull_machine = function(index) {
      reward <- pull_bandit_player(index)

      self$indices[[n]] <- index
      self$rewards[[n]] <- reward
      self$n <- self$n + 1

      self$update_machines()

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
          self$winner <- get_winning_bandit(winner)
          self$p_distrs <- get_distrs(winner)
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
c1 <- capture_output$new(3)
c1$rewards()

c1$f()
c1$a



strategy_demo <- function(play_tracker) {
  pull <- play_tracker()
  moving_avg <- c()
  make_step <- function() {

  }

  make_step
}

f1 <- function() {
  e1 <- env(
    a = 0
  )
  i1 <- function() {
    e1$a <- e1$a + 1
    e1$a
  }
  i1
}


ff1 <- f1()
ff1()

ff2 <- f1()
ff2()




y <- 1
f <- function(x) x + y
