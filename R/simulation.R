styler:::style_active_file()

library(rlang)
library(R6)
library(collections) # useful data structures
library(tidyverse)
source("R/constants.R")


# library(furrr) # for parallel computation, but doesn't work
# plan(multisession, workers = 4)

# basic functions --------------------------------------------------

# сыграть 1 раз
pull_bandit <- function(p_distr) {
  sample(winning_sizes, size = 1, prob = p_distr)
}

# получить новую победную машину
get_winning_bandit <- function(last_winner = 0) {
  sample_from <- 1:n_bandits
  sample_from <- sample_from[sample_from != last_winner]
  winning_bandit <- sample(sample_from, size = 1)
  winning_bandit
}

# Получить распределения с учетом индекса победной машины
get_distrs <- function(winner) {
  p_distrs <- replicate(n_bandits, losing_p_distr, simplify = FALSE)
  p_distrs[[winner]] <- winning_p_distr
  p_distrs
}


e <- 2.71828

sigmoid <- function(x, c1, c2) {
  1 / (1 + e^((-c1) * (x - c2)))
}

# сигмоида, подстроенная под средний выигрыш на автомате
sigmoid_paramed <- function(x) sigmoid(x, c1 = .001, c2 = 5000)

softmax <- function(x) {
  x_exp <- e^(x)
  x_exp / sum(x_exp)
}


# class with Gaming environment --------------------------------------

GameEnvironment <- R6Class("GameEnvironment",
  public = list(
    n = 1, # текущий шаг
    rewards = NULL, # история призов
    indices = NULL, # история индексов автоматов
    changes = NULL, # на каком шаге какой автомат был выигрышным

    initialize = function(n_steps) {
      self$rewards <- vector("list", n_steps)
      self$indices <- vector("list", n_steps)

      private$winner <- get_winning_bandit()
      private$p_distrs <- get_distrs(private$winner)

      self$changes <- list(c(private$winner, 1))
      private$history_prizes <- vector("numeric", length = n_bandits)
    },
    pull_machine = function(index) { # метод для стратегии
      if (self$n > n_steps) {
        # стратегия может заиграться, возвращаем 0 в таком случае,
        # сайд эффектов никаких не происходит
        return(0)
      }
      reward <- pull_bandit(private$p_distrs[[index]])

      self$indices[[self$n]] <- index
      self$rewards[[self$n]] <- reward
      self$n <- self$n + 1

      private$update_machines()
      private$history_prizes[[index]] <-
        private$history_prizes[[index]] + reward

      return(reward)
    }
  ),
  private = list(
    p_distrs = NULL, # распределения автоматов
    winner = NULL, # текущий выигрышный автомат
    history_prizes = list(), # история изменений выигрышного автомата
    last_change = 0, # кол-во шагов с прошлого изменения
    update_machines = function() {
      private$last_change <- private$last_change + 1

      if ( # если автомат принес больше всего выгоды и >= 200 игр
        (private$winner == which.max(private$history_prizes)) &
          (private$last_change >= n_games_change_p)
      ) {
        # значение на сигмоиде -> 1, чем больше прибыли от автомата
        p <- sigmoid_paramed(private$history_prizes[[private$winner]])
        is_change <- sample(0:1, size = 1, prob = c(1 - p, p))

        if (is_change) {
          private$winner <- get_winning_bandit(private$winner)
          private$p_distrs <- get_distrs(private$winner)
          private$history_prizes <- vector("numeric", length = n_bandits)
          self$changes <- append(
            self$changes, list(c(private$winner, self$n))
          )
          private$last_change <- 0
        }
      }
    }
  )
)

# запуск одной итерации стратегии
run_iter <- function(strategy) {
  g <- GameEnvironment$new(n_steps) # init environment
  strategy_step <- strategy() # init strategy
  # get method of game environment instance to put into strategy
  pull_lever <- g$pull_machine
  while (g$n <= n_steps) { # iterate while num. of games <= n_steps
    strategy_step(pull_lever)
  }
  # функция возвращает данные в списке для дальнейшего анализа
  list(
    ind = g$indices, rewards = g$rewards, changes = g$changes
  )
}

# Повторяем итерации и возвращаем список
repeate_iters <- function(strategy) {
  map(1:n_iters, ~ run_iter(strategy))
}


# strategies ----------------------------------------------------------

# На данный структура стратегий -
# функция, возвращающая функцию, которая принимает
# функцию нажимания ручки и получения награды.
# Для игры внутренняя функция обязана совершить одно или
# несколько нажиманий ручки как side-effect.
# Иначе программа зависнет на while. Функция ничего не возвращает.
# Во внешней функции можно создать переменные состояний (closure)

# !!!DISCLAIMER!!!
# Данные стратегии определенно могут быть улучшены
# Но это трудозатратно, так как вручную точно не быстрей,
# Либо автоматически - добавить возможность параметризации стратегий,
# Запускать симуляции с различными параметрами и брать лучшие параметры,
# Но ещё встает вопрос эффективности, так как симуляции не быстрые,
# а ещё надо перебирать параметры. Поэтому эти стратегии несут
# демонстрационный характер, что они справляются лучше случая.


# random play
strategy1 <- function() {
  engine <- function(func) {
    choice <-
      throwaway <- func(sample(1:n_bandits, size = 1))
  }
  engine
}

strip_deque <- function(q, max_size) {
  if (q$size() > max_size) {
    q$popleft() # throwaway
  }
  return(q)
}

# hate-to-lose man
strategy2 <- function() {
  patience_level <- 10
  to_pull <- sample(1:n_bandits, size = 1)

  engine <- function(func) {
    result <- func(to_pull)
    if (result < price) {
      patience_level <<- patience_level - (1 - result / price) * 2
    } else {
      patience_level <<- 10
    }

    if (patience_level <= 0) {
      sample_from <- 1:n_bandits
      sample_from <- sample_from[sample_from != to_pull]
      to_pull <<- sample(1:n_bandits, size = 1)
    }
  }
  engine
}

# t-test for expected value
strategy3 <- function() {
  get_lower_boundary <- function(x) {
    t.test(x,
      mu = price, alternative = "greater",
      conf.level = .5 # чтобы чуть сузить интервалы
    )$conf.int[[1]]
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
    to_pull <- sample(1:n_bandits, p = p, size = 1)
    result <- pull_func(to_pull)

    last_winnings[[to_pull]]$push(result)
    last_winnings[[to_pull]] <-
      strip_deque(last_winnings[[to_pull]], max_history)

    lower_boundaries[[to_pull]] <<- last_winnings[[to_pull]]$as_list() |>
      unlist() |>
      get_lower_boundary()
  }
  engine
}

# conservative player
strategy4 <- function() {
  conservative_exp <- function(vec) {
    size <- length(vec)
    t <- table(vec)
    table_names <- names(t)
    sum_rewards <- 0
    pp <- 0 # для нормализации вероятностей (не очень удачной)
    for (i in seq_along(t)) {
      test <- prop.test(
        t[[i]], size,
        conf.level = .9, alternative = "two.sided"
      )
      pp <- pp + test$conf.int[[1]]
      sum_rewards <- sum_rewards +
        test$conf.int[[1]] * as.integer(table_names[[i]])
    }
    return(sum_rewards * (1 / pp))
  }

  exp_values <- rep(1, n_bandits)

  last_winnings <- map(1:n_bandits, ~ deque(c(9.8, 10.2)))
  max_history <- 100
  to_pull <- 1

  engine <- function(pull_func) {
    p <- exp_values |> softmax()
    result <- pull_func(to_pull)

    last_winnings[[to_pull]]$push(result)
    last_winnings[[to_pull]] <-
      strip_deque(last_winnings[[to_pull]], max_history)

    exp_values[[to_pull]] <- conservative_exp(
      last_winnings[[to_pull]]$as_list() |> unlist()
    )
    if (exp_values[[to_pull]] < 7) { # think how to not hardcode
      to_pull <<- (to_pull) %% n_bandits + 1
    }
  }
  engine
}


# cheater
strategy5 <- function() {
  get_info <- function(vec) {
    vec_fac <- factor(vec, levels = winning_sizes)
    data <- list()
    get_p_val <- function(distr) { # maybe to use fisher.test?
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
    last_winnings[[to_pull]] <-
      strip_deque(last_winnings[[to_pull]], max_history)

    data <- get_info(last_winnings[[to_pull]]$as_list())

    if ((last_winnings[[to_pull]]$size() >= 10) &
      (data$p_los > data$p_win)) {
      to_pull <<- (to_pull) %% n_bandits + 1
    }
  }
  engine
}

# for interactive debug --------------------------------------------

# get_attr_data <- function(data, attribute) {
#   map(data, ~ .x[[attribute]] |> unlist())
# }
#
# get_rewards_simulations <- function(data) {
#   get_attr_data(data, "rewards")
# }

# g <- GameEnvironment$new(n_steps)
# str1 <- strategy4()
# pull_ <- init_pull_bandit_player(g)
# walk(1:100, ~ str1(pull_))

# it1 <- run_iter(strategy5)

# it100 <- repeate_iters(strategy2)
#
# r1 <- get_rewards_simulations(it100)
# map_dbl(r1, mean)
# map_dbl(r1, mean) |> mean()

# data export ------------------------------------------------------

# нельзя менять последовательность, связка стратегии и имени - хардкод
# который можно было бы убрать, но это того не стоит
strategies <- c(
  strategy1,
  strategy2,
  strategy3,
  strategy4,
  strategy5
)

iterations_from_strategies <- map(strategies, repeate_iters)

iwalk(
  iterations_from_strategies,
  ~ write_rds(.x, name_strategy(.y), compress = "gz")
)
