
# random play

strategy1 <- function() {
  engine <- function(func) {
    choice =
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
