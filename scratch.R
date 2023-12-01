library(tidyverse)

# right now it is quite messy
# and I need some base functions for graphs and testing methods

# bandits definition -------------------------------------------------

scale_p_distr = function(vec) vec / sum(vec) # sum(vec) == 1

winning_p_distr = c(4, 1.5, 2, 2.5, .13, .05) |>
  scale_p_distr()

losing_p_distr = c(5, 2, 2, 1, .1, .03) |>
  scale_p_distr()

price = 10
n_bandits = 20 # constant

winning_sizes = c(0, price %/% 2, price, price*2, price*10, price*100)

pull_bandit = function(p_distr) { # random
  sample(winning_sizes, size=1, prob=p_distr)
}

get_winning_bandit = function(last_winner=0) { # random
  sample_from = 1:n_bandits
  sample_from = sample_from[sample_from != last_winner]
  winning_bandit = sample(sample_from, size=1)
  winning_bandit
}

get_distrs = function(winner) {
  p_distrs = replicate(n_bandits, losing_p_distr, simplify = FALSE)
  p_distrs[[winner]] = winning_p_distr
  p_distrs
}

get_winning_distrs = function(last_winner=0) {
  winning = get_winning_bandit(last_winner)
  p_distrs = get_distrs(winning)
  list("winning"=winning, "p_distrs"=p_distrs)
}

sigmoid = function(x, c1, c2) {
  1 / (1 + 2.7^((-c1)*(x - c2)))
}

sigmoid_paramed = \(x) sigmoid(x, c1=.001, c2=5000)

iter_play = 0
change_p_distrs = function(history_prizes, winner) {
  # print('hello')
  .GlobalEnv$iter_play = .GlobalEnv$iter_play + 1
  if (winner == which.max(history_prizes)) {
    # print('max winner condition')
    p=sigmoid_paramed(history_prizes[[winner]])
    is_change=sample(0:1, size=1, prob=c(1-p, p))
    # print(is_change)

    if (is_change) {
      winner <- get_winning_bandit(winner)
      .GlobalEnv$winner = winner
      p_distrs <- get_distrs(winner)
      .GlobalEnv$p_distrs = p_distrs
      history_prizes <- vector('numeric', length = n_bandits)
      .GlobalEnv$history_prizes = history_prizes
      .GlobalEnv$changes = append(
        .GlobalEnv$changes, list(c(winner, iter_play))
        )
    }
  }
  # list(p_distrs)
}

# plot(s, 0, 10000)

winner = get_winning_bandit()
winner
changes = list(c(winner, 1))
p_distrs = get_distrs(winner)

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

my_zip = function(l1, l2) transpose(list(l1, l2))

history_prizes = vector('numeric', length = n_bandits)


pull_bandit_player = function(index) {
  winning = pull_bandit(p_distrs[[index]])
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

iters = 1000
# Остаток на балансе под конец игры
plot_end_cash = function(cash_left) {
  data = tibble(
    x=cash+map(cash_left, sum) |> unlist() - price * iters
     )
  ggplot(data=data, aes(x=x)) +
    geom_histogram(color='black', fill='white')
}
# plot_end_cash(bandit_results)


# Остаток на балансе в ходе игры
plot_left_on_step = function(detail_wins) {
  data_on_step = map(
    bandit_results, \(x) cumsum(x) - cumsum(rep(price, iters))
    ) |> transpose() |> map(unlist)
  data_reduced = map(data_on_step, mean) |> unlist()
  data_df = tibble(y=data_reduced+cash) |> mutate(x=row_number())
  ggplot(data=data_df, aes(x=x, y=y)) +
    geom_line()
}
plot_left_on_step(bandit_results)
# add ghost individual lines, would be good,
# need to think how to convert list of vectors to tibble

# strategies ----------------------------------------------------------

cash = 100
bandit_results = list()

# random play

for (iter in 1:100){
  bandit_results[[iter]] = replicate(
    1000,
    pull_bandit_player(sample(1:n_bandits, size=1))
  )
}

# play where more expectancy to win

# different alpha in t statistics
# write strategy based on Bayesian statistics

# visualization -------------------------------------------------------


