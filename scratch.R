library(tidyverse)


# bandits definition -------------------------------------------------

scale_p_distr = function(vec) vec / sum(vec) # sum(P) = 1

winning_p_distr = c(4, 1.5, 2, 2.5, .13, .05) |>
  scale_p_distr()

losing_p_distr = c(5, 2, 2, 1, .1, .03) |>
  scale_p_distr()

get_winnings = function(price) {
  c(0, price %/% 2, price, price*2, price*10, price*100)
}

pull_bandit = function(price, p_distr) {
  sample(get_winnings(price), size=1, prob=p_distr)
}

prices = seq(5, 20) * 2
n_bandits = length(prices)

# estimation experiments ---------------------------------------------
sum(replicate(1000, pull_bandit(10, losing_p_distr))) / 1000

t.test(
  replicate(1000, pull_bandit(10, winning_p_distr)),
  mu=10,
  alternative='greater'
  )


# player --------------------------------------------------------------

cash = 100
bandit_results = list()
# bandit_estimations = list?


# strategies ----------------------------------------------------------


# visualization -------------------------------------------------------


