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
p_distrs = replicate(n_bandits, losing_p_distr, simplify = FALSE)
p_distrs[[sample(1:n_bandits, 1)]] = winning_p_distr

# estimation experiments ---------------------------------------------
sum(replicate(1000, pull_bandit(10, losing_p_distr))) / 1000

t.test(
  replicate(1000, pull_bandit(10, winning_p_distr)),
  mu=10,
  alternative='greater'
  )


# player --------------------------------------------------------------

myZip = function(l1, l2) transpose(list(l1, l2))

cash = 100
bandit_results = list()
# bandit_estimations = list?
ll = myZip(as.list(prices), p_distrs)
# some strategy
for (j in seq_along(ll)) {
  i = ll[[j]]
  res = replicate(1000, pull_bandit(i[[1]], i[[2]]))
  bandit_results[[j]] = res
}
# we can reuse old t statistics (in jupyterlite)
p_vals = sapply(
  myZip(as.list(prices), bandit_results),
  \(x) t.test(x[[2]], mu=x[[1]], alternative='greater')$p.value
  )

# which is the best
which.min(p_vals)


# write strategy based on Bayesian statistics

# strategies ----------------------------------------------------------


# visualization -------------------------------------------------------


