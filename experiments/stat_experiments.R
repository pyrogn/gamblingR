library(tidyverse)
source("R/constants.R")

pull_bandit <- function(p_distr) { # random
  sample(winning_sizes, size = 1, prob = p_distr)
}

res=map_dbl(1:50, ~pull_bandit(winning_p_distr))
# res=map_dbl(1:50, ~pull_bandit(losing_p_distr))

res
# make it a factor
t = table(res)/length(res)
t
prop.test(13, length(res), conf.level = .5, alternative = 'two.sided')
prop.test(1, length(res), conf.level = .5, alternative = 'two.sided')
conservative_exp = function(vec) {
  size=length(vec)
  t = table(vec)
  table_names = names(t)
  # mean(res)
  sum_rewards = 0
  pp = 0
  for (i in seq_along(t)) {
    cat(paste0(table_names[[i]]), ' ')
    cat(paste0(t[[i]]), ' ')
    test = prop.test(
      t[[i]], size, conf.level = .9, alternative = 'two.sided')
    cat(paste0(t[[i]] / size), ' ')
    cat(paste0(round(test$conf.int[[1]], 2)), ' ')
    cat('\n')
    pp <- pp + test$conf.int[[1]]
    sum_rewards <- sum_rewards +
      test$conf.int[[1]] * as.integer(table_names[[i]])

  }
  return(sum_rewards * (1/pp))
}
conservative_exp(res)

  # factor(levels=c(0, 5, 10, 20,100, 1000))

# t.test(res, mu=10)
# table(res)/length(res)

# chisq.test(res, winning_p_distr)
# chisq.test(table(res)/length(res))


chisq.test(table(res), p=winning_p_distr)
chisq.test(table(res), p=losing_p_distr)

res=map_dbl(1:10, ~pull_bandit(losing_p_distr))
# table(res)/100
# mean(res)
chisq.test(table(res), p=winning_p_distr)
chisq.test(table(res), p=losing_p_distr)



dat <- iris

dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length),
                   "small", "big"
)

table(dat$Species, dat$size)
test <- chisq.test(table(dat$Species, dat$size))
test

# https://statsandr.com/blog/chi-square-test-of-independence-in-r/


