res=map_dbl(1:50, ~pull_bandit(winning_p_distr))
# |>


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


