library(tidyverse)

# import data ----------------------------------------------

# r2 = read_rds("data/s1.RData")

# helpers----------------------------------------------


get_rewards_simulations <- function(results) {
  map(results, ~ .x$rewards |> unlist())
}

# plots ----------------------------------------------------

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
