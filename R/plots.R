styler:::style_active_file()

library(tidyverse)
library(ggridges)
library(slider)
source("R/constants.R")

# import data ----------------------------------------------

sim_data <- map(1:3, ~ read_rds(name_strategy(.x)))

# helpers----------------------------------------------

get_rewards_simulations <- function(results) {
  map(results, ~ .x$rewards |> unlist())
}

sim_rewards <- map(sim_data, get_rewards_simulations)

# rewards_at_step <- map(sim_rewards, cumsum)

cash_revenue <- sim_rewards |>
  map_depth(2, sum) |>
  map_depth(1, unlist) |>
  map(\(x) x - price * n_steps) |>
  tibble(x = _) |>
  mutate(strategy = as.factor(row_number())) |>
  unnest(x)

# looks unsafe but what can we do...
rewards_at_step <- sim_rewards |>
  map(transpose) |>
  map_depth(2, unlist) |>
  map_depth(2, mean) |>
  map(unlist) |>
  map(~ cumsum(.x) - cumsum(rep(price, n_steps))) |>
  tibble(y = _) |>
  mutate(strategy = row_number()) |>
  unnest(y) |>
  group_by(strategy) |>
  mutate(x = row_number()) |>
  ungroup() |>
  mutate(strategy = as.factor(strategy))


bankrupt_at_step <- sim_rewards |>
  map_depth(2, ~ cash + cumsum(.x) - cumsum(rep(price, n_steps))) |>
  map_depth(2, ~ which(.x < price)[1]) |>
  map(unlist)

n_bankrupt_at_step <- bankrupt_at_step |>
  map_depth(
    1,
    \(strategy) map_int(
      1:n_steps,
      \(step) length(which(strategy < step))
    )
  ) |>
  tibble(x = _) |>
  mutate(strategy = as.factor(row_number())) |>
  unnest(x) |>
  group_by(strategy) |>
  mutate(step = row_number(), prop = x / n_steps) |>
  ungroup()



# plots ----------------------------------------------------


# Остаток на балансе под конец игры
plot_end_cash <- function(cash_left) {
  ggplot(cash_revenue, aes(x = x, y = strategy, fill = strategy)) +
    geom_boxplot() +
    theme_minimal()
}
plot_end_cash(cash_revenue)


# Остаток на балансе в ходе игры
plot_left_on_step <- function(detail_wins) {
  ggplot(data = detail_wins, aes(
    x = x, y = y, color = strategy, group = strategy
  )) +
    geom_line() +
    theme_minimal()
}
plot_left_on_step(rewards_at_step)

# Процент банкротов на определенном шаге

n_bankrupt_at_step |>
  ggplot(aes(x = step, y = prop, color = strategy)) +
  geom_line() +
  theme_minimal() +
  labs(
    y = "Процент банкротов на шаге",
    x = "Шаг"
  ) +
  scale_y_continuous(labels = scales::percent)





# Про индексы и специфику выбора


# создание датафрейма с индексом и шагом (уфффф...--0[_32][}{}])
sim_data[[3]][[1]]$changes |>
  transpose() |>
  map(unlist) |>
  as_tibble(.name_repair = "unique") |>
  rename(index = ...1, step_n = ...2) |>
  mutate(to = lead(step_n - 1, default = n_steps, order_by = step_n)) |>
  rowwise() |>
  mutate(step_n = list(seq(step_n, to))) |>
  unnest(step_n) |>
  select(index, step_n)

sim_data[[3]][[1]]$ind |> unlist() |>
  as_tibble() |>
  mutate(step_n = row_number()) |>
  rename(index = value) |>
  mutate(is_target = as.numeric(index == 9)) |>
  mutate(
    moving_avg = slider::slide_dbl(is_target, mean, .before = 10, after=10)
    )

# make inner join and use target index from 1 table

# Процент использования нужной машины до смены и после смены
