styler:::style_active_file()

library(tidyverse)
library(ggridges)
library(slider)
source("R/constants.R")

# import data ----------------------------------------------

sim_data <- map(1:5, ~ read_rds(name_strategy(.x)))

# helpers----------------------------------------------

get_attr_data <- function(data, attribute) {
  map(data, ~ .x[[attribute]] |> unlist())
}

get_rewards_simulations <- function(data) {
  get_attr_data(data, "rewards")
}

get_index_simulations <- function(data) {
  get_attr_data(data, "ind")
}

sim_rewards <- map(sim_data, get_rewards_simulations)
sim_ind <- map(sim_data, get_index_simulations)

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
get_hits_on_position <- function(data) {
  history_changes <- data$changes |>
    transpose() |>
    map(unlist) |>
    as_tibble(.name_repair = "unique") |>
    rename(index = ...1, from = ...2) |>
    mutate(to = lead(from - 1, default = n_steps, order_by = from) |>
      as.integer()) |>
    rowwise() |>
    mutate(step_n = list(seq(from, to))) |>
    unnest(step_n) |>
    mutate(
      is_change = step_n == from,
      pos_lookup = step_n - from + 1,
      neg_lookup = step_n - to - 1
    ) |>
    rowwise() |>
    mutate(
      rel_position = if_else(
        pos_lookup < -neg_lookup, pos_lookup, neg_lookup
      )
    ) |>
    ungroup() |>
    mutate(position_look = if_else(
      abs(rel_position) < 300, rel_position, NA
    )) |>
    select(index, step_n, position_look)


  choice_at_step <- sim_data[[3]][[1]]$ind |>
    unlist() |>
    as_tibble() |>
    mutate(step_n = row_number()) |>
    rename(choice = value)

  res <- history_changes |>
    inner_join(choice_at_step, by = "step_n") |>
    mutate(is_target = as.numeric(index == choice)) |>
    filter(!is.na(position_look)) |>
    group_by(position_look) |>
    summarise(n = n(), target_hit = sum(is_target))
  res
}


# Добавить график с дисперсией выбора

uniq_elems_slide <- function(vec) {
  slide_dbl(vec, \(x) x |>
    unique() |>
    length(), .before = 10, .after = 10)
}

sim_ind |>
  map_depth(2, uniq_elems_slide) |>
  map(transpose) |>
  map_depth(2, unlist) |>
  map_depth(2, mean) |>
  map_depth(1, unlist)




# make inner join and use target index from 1 table
# add column - steps to change: -3,-2,-1,0,1,2,3... Some may be NA
# map to many iterations (HOW?)
# group by delta, summarize mean
# plot it as lineplot probably. Or maybe something with CI.

# Процент использования нужной машины до смены и после смены
