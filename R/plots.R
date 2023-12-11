# styler:::style_active_file()

library(tidyverse)
library(ggridges)
library(slider)
library(ggrepel)
library(glue)
library(latex2exp)
# cannot use relative path because of qmd file
source("/Users/pyro/R/gamblingR/R/constants.R")

# Выглядит очень страшно, и это тяжело читать. И редактировать.
# Я даже затрудняюсь это комментировать, не добавит ясности.

# Есть идеи, как можно было бы это упростить.
# 1. На этапе ранее как можно меньше создать вложенных структур
# 2. За место map_depth одного уровня использовать map(data, map())
# 3. Получше познакомиться с паттернами работы в tidy формате

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


# plots and data -----------------------------------------------------

sim_rewards <- map(sim_data, get_rewards_simulations)
sim_ind <- map(sim_data, get_index_simulations)

cash_profit <- sim_rewards |>
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


mutate_strategy <- function(df) {
  df |>
    mutate(
      strategy =
        factor(
          strategy,
          labels = c(
            "Randy", "NotLoser", "T-man", "Conservative", "Cheater"
          )
        )
    )
}

# Остаток на балансе под конец игры

profit_ci <- cash_profit |>
  mutate(x = x / n_steps) |>
  group_by(strategy) |>
  summarize(
    mean = mean(x),
    se = sd(x) / sqrt(n()),
    n = n()
  ) |>
  mutate(delta_ci = qt(0.975, df = n) * se) |> # alpha=95%
  mutate(
    lb = mean - delta_ci,
    hb = mean + delta_ci
  )



plot_expected_profit <- function(profit_ci) {
  profit_ci |>
    mutate_strategy() |>
    ggplot(
      aes(
        y = fct_reorder(strategy, mean), x = mean, group = strategy
      )
    ) +
    geom_errorbarh(aes(xmin = lb, xmax = hb)) +
    labs(
      x = "Average profit per a game", y = "Strategy",
      title = TeX(paste0(
        "Comparison of expected profit from a game. ",
        "$\\alpha$ = 95%"
      ))
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(-2, 2, .2)) +
    geom_vline(xintercept = 0, color = "red", alpha = 0.8)
}

# plot_expected_profit(profit_ci)


# Остаток на балансе в ходе игры
plot_left_on_step <- function(detail_wins) {
  detail_wins |>
    mutate_strategy() |>
    mutate(label = ifelse(x == n_steps, as.character(strategy), NA)) |>
    ggplot(aes(
      x = x, y = y, color = strategy
    )) +
    geom_line(size = 1) +
    labs(
      x = "Step", y = "Cumulative profit",
      title = "Game profit estimation by steps"
    ) +
    # coord_cartesian(xlim = c(0, n_steps)) + # не обрезает почему-то
    theme_minimal() +
    geom_hline(yintercept = 0, color = "black", linetype = "dotted") +
    geom_label_repel(aes(label = label),
      nudge_x = 50,
      na.rm = TRUE
    ) +
    scale_color_discrete(guide = FALSE)
}
# plot_left_on_step(rewards_at_step)


# Процент банкротов на определенном шаге

plot_bankrupcy <- function(n_bankrupt_at_step) {
  n_bankrupt_at_step |>
    mutate_strategy() |>
    mutate(label = ifelse(
      step == n_steps, as.character(strategy), NA
    )) |>
    ggplot(aes(x = step, y = prop, color = strategy)) +
    geom_line() +
    theme_minimal() +
    labs(
      y = "Percent of bankrupts",
      x = "Step",
      title = "Speed of bankrupcy"
    ) +
    scale_y_continuous(labels = scales::percent) +
    geom_label_repel(aes(label = label),
      nudge_x = 50,
      na.rm = TRUE
    ) +
    scale_color_discrete(guide = FALSE)
}
# plot_bankrupcy(n_bankrupt_at_step)


# Про индексы и специфику выбора

# Процент использования нужной машины до смены и после смены

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
    mutate(position_look = if_else( # maybe edit position
      -30 <= rel_position & rel_position <= 300,
      rel_position, NA
    )) |>
    filter(!is.na(position_look)) |>
    select(index, step_n, position_look)


  choice_at_step <- data$ind |>
    unlist() |>
    as_tibble() |>
    mutate(step_n = row_number()) |>
    rename(choice = value)

  res <- history_changes |>
    inner_join(choice_at_step, by = "step_n") |>
    mutate(is_target = as.numeric(index == choice)) |>
    group_by(position_look) |>
    summarise(n = n(), target_hit = sum(is_target))
  res
}



position_hits_df <- sim_data |>
  map(~ map_dfr(., get_hits_on_position) |>
    group_by(position_look) |>
    summarize(prop_hit = sum(target_hit) / sum(n))) |>
  bind_rows(.id = "strategy") |>
  suppressMessages()

plot_hitting_target <- function(position_hits_df) {
  position_hits_df |>
    mutate_strategy() |>
    ggplot(aes(x = position_look, y = prop_hit, color = strategy)) +
    geom_line() +
    geom_vline(xintercept = 0, linewidth = 1, color = "red") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(-200, 300, 20)) +
    labs(
      x = "Relative step to winning machine change",
      y = "Proportion hits to winning machine",
      title = "Speed of finding a winning machine"
    )
}
# plot_hitting_target(position_hits_df)

# График с дисперсией выбора

uniq_elems_slide <- function(vec) {
  slide_dbl(
    vec,
    \(x) x |>
      unique() |>
      length(),
    .before = 10, .after = 10
  )
}

uniq_ind_data <- sim_ind |>
  map_depth(2, uniq_elems_slide) |>
  map(transpose) |>
  map_depth(2, unlist) |>
  map_depth(2, mean) |>
  map_depth(1, unlist) |>
  map(~ tibble(average_lookout = .) |> mutate(step = row_number())) |>
  bind_rows(.id = "strategy")

plot_machine_diversity <- function(uniq_ind_data) {
  uniq_ind_data |>
    mutate_strategy() |>
    ggplot(aes(x = step, y = average_lookout, color = strategy)) +
    geom_line() +
    theme_minimal() +
    labs(
      x = "Step",
      title = TeX(glue(
        "Machines diversity in step $\\pm$ 10 steps"
      )),
      y = "Avg number of machines"
    ) +
    coord_cartesian(ylim = c(1, n_bandits)) +
    scale_y_continuous(breaks = 1:n_bandits)
}
# plot_machine_diversity(uniq_ind_data)
