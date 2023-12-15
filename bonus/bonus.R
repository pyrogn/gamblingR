library(readr)
library(stringr)
library(purrr)
library(cli)

tree_orig <- read_file("bonus/tree.txt")

splitted_tree <- str_split(tree_orig, "\n")[[1]]

# делим дерево на ветви и ствол
is_top <- splitted_tree |>
  map_lgl(~ str_detect(., pattern = "\\*"))

brown <- make_ansi_style("#a67049")

top <- splitted_tree[is_top]
trunk <- splitted_tree[!is_top] |>
  map_chr(~ brown(.) |> str_conv("utf-8"))

# определение порядкового номера ветки для градиента
cnt_stars <- map_int(top, \(x) str_count(x, "\\*"))
cnt_stars_shifed <- cnt_stars[2:length(cnt_stars)]
cnt_diff <- cnt_stars - cnt_stars_shifed
ind_change <- c(0, which(cnt_diff > 0))

get_rn_branch <- function(ind) {
  diff <- ind_change[ind_change < ind] |> max()
  ind - diff
}


base_color <- c(30, 80, 30)
brighten_color <- function(color, n_iter = 1, factor = 1.05) {
  (color * factor**n_iter) |>
    pmin(255) |>
    round()
  # какой-то сильный градиент...
}

vec_to_color <- function(vec) {
  vec |>
    matrix(ncol = 3) |>
    rgb(maxColorValue = 255)
}

# получить порядковый номер ветки в группе веток
branch2ansi_style <- function(ind) {
  get_rn_branch(ind) |>
    brighten_color(base_color) |>
    vec_to_color() |>
    make_ansi_style()
}


# замена одной иголки на лампочку в позиции
place_light <- function(len_string, string_mod, position, light) {
  string <- rep("*", len_string) |> str_flatten()
  c(
    string_mod(str_sub(string, 0, position - 1)),
    light,
    string_mod(str_sub(string, position + 1, len_string))
  ) |>
    str_flatten()
}

# Посчитать на ветке звездочки и пробелы
split_level <- function(row) {
  n_spaces <- str_count(row, " ")
  n <- nchar(row)
  n_stars <- n - n_spaces
  spaces <- rep(" ", n_spaces) |> str_flatten()
  return(list(n_spaces = n_spaces, n_stars = n_stars))
}
# цвета гирлянд
colors <- c(
  col_br_red,
  col_br_yellow,
  col_br_white,
  col_br_blue
)
random_n <- function(n) {
  sample(1:n, size = 1)
}
rep_concat <- function(sym, n) {
  rep(sym, n) |>
    str_flatten()
}
# обработка одной ветки
process_row <- function(row, ind) {
  splitted_n <- split_level(row)
  style <- branch2ansi_style(ind)
  if (random_n(2) == 1) { # 50% chance
    position <- random_n(splitted_n$n_stars)
    color <- colors[[random_n(length(colors))]]
    stars <- place_light(
      splitted_n$n_stars,
      style,
      position,
      color("O")
    )
  } else {
    stars <- rep_concat("*", splitted_n$n_stars) |>
      style()
  }
  spaces <- rep_concat(" ", splitted_n$n_space)
  c(spaces, stars) |> str_flatten()
}

for (i in 1:3000) {
  # с глаз долой прошлый кадр
  newlines <- rep_concat("\n", 8)

  top_mod <- imap_chr(top, process_row)
  tree_colored <- c(top_mod, trunk) |>
    str_flatten(collapse = "\n")

  to_cat <- c(newlines, tree_colored) |> str_flatten()

  # system('clear') # might flicker
  cat(to_cat)
  Sys.sleep(.3)
}
