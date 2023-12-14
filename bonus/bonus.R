library(readr)
library(stringr)
library(purrr)
library(cli)

# Код пока что очень некрасивый

res = read_file('bonus/tree.txt')

splitted_res = str_split(res, '\n')[[1]]
splitted_res

is_top = splitted_res |>
  map_lgl(~str_detect(.,pattern = '\\*'))

brown = make_ansi_style("#a67049")

top = splitted_res[is_top]
trunk = splitted_res[!is_top] |>
  map_chr(~brown(.) |> str_conv('utf-8'))


place_light = function(len_string, string_mod, position, light) {
  string = rep('*', len_string) |> str_flatten()
  c(
    string_mod(str_sub(string, 0, position-1)),
    light,
    string_mod(str_sub(string,position+1, len_string))
  ) |>
    str_flatten()
}

split_level = function(row) {
  n_spaces = str_count(row, ' ')
  n = nchar(row)
  n_stars = n - n_spaces
  spaces = rep(' ', n_spaces) |> str_flatten()
  return(list(space=n_spaces, star=n_stars))
}
colors = c(
  col_br_red,
  col_br_yellow,
  col_br_white,
  col_br_blue
)
process_row = function(row) {
  splitted_n = split_level(row)
  if (sample(1:2, size=1) == 1) {
    position = sample(1:splitted_n$star, size=1)
    color = colors[[sample(1:length(colors), size=1)]]
    stars = place_light(splitted_n$star, col_green, position, color('O'))
  } else {
    stars = rep('*', splitted_n$star) |> str_flatten() |>
      col_green()
  }
  spaces = rep(' ', splitted_n$space) |> str_flatten()
  c(spaces, stars) |> str_flatten()

}

for (i in 1:30) {
  newlines = rep('\n', 8) |> str_flatten()
  top_mod = map_chr(top, process_row)
  c_res = c(top_mod, trunk)
  final_res = c_res |>
    str_c('\n') |>
    str_flatten()

  system('clear') # might flicker
  cat(c(newlines, final_res))
  Sys.sleep(.3)
}

