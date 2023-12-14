library(readr)
library(stringr)
library(purrr)
library(cli)

res = read_file('bonus/tree.txt')

splitted_res = str_split(res, '\n')[[1]]
splitted_res

is_top = splitted_res |>
  map_lgl(~str_detect(.,pattern = '\\*'))

top = splitted_res[is_top]
trunk = splitted_res[!is_top]

colored_res = do.call(c, list(col_br_green(top), trunk)) |>
  str_c('\n')

# top[[2]]

# place_light = function(row)

# make colors # read docs
# make_ansi_style("#12ff51")

for (i in 1:30) {
  # system('clear') # otherwise it is flickering badly
  # cat('\n\n\n\n\n\n')
  # cat(i, '\n')
  cat(colored_res)
  Sys.sleep(.3)
}

