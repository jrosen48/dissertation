library(tidyverse)

my_dir <- "_bookdown_files/rosenberg-dissertation_files/figure-latex/"

list.files(my_dir)

for (i in 1:length(list.files(my_dir))) {
  x <- stringr::str_c(my_dir,
                      list.files(my_dir)[i])
  print(x)
  y <- str_remove(x, "-1.pdf")
  print(y)
  # z <- str_c(y, ".pdf")
  file.rename(from = x, to = y)
}
