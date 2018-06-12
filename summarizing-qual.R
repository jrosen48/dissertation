library(googlesheets)
library(tidyverse)

g <- gs_title("New Overall Coding Sheet")
d <- gs_read(g)
d

# Generate

d %>%
  mutate(row_number = row_number()) %>%
  slice(1:239) %>%
  select(Generate:gen_unclear) %>%
  filter(Generate > 0) %>%
  summarize_all(sum, na.rm = TRUE)

# Others

d %>%
  mutate(row_number = row_number()) %>%
  slice(1:239) %>%
  select(Communicate:com_unclear) %>%
  filter(Communicate > 0) %>%
  summarize_all(sum, na.rm = TRUE) / 103
