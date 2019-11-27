library(tidyverse)
by_cyl <- mtcars %>% group_by(cyl)
by_cyl

by_cyl %>% summarise(disp = mean(disp), hp = mean(hp))
by_cyl %>% filter(disp == max(disp))

by_vs_am <- mtcars %>% group_by(vs, am)
by_vs_am

tbl <- tibble(
  x = 1:10,
  y = factor(rep(c("a", "c"), each = 5), levels = c("a", "b", "c"))
)

tbl %>%
  group_by(y) %>%
  group_rows()

str_c("letter: ", letters)
str_c(letters, "is for", "...")
str_c(letters, collapse = ", ")

str_flatten(letters, "-")

name <- "Fred"
age <- 50
anniversary <- as.Date("1991-10-22")
str_glue(
  "My name is {name}, ",
  "my age next year is {age + 1}, ",
  "and my anniversary is {format(anniversary, '%A, %B, %Y')}."
)

mtcars %>% str_glue_data("{rownames(.)} has {hp} hp")
head(mtcars)
