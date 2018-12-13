library(adventofcode18)
x <- readLines("./inst/input10.txt")

df <- x %>%
  parse_light_points()

steps <- df %>%
  step_light_points(11000)

library(ggplot2)
library(gganimate)

ggplot(steps %>% keep_rows(step == 10407)) +
  aes(x = x, y = y) +
  geom_point(size = 3) +
  facet_wrap("step")

# p1 <-
# p2 <-
#
# stopifnot(p1 == aoc18_solutions$day10a)
# stopifnot(p2 == aoc18_solutions$day10b)
