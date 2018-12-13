library(adventofcode18)
x <- readLines("./inst/input10.txt")

df <- x %>%
  parse_light_points()

steps <- df %>%
  step_light_points(11000)

# I solved this by doing a bunch of plotting and animating until
# I found the key frame where the message was formed.
library(ggplot2)

ggplot(steps[steps$step == 10407, ]) +
  aes(x = x, y = y) +
  geom_point(size = 3) +
  facet_wrap("step") +
  coord_fixed(.8) +
  scale_y_reverse()

p1 <- "PHLGRNFK"
p2 <- 10407

stopifnot(p1 == aoc18_solutions$day10a)
stopifnot(p2 == aoc18_solutions$day10b)
