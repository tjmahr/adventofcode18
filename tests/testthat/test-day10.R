context("test-day10")

test_that("multiplication works", {
x <- "
  position=< 9,  1> velocity=< 0,  2>
  position=< 7,  0> velocity=<-1,  0>
  position=< 3, -2> velocity=<-1,  1>
  position=< 6, 10> velocity=<-2, -1>
  position=< 2, -4> velocity=< 2,  2>
  position=<-6, 10> velocity=< 2, -2>
  position=< 1,  8> velocity=< 1, -1>
  position=< 1,  7> velocity=< 1,  0>
  position=<-3, 11> velocity=< 1, -2>
  position=< 7,  6> velocity=<-1, -1>
  position=<-2,  3> velocity=< 1,  0>
  position=<-4,  3> velocity=< 2,  0>
  position=<10, -3> velocity=<-1,  1>
  position=< 5, 11> velocity=< 1, -2>
  position=< 4,  7> velocity=< 0, -1>
  position=< 8, -2> velocity=< 0,  1>
  position=<15,  0> velocity=<-2,  0>
  position=< 1,  6> velocity=< 1,  0>
  position=< 8,  9> velocity=< 0, -1>
  position=< 3,  3> velocity=<-1,  1>
  position=< 0,  5> velocity=< 0, -1>
  position=<-2,  2> velocity=< 2,  0.
  position=< 5, -2> velocity=< 1,  2>
  position=< 1,  4> velocity=< 2,  1>
  position=<-2,  7> velocity=< 2, -2>
  position=< 3,  6> velocity=<-1, -1>
  position=< 5,  0> velocity=< 1,  0>
  position=<-6,  0> velocity=< 2,  0>
  position=< 5,  9> velocity=< 1, -2>
  position=<14,  7> velocity=<-2,  0>
  position=<-3,  6> velocity=< 2, -1>
  "


  df <- x %>%
    read_text_lines() %>%
    parse_light_points()

  steps <- df %>%
    step_light_points(6)

  library(ggplot2)

  ggplot(steps) +
    aes(x = x, y = y) +
    geom_point() +
    facet_wrap("step")

  ggplot(steps) +
    aes(x = x, y = y) +
    geom_point(size = 3) +
    transition_states(step, .1, .15) +
    labs(title = 'Step: {closest_state}')


})


