library(adventofcode18)
x <- readLines("./inst/input09.txt")

p1_history <- x %>%
  parse_marble_description() %>%
  do.call(run_marbles, .)

p1 <- p1_history %>%
  get_marble_high_score()

stopifnot(p1 == aoc18_solutions$day09a)


p2_problem <- parse_marble_description(x)
p2_problem$marbles <- p2_problem$marbles * 100

p2_history <- run_marbles(p2_problem$players, p2_problem$marbles)
#
# library(ggplot2)
# ggplot(p1_history) + aes(x = turn, y = score, group = player) + geom_line()
#
# ggplot(p1_history) +
#   aes(x = turn, y = score) +
#   geom_line() +
#   stat_summary(aes(group = player), fun.y = sum, geom = "line")
#
# for(turn)
#
# a1 <- p1_history %>%
#   keep_rows(turn < 20000) %>%
#   get_marble_high_score()
#
# a2 <- p1_history %>%
#   keep_rows(turn < 21000) %>%
#   get_marble_high_score()
#
# a3 <- p1_history %>%
#   keep_rows(turn < 22000) %>%
#   get_marble_high_score()
#
# a4 <- p1_history %>%
#   keep_rows(turn < 23000) %>%
#   get_marble_high_score()
#
# c(a1, a2, a3, a4) %>% diff
#
# a2 <- p1_history[1:21000, ] %>%
#   get_marble_high_score()
# a3 <- p1_history[1:22000, ] %>%
#   get_marble_high_score()
# a1 <- p1_history[1:20000, ] %>%
#   get_marble_high_score()

stopifnot(p2 == aoc18_solutions$day09b)
