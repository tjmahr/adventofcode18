library(adventofcode18)
x <- readLines("./inst/input09.txt")

p1_history <- x %>%
  parse_marble_description() %>%
  do.call(run_marbles_c, .)

p1 <- p1_history %>%
  get_marble_high_score()

stopifnot(p1 == aoc18_solutions$day09a)

# Don't run. It took 10 hours.

# p2_problem <- parse_marble_description(x)
# p2_history_2 <- run_marbles_c(p2_problem$players, p2_problem$marbles * 100)
#
# p2_history_2 %>%
#   get_marble_high_score()
#
# stopifnot(p2 == aoc18_solutions$day09b)
