library(adventofcode18)
x <- readLines("./inst/input04.txt")

p1_df <- find_sleepiest_minute_of_sleepiest_guard(x)
p1 <- p1_df$minute * p1_df$guard

p2_df <- find_guard_with_sleepiest_minute(x)
p2 <- p2_df$minute * p2_df$guard

stopifnot(p1 == aoc18_solutions$day04a)
stopifnot(p2 == aoc18_solutions$day04b)
