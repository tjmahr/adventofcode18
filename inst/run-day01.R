library(adventofcode18)
x <- readLines("./inst/input01.txt")
x <- as.integer(x)
p1 <- sum_frequency(x)
p2 <- analyze_frequency_stream(x)

stopifnot(p1 == aoc18_solutions$day01a)
stopifnot(p2 == aoc18_solutions$day01b)
