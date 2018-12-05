library(adventofcode18)
x <- readLines("./inst/input05.txt")

p1 <- f1(x)
p1 <- f2(x)

stopifnot(p1 == aoc18_solutions$day05a)
stopifnot(p2 == aoc18_solutions$day05b)
