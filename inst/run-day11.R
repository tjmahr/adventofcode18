library(adventofcode18)
x <- readLines("./inst/input11.txt")

p1 <- f11a(x)
p2 <- f11b(x)

stopifnot(p1 == aoc18_solutions$day11a)
stopifnot(p2 == aoc18_solutions$day11b)
