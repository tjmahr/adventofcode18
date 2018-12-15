library(adventofcode18)
x <- readLines("./inst/input12.txt")

p1 <- f12a(x)
p2 <- f12b(x)

stopifnot(p1 == aoc18_solutions$day12a)
stopifnot(p2 == aoc18_solutions$day12b)
