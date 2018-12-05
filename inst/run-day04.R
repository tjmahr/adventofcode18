library(adventofcode18)
x <- readLines("./inst/input04.txt")

p1 <- f1(x)
p1 <- f2(x)

stopifnot(p1 == aoc18_solutions$day04a)
stopifnot(p2 == aoc18_solutions$day04b)
