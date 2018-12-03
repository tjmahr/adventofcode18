library(adventofcode18)
x <- readLines("./inst/input03.txt")

p1 <- count_overlapping_fabric_claims(x)
p1 <- f2(x)

stopifnot(p1 == aoc18_solutions$day03a)
stopifnot(p2 == aoc18_solutions$day03b)
