library(adventofcode18)
x <- readLines("./inst/input08.txt")

p1 <- parse_license(x)
p2 <- parse_license2(x)

stopifnot(sum(p1) == aoc18_solutions$day08a)
stopifnot(p2 == aoc18_solutions$day08b)
