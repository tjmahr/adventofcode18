library(adventofcode18)
x <- readLines("./inst/input02.txt")

p1 <- checksum_box_id(x)
p2 <- find_one_character_neighbors(x)

stopifnot(p1 == aoc18_solutions$day02a)
stopifnot(p2 == aoc18_solutions$day02b)
