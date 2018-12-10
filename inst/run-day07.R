library(adventofcode18)
x <- readLines("./inst/input07.txt")

df <- read_sleigh_instructions(x)
p1_steps <- sort_sleigh_steps(df)
p1 <- paste0(p1_steps, collapse = "")

p1 <- f07a(x)
p2 <- f07b(x)

stopifnot(p1 == aoc18_solutions$day07a)
stopifnot(p2 == aoc18_solutions$day07b)
