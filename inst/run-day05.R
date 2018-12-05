library(adventofcode18)
x <- readLines("./inst/input05.txt")

p1 <- run_polymer_reaction(x)

# that's a slow one!
p2 <- simulate_polymer_reactions(x)

stopifnot(nchar(p1) == aoc18_solutions$day05a)
stopifnot(min(unlist(p2)))
