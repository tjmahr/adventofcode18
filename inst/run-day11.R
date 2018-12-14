library(adventofcode18)
x <- readLines("./inst/input11.txt") %>%
  as.numeric()

m <- create_power_cell_matrix(300, x)
peak_corner <- pool_power_cells(m, 3)
p1 <- paste0(peak_corner$x, ",", peak_corner$y)

results <- search_power_cell_pools(m, to_search = 10:14)
best_row <- results[which.max(results$power), ]
p2 <- paste(best_row$x, best_row$y, best_row$span, sep = ",")

stopifnot(p1 == aoc18_solutions$day11a)
stopifnot(p2 == aoc18_solutions$day11b)
