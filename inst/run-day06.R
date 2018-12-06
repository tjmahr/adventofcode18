library(adventofcode18)
x <- readLines("./inst/input06.txt")

# p1 <- f06a(x)
df <- read_beacon_coordinates(x)
grid <- search_grid_around_beacons(df)
areas <- measure_finite_beacon_areas(grid)

p1 <- max(unlist(areas))

# I had to add the `pad` argument because one of the edges of grid still had
# safe regions, which means that I hadn't found all of them yet.
safe_grid <- search_grid_for_safe_regions(df, 10000, pad = 20)
areas <- measure_safe_area(safe_grid)

stopifnot(p1 == aoc18_solutions$day06a)
stopifnot(p2 == aoc18_solutions$day06b)
