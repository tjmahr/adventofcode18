# book-keeping

#' Day 06: Chronal Coordinates
#'
#' [Chronal Coordinates](https://adventofcode.com/2018/day/6)
#'
#' @name day06
#' @rdname day06
#' @details
#'
#' **Part One**
#'
#' The device on your wrist beeps several times, and once again you feel
#' like you\'re falling.
#'
#' \"[Situation
#' critical]{title="Why is the situation always critical? Why can't the situation just be boring for once?"},\"
#' the device announces. \"Destination indeterminate. Chronal interference
#' detected. Please specify new target coordinates.\"
#'
#' The device then produces a list of coordinates (your puzzle input). Are
#' they places it thinks are safe or dangerous? It recommends you check
#' manual page 729. The Elves did not give you a manual.
#'
#' *If they\'re dangerous,* maybe you can minimize the danger by finding
#' the coordinate that gives the largest distance from the other points.
#'
#' Using only the [Manhattan
#' distance](https://en.wikipedia.org/wiki/Taxicab_geometry), determine the
#' *area* around each coordinate by counting the number of
#' [integer](https://en.wikipedia.org/wiki/Integer) X,Y locations that are
#' *closest* to that coordinate (and aren\'t *tied in distance* to any
#' other coordinate).
#'
#' Your goal is to find the size of the *largest area* that isn\'t
#' infinite. For example, consider the following list of coordinates:
#'
#'     1, 1
#'     1, 6
#'     8, 3
#'     3, 4
#'     5, 5
#'     8, 9
#'
#' If we name these coordinates `A` through `F`, we can draw them on a
#' grid, putting `0,0` at the top left:
#'
#'     ..........
#'     .A........
#'     ..........
#'     ........C.
#'     ...D......
#'     .....E....
#'     .B........
#'     ..........
#'     ..........
#'     ........F.
#'
#' This view is partial - the actual grid extends infinitely in all
#' directions. Using the Manhattan distance, each location\'s closest
#' coordinate can be determined, shown here in lowercase:
#'
#'     aaaaa.cccc
#'     aAaaa.cccc
#'     aaaddecccc
#'     aadddeccCc
#'     ..dDdeeccc
#'     bb.deEeecc
#'     bBb.eeee..
#'     bbb.eeefff
#'     bbb.eeffff
#'     bbb.ffffFf
#'
#' Locations shown as `.` are equally far from two or more coordinates, and
#' so they don\'t count as being closest to any.
#'
#' In this example, the areas of coordinates A, B, C, and F are infinite -
#' while not shown here, their areas extend forever outside the visible
#' grid. However, the areas of coordinates D and E are finite: D is closest
#' to 9 locations, and E is closest to 17 (both including the coordinate\'s
#' location itself). Therefore, in this example, the size of the largest
#' area is *17*.
#'
#' *What is the size of the largest area* that isn\'t infinite?
#'
#' **Part Two**
#'
#' On the other hand, *if the coordinates are safe*, maybe the best you can
#' do is try to find a *region* near as many coordinates as possible.
#'
#' For example, suppose you want the sum of the [Manhattan
#' distance](https://en.wikipedia.org/wiki/Taxicab_geometry) to all of the
#' coordinates to be *less than 32*. For each location, add up the
#' distances to all of the given coordinates; if the total of those
#' distances is less than 32, that location is within the desired region.
#' Using the same coordinates as above, the resulting region looks like
#' this:
#'
#'     ..........
#'     .A........
#'     ..........
#'     ...###..C.
#'     ..#D###...
#'     ..###E#...
#'     .B.###....
#'     ..........
#'     ..........
#'     ........F.
#'
#' In particular, consider the highlighted location `4,3` located at the
#' top middle of the region. Its calculation is as follows, where `abs()`
#' is the [absolute value](https://en.wikipedia.org/wiki/Absolute_value)
#' function:
#'
#' -   Distance to coordinate A: `abs(4-1) + abs(3-1) =  5`
#' -   Distance to coordinate B: `abs(4-1) + abs(3-6) =  6`
#' -   Distance to coordinate C: `abs(4-8) + abs(3-3) =  4`
#' -   Distance to coordinate D: `abs(4-3) + abs(3-4) =  2`
#' -   Distance to coordinate E: `abs(4-5) + abs(3-5) =  3`
#' -   Distance to coordinate F: `abs(4-8) + abs(3-9) = 10`
#' -   Total distance: `5 + 6 + 4 + 2 + 3 + 10 = 30`
#'
#' Because the total distance to all coordinates (`30`) is less than 32,
#' the location is *within* the region.
#'
#' This region, which also includes coordinates D and E, has a total size
#' of *16*.
#'
#' Your actual region will need to be much larger than this example,
#' though, instead including all locations with a total distance of less
#' than *10000*.
#'
#' *What is the size of the region containing all locations which have a
#' total distance to all given coordinates of less than 10000?*
#'
#' @note I call the "coordinates" *beacons* to clarify my
#'   conceptualization of the problem.
#' @return For Part One, `read_beacon_coordinates(lines)` returns a dataframe
#'   with the problem input. `search_grid_around_beacons(df_beacon)` returns a
#'   matrix of the grid described by the problem input. The value of each cell
#'   is the labels of the nearest beacon or `"."` in the case of ties.
#'   `measure_finite_beacon_areas(area_grid)` returns a list of the
#'   beacon-areas. The keys (names) in the list are the labels of the beacons
#'   and the values are the finite areas.... For Part Two,
#'   `search_grid_for_safe_regions(df_beacon, threshold)` returns a matrix of
#'   the grid described by the problem input. The cell is in the safe region if
#'   its value is `"#"`. Otherwises, its value is `"."`.
#'   `measure_safe_area(safe_grid)` returns a list with the size of the `"#"` in
#'   the grid.
#' @examples
#' lines <- "
#'   1, 1
#'   1, 6
#'   8, 3
#'   3, 4
#'   5, 5
#'   8, 9"
#'
#' df <- lines %>%
#'   read_text_lines() %>%
#'   read_beacon_coordinates()
#'
#' # Match labels of description
#' df$label <- LETTERS[as.integer(df$label)]
#'
#' grid <- search_grid_around_beacons(df)
#' measure_finite_beacon_areas(grid)
#'
#' safe_grid <- search_grid_for_safe_regions(df, 32)
#' safe_grid
#'
#' measure_safe_area(safe_grid)
NULL

#' @param lines character vector with problem input. Each string in the vector
#'   should a be coordinate.
#' @rdname day06
#' @export
read_beacon_coordinates <- function(lines) {
  df <- read.csv(text = lines, header = FALSE) %>%
    setNames(c("x", "y"))
  df$label <- as.character(seq_along(df$x))
  df
}

#' @param df_beacons a dataframe created by `read_beacon_coordinates()`
#' @param pad integer indicating how far north, east, south, west of the beacons
#'   to go when making the grid.
#' @rdname day06
#' @export
search_grid_around_beacons <- function(df_beacons, pad = 1) {
  stopifnot(pad > 0)

  # smallest x,y in grid will be 1,1
  df_beacons$x <- df_beacons$x - min(df_beacons$x) + pad + 1
  df_beacons$y <- df_beacons$y - min(df_beacons$y) + pad + 1

  wide <- max(df_beacons$y) + pad
  high <- max(df_beacons$x) + pad

  to_search <- expand.grid(
    x = seq_len(wide),
    y = seq_len(high))

  to_search$nearest <- purrr::map2_chr(
    to_search$x,
    to_search$y,
    get_nearest_beacon,
    df_beacons = df_beacons)

  matrix(
    to_search$nearest,
    nrow = high,
    ncol = wide,
    byrow = TRUE)
}

get_nearest_beacon <- function(x, y, df_beacons) {
  dists <- abs((x - df_beacons$x)) + abs((y - df_beacons$y))
  nearest <- df_beacons$label[dists == min(dists)]
  if (length(nearest) != 1) "." else nearest
}

#' @param area_grid a matrix created by `search_grid_around_beacons()`
#' @rdname day06
#' @export
measure_finite_beacon_areas <- function(area_grid) {
  # Values that show up in outer rectangle will have an infinite area
  top_bottom_edges <- as.vector(area_grid[c(1, nrow(area_grid)), ])
  left_right_edges <- as.vector(area_grid[, c(1, ncol(area_grid))])

  # Ignore values in outer rectangle. Make sure "." (ties) is also ignored
  to_exclude <- c(top_bottom_edges, left_right_edges) %>%
    append(".") %>%
    unique() %>%
    sort()
  counts <- table(area_grid) %>% as.list()
  counts[to_exclude] <- NULL
  counts
}

#' @param threshold a point is safe when the total distance between the point
#'   and all the beacons is less than the threshold
#' @rdname day06
#' @export
search_grid_for_safe_regions <- function(df_beacons, threshold, pad = 1) {
  # smallest x,y in grid will be 1,1
  df_beacons$x <- df_beacons$x - min(df_beacons$x) + pad + 1
  df_beacons$y <- df_beacons$y - min(df_beacons$y) + pad + 1

  wide <- max(df_beacons$y) + pad
  high <- max(df_beacons$x) + pad

  to_search <- expand.grid(
    x = seq_len(wide),
    y = seq_len(high))

  to_search$safe <- purrr::map2_chr(
    to_search$x,
    to_search$y,
    is_in_safe_region,
    df_beacons = df_beacons,
    threshold = threshold)

  matrix(
    to_search$safe,
    nrow = high,
    ncol = wide,
    byrow = TRUE)
}

is_in_safe_region <- function(x, y, df_beacons, threshold) {
  dists <- abs((x - df_beacons$x)) + abs((y - df_beacons$y))
  if (sum(dists) < threshold) "#" else "."
}

#' @param safe_grid a matrix created by `search_grid_for_safe_regions()`
#' @rdname day06
#' @export
measure_safe_area <- function(safe_grid) {
  counts <- table(safe_grid) %>% as.list()
  counts
}
