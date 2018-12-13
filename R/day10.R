# I solved this one using plotting because the main task is to read a string
# spelled out with a bunch of dots in a grid.

#' Day 10: The Stars Align
#'
#' [The Stars Align](https://adventofcode.com/2018/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' **Part One**
#'
#' It's no use; your navigation system simply isn't capable of providing
#' [walking
#' directions]{title="At the iceberg, use any lane to turn left. Then, swim for eight thousand miles."}
#' in the arctic circle, and certainly not in 1018.
#'
#' The Elves suggest an alternative. In times like these, North Pole rescue
#' operations will arrange points of light in the sky to guide missing
#' Elves back to base. Unfortunately, the message is easy to miss: the
#' points move slowly enough that it takes hours to align them, but have so
#' much momentum that they only stay aligned for a second. If you blink at
#' the wrong time, it might be hours before another message appears.
#'
#' You can see these points of light floating in the distance, and record
#' their position in the sky and their velocity, the relative change in
#' position per second (your puzzle input). The coordinates are all given
#' from your perspective; given enough time, those positions and velocities
#' will move the points into a cohesive message!
#'
#' Rather than wait, you decide to fast-forward the process and calculate
#' what the points will eventually spell.
#'
#' For example, suppose you note the following points:
#'
#'     position=< 9,  1> velocity=< 0,  2>
#'     position=< 7,  0> velocity=
#'     position=< 3, -2> velocity=
#'     position=< 6, 10> velocity=
#'     position=< 2, -4> velocity=< 2,  2>
#'     position= velocity=< 2, -2>
#'     position=< 1,  8> velocity=< 1, -1>
#'     position=< 1,  7> velocity=< 1,  0>
#'     position= velocity=< 1, -2>
#'     position=< 7,  6> velocity=
#'     position= velocity=< 1,  0>
#'     position= velocity=< 2,  0>
#'     position=<10, -3> velocity=
#'     position=< 5, 11> velocity=< 1, -2>
#'     position=< 4,  7> velocity=< 0, -1>
#'     position=< 8, -2> velocity=< 0,  1>
#'     position=<15,  0> velocity=
#'     position=< 1,  6> velocity=< 1,  0>
#'     position=< 8,  9> velocity=< 0, -1>
#'     position=< 3,  3> velocity=
#'     position=< 0,  5> velocity=< 0, -1>
#'     position= velocity=< 2,  0>
#'     position=< 5, -2> velocity=< 1,  2>
#'     position=< 1,  4> velocity=< 2,  1>
#'     position= velocity=< 2, -2>
#'     position=< 3,  6> velocity=
#'     position=< 5,  0> velocity=< 1,  0>
#'     position= velocity=< 2,  0>
#'     position=< 5,  9> velocity=< 1, -2>
#'     position=<14,  7> velocity=
#'     position= velocity=< 2, -1>
#'
#' Each line represents one point. Positions are given as `<X, Y>` pairs: X
#' represents how far left (negative) or right (positive) the point
#' appears, while Y represents how far up (negative) or down (positive) the
#' point appears.
#'
#' At `0` seconds, each point has the position given. Each second, each
#' point's velocity is added to its position. So, a point with velocity
#' `<1, -2>` is moving to the right, but is moving upward twice as quickly.
#' If this point's initial position were `<3, 9>`, after `3` seconds, its
#' position would become `<6, 3>`.
#'
#' Over time, the points listed above would move like this:
#'
#'     Initially:
#'     ........#.............
#'     ................#.....
#'     .........#.#..#.......
#'     ......................
#'     #..........#.#.......#
#'     ...............#......
#'     ....#.................
#'     ..#.#....#............
#'     .......#..............
#'     ......#...............
#'     ...#...#.#...#........
#'     ....#..#..#.........#.
#'     .......#..............
#'     ...........#..#.......
#'     #...........#.........
#'     ...#.......#..........
#'
#'     After 1 second:
#'     ......................
#'     ......................
#'     ..........#....#......
#'     ........#.....#.......
#'     ..#.........#......#..
#'     ......................
#'     ......#...............
#'     ....##.........#......
#'     ......#.#.............
#'     .....##.##..#.........
#'     ........#.#...........
#'     ........#...#.....#...
#'     ..#...........#.......
#'     ....#.....#.#.........
#'     ......................
#'     ......................
#'
#'     After 2 seconds:
#'     ......................
#'     ......................
#'     ......................
#'     ..............#.......
#'     ....#..#...####..#....
#'     ......................
#'     ........#....#........
#'     ......#.#.............
#'     .......#...#..........
#'     .......#..#..#.#......
#'     ....#....#.#..........
#'     .....#...#...##.#.....
#'     ........#.............
#'     ......................
#'     ......................
#'     ......................
#'
#'     After 3 seconds:
#'     ......................
#'     ......................
#'     ......................
#'     ......................
#'     ......#...#..###......
#'     ......#...#...#.......
#'     ......#...#...#.......
#'     ......#####...#.......
#'     ......#...#...#.......
#'     ......#...#...#.......
#'     ......#...#...#.......
#'     ......#...#..###......
#'     ......................
#'     ......................
#'     ......................
#'     ......................
#'
#'     After 4 seconds:
#'     ......................
#'     ......................
#'     ......................
#'     ............#.........
#'     ........##...#.#......
#'     ......#.....#..#......
#'     .....#..##.##.#.......
#'     .......##.#....#......
#'     ...........#....#.....
#'     ..............#.......
#'     ....#......#...#......
#'     .....#.....##.........
#'     ...............#......
#'     ...............#......
#'     ......................
#'     ......................
#'
#' After 3 seconds, the message appeared briefly: `HI`. Of course, your
#' message will be much longer and will take many more seconds to appear.
#'
#' *What message will eventually appear in the sky?*
#'
#' **Part Two**
#'
#' Good thing you didn\'t have to wait, because that would have taken a
#' long time - much longer than the `3` seconds in the example above.
#'
#' Impressed by your sub-hour communication capabilities, the Elves are
#' curious: *exactly how many seconds would they have needed to wait for
#' that message to appear?*
#'
#' @param x a character vector with the position and velocity data from the
#'   puzzle input.
#' @param df a dataframe of light positions and velocities created by
#' @param n number of time steps to advance
#' @return For Parts One and Two, `parse_light_points(x)` returns a dataframe
#'   with the starting position and velocity for each light point in the puzzle
#'   input. `step_light_points(df, n)` returns a dataframe with position of each
#'   light point after `n` steps.
#' @export
#' @examples
#' x <- "
#'   position=< 9,  1> velocity=< 0,  2>
#'   position=< 7,  0> velocity=<-1,  0>
#'   position=< 3, -2> velocity=<-1,  1>
#'   position=< 6, 10> velocity=<-2, -1>
#'   position=< 2, -4> velocity=< 2,  2>
#'   position=<-6, 10> velocity=< 2, -2>
#'   position=< 1,  8> velocity=< 1, -1>
#'   position=< 1,  7> velocity=< 1,  0>
#'   position=<-3, 11> velocity=< 1, -2>
#'   position=< 7,  6> velocity=<-1, -1>
#'   position=<-2,  3> velocity=< 1,  0>
#'   position=<-4,  3> velocity=< 2,  0>
#'   position=<10, -3> velocity=<-1,  1>
#'   position=< 5, 11> velocity=< 1, -2>
#'   position=< 4,  7> velocity=< 0, -1>
#'   position=< 8, -2> velocity=< 0,  1>
#'   position=<15,  0> velocity=<-2,  0>
#'   position=< 1,  6> velocity=< 1,  0>
#'   position=< 8,  9> velocity=< 0, -1>
#'   position=< 3,  3> velocity=<-1,  1>
#'   position=< 0,  5> velocity=< 0, -1>
#'   position=<-2,  2> velocity=< 2,  0.
#'   position=< 5, -2> velocity=< 1,  2>
#'   position=< 1,  4> velocity=< 2,  1>
#'   position=<-2,  7> velocity=< 2, -2>
#'   position=< 3,  6> velocity=<-1, -1>
#'   position=< 5,  0> velocity=< 1,  0>
#'   position=<-6,  0> velocity=< 2,  0>
#'   position=< 5,  9> velocity=< 1, -2>
#'   position=<14,  7> velocity=<-2,  0>
#'   position=<-3,  6> velocity=< 2, -1>
#'   "
#'
#' df <- x %>%
#'   read_text_lines() %>%
#'   parse_light_points()
#'
#' steps <- df %>%
#'   step_light_points(5)
#'
#' library(ggplot2)
#'
#' ggplot(steps) +
#'   aes(x = x, y = y) +
#'   geom_point() +
#'   facet_wrap("step")
step_light_points <- function(df, n) {
  # Basic idea: Run n steps by multiplying the velocities by n steps

  # Step 0 has the original locations.
  df_old <- df
  df <- keep_rows(df, .data$step == 0)

  # Create placeholder rows for the new steps
  df_new <- data.frame(
    text = rep(df$text, n),
    x = rep(df$x, n),
    y = rep(df$y, n),
    dx = rep(df$dx, n),
    dy = rep(df$dy, n),
    id = rep(df$id, n),
    step = rep(seq_len(n), each = nrow(df))
  )

  # Add max step to the new step counts so that this function
  # can add more steps onto an existing dataframe
  df_new$step <- df_new$step + max(df_old$step)
  df_new$x <- df_new$x + df_new$step * df_new$dx
  df_new$y <- df_new$y + df_new$step * df_new$dy
  rbind(df_old, df_new)
}

#' @rdname day10
#' @export
parse_light_points <- function(x) {
  # \\s* is 0 or more spaces
  # (-?\\d+) is an optional minus sign plus 1 or more digits
  # . is my lazy way of matching < or >
  re1 <- "position=.\\s*(-?\\d+),\\s*(-?\\d+)."
  re2 <- "velocity=.\\s*(-?\\d+),\\s*(-?\\d+)."
  re <- paste0(re1, " ", re2)

  df <- x %>%
    stringr::str_match(re) %>%
    as.data.frame() %>%
    stats::setNames(c("text", "x", "y", "dx", "dy")) %>%
    utils::type.convert(as.is = TRUE)

  df$id <- seq_len(nrow(df))
  df$step <- 0
  df
}
