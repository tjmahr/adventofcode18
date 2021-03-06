# book-keeping of indices
# then i improved performance by pre-allocating vector space
# and removing helper functions like head() or tail() to eke out more speed
# then I rewrote the main function in Rcpp to get even more speed. It still took
# 10 hours.

#' Day 09: Marble Mania
#'
#' [Marble Mania](https://adventofcode.com/2018/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' You talk to the Elves while you wait for your navigation system to
#' [initialize]{title="Do you have any idea how long it takes to load navigation data for all of time and space?!"}.
#' To pass the time, they introduce you to their favorite
#' [marble](https://en.wikipedia.org/wiki/Marble_(toy)) game.
#'
#' The Elves play this game by taking turns arranging the marbles in a
#' *circle* according to very particular rules. The marbles are numbered
#' starting with `0` and increasing by `1` until every marble has a number.
#'
#' First, the marble numbered `0` is placed in the circle. At this point,
#' while it contains only a single marble, it is still a circle: the marble
#' is both clockwise from itself and counter-clockwise from itself. This
#' marble is designated the *current marble*.
#'
#' Then, each Elf takes a turn placing the *lowest-numbered remaining
#' marble* into the circle between the marbles that are `1` and `2` marbles
#' *clockwise* of the current marble. (When the circle is large enough,
#' this means that there is one marble between the marble that was just
#' placed and the current marble.) The marble that was just placed then
#' becomes the *current marble*.
#'
#' However, if the marble that is about to be placed has a number which is
#' a multiple of `23`, *something entirely different happens*. First, the
#' current player keeps the marble they would have placed, adding it to
#' their *score*. In addition, the marble `7` marbles *counter-clockwise*
#' from the current marble is *removed* from the circle and *also* added to
#' the current player\'s score. The marble located immediately *clockwise*
#' of the marble that was removed becomes the new *current marble*.
#'
#' For example, suppose there are 9 players. After the marble with value
#' `0` is placed in the middle, each player (shown in square brackets)
#' takes a turn. The result of each of those turns would produce circles of
#' marbles like this, where clockwise is to the right and the resulting
#' current marble is in parentheses:
#'
#'     [-] (0)
#'     [1]  0 (1)
#'     [2]  0 (2) 1
#'     [3]  0  2  1 (3)
#'     [4]  0 (4) 2  1  3
#'     [5]  0  4  2 (5) 1  3
#'     [6]  0  4  2  5  1 (6) 3
#'     [7]  0  4  2  5  1  6  3 (7)
#'     [8]  0 (8) 4  2  5  1  6  3  7
#'     [9]  0  8  4 (9) 2  5  1  6  3  7
#'     [1]  0  8  4  9  2(10) 5  1  6  3  7
#'     [2]  0  8  4  9  2 10  5(11) 1  6  3  7
#'     [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
#'     [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
#'     [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
#'     [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
#'     [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
#'     [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
#'     [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
#'     [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
#'     [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
#'     [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
#'     [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
#'     [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
#'     [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
#'     [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15
#'
#' The goal is to be the *player with the highest score* after the last
#' marble is used up. Assuming the example above ends after the marble
#' numbered `25`, the winning score is `23+9=32` (because player 5 kept
#' marble `23` and removed marble `9`, while no other player got any points
#' in this very short example game).
#'
#' Here are a few more examples:
#'
#' -   `10` players; last marble is worth `1618` points: high score is
#'     *`8317`*
#' -   `13` players; last marble is worth `7999` points: high score is
#'     *`146373`*
#' -   `17` players; last marble is worth `1104` points: high score is
#'     *`2764`*
#' -   `21` players; last marble is worth `6111` points: high score is
#'     *`54718`*
#' -   `30` players; last marble is worth `5807` points: high score is
#'     *`37305`*
#'
#' *What is the winning Elf\'s score?*
#'
#' **Part Two**
#'
#' Amused by the speed of your answer, the Elves are curious:
#'
#' *What would the new winning Elf\'s score be if the number of the last
#' marble were 100 times larger?*
#'
#' @param x sentence like the puzzle input descriping the number of players and
#'   value of the last marble
#' @param players,marbles the numbers of players and marbles, as returned by
#'   `parse_marble_description(x)`
#' @param marble_history a dataframe as returned by
#'   `run_marbles(players, marbles)` with the history of scoring turns in the
#'   marble game.
#' @return For Parts One and Two, `parse_marble_description(x)` returns a list
#'   with an element `players` for the number of players and an element
#'   `marbles` with the number of marbles that need to be played.
#'   `run_marbles(players, marbles)` simulates the game and return a dataframe
#'   with one row per scoring turn. `run_marbles_c(players, marbles)` is an
#'   alternative version written in C++ for speed.
#'   `get_marble_high_score(marble_history)` returns the score of the highest
#'   scoring player in a game. The helper function `wrap_around2(xs, y)` handles
#'   the details of wrapping positions `xs` around a circular vector `y`.
#' @export
#' @examples
#' "10 players; last marble is worth 1618 points: high score is 8317" %>%
#'   parse_marble_description() %>%
#'   do.call(run_marbles, .) %>%
#'   get_marble_high_score()
#'
#' # 0 is position 1
#' # 7 is first position (wrapped around 1 time)
#' # -1 is the last element (wrapped around backwards)
#' setNames(wrap_around2(-10:10, 1:6), -10:10)
get_marble_high_score <- function(marble_history) {
  marble_history %>%
    aggregate2(score ~ player, sum) %>%
    keep_rows(.data$score == max(.data$score)) %>%
    getElement("score")
}

#' @rdname day09
#' @export
run_marbles <- function(players, marbles) {
  start <- c(0L)
  current_position <- 1L
  nrows <- marbles %/% 23L

  history <- data.frame(
    player = integer(nrows),
    turn = integer(nrows),
    bonus = integer(nrows),
    score = integer(nrows),
    high_score = integer(nrows))

  for (i in seq.int(1L, marbles)) {
    if (i %% 10000L == 0L) message("step: ", i)
    if (i %% 23L == 0L) {
      to_remove <- wrap_around2(current_position - 7L, start)
      bonus <- start[to_remove]
      start <- start[-to_remove]
      position <- wrap_around2(current_position - 7L, start)

      row <- i %/% 23L
      history[row, "player"] <- ifelse(
        i %% players == 0L,
        players,
        i %% players)
      history[row, "turn"] <- i
      history[row, "bonus"] <- bonus
      history[row, "score"] <- i + bonus
      history[row, "high_score"] <- get_marble_high_score(history)
    } else {
      position <- wrap_around2(current_position + 2, start)
      start <- insert_value(start, position, i)
    }
    current_position <- position
  }
  history
}

#' @rdname day09
#' @export
run_marbles_c <- function(players, marbles) {
  nrows <- marbles %/% 23L
  turns <- seq_len(nrows) * 23

  history <- data.frame(
    player = ifelse(turns %% players == 0L, players, turns %% players),
    turn = turns,
    bonus = integer(nrows),
    score = integer(nrows))


  history$score <- run_marbles_c_scores(players, marbles)
  history$bonus <- history$score - history$turn

  history <- history %>%
    split(.$player) %>%
    purrr::map_dfr(
      function(x) {
        x$high_score = cumsum(x$score)
        x}
    )

  history[order(history$turn), ]

}

#' @rdname day09
#' @export
parse_marble_description <- function(x) {
  players <- x %>%
    stringr::str_extract("\\d+ players") %>%
    stringr::str_remove(" players") %>%
    as.integer()
  marbles <- x %>%
    stringr::str_extract("\\d+ points") %>%
    stringr::str_remove(" points") %>%
    as.integer()
  list(players = players, marbles = marbles)
}

#' @rdname day09
#' @export
#' @param xs positions in a circular vector
#' @param y a circular vector
wrap_around2 <- function(xs, y) {
  # treat position 0 as position 1
  # negative ones should count from end
  xs <- ifelse(xs <= 0, xs + 1, xs)
  ((xs - 1) %% length(y)) + 1
}

insert_value <- function(vector, position, value) {
  if (position == 1) {
    c(value, vector)
  } else {
    start <- vector[seq.int(1, position - 1)]
    rest <- vector[seq.int(position, length(vector))]
    c(start, value, rest)
  }
}

