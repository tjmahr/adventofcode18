context("test-day09")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
input <- c("9 players; last marble is worth 25 points")
players <- 9
marbles <- 25
players <- 17
marbles <- 1104

run_marbles <- function(marbles, players) {
  start <- c(0)
  current_position <- 1
  history <- data.frame(player = numeric(0), turn = numeric(0), score = numeric(0))

  for (i in 1:marbles) {
    seed <- i
    if (i %% 23 == 0) {
      to_remove <- wrap_around2(current_position - 7, start)
      bonus <- start[to_remove]
      start <- start[-to_remove]
      position <- wrap_around2(current_position - 7, start)

      row <- list(
        player = ifelse(i %% players == 0, players, i %% players),
        turn = i,
        score = i + bonus)
      history <- tibble::add_row(history, !!! row)
    } else {
      position <- wrap_around2(current_position + 2, start)
      start <- insert_value(start, position, seed)
    }
    current_position <- position
    p_start <- as.character(start)
    p_start[current_position] <- paste0("(", p_start[current_position], ")")
    # print(p_start)
  }
  history
}


history <- run_marbles(25, 7)
history
aggregate2(history, score ~ player, sum)

wrap_around <- function(xs, length) {
  ((xs - 1) %% length) + 1
}

wrap_around2 <- function(xs, y) {
  ((xs - 1) %% length(y)) + 1
}

insert_value <- function(vector, position, value) {
  start <- utils::head(vector, position - 1)
  if (position == 1) {
    rest <- vector
  } else {
    rest <- utils::tail(vector, -position + 1)
  }
  c(start, value, rest)
}
