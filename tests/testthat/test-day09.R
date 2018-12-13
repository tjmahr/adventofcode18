context("test-day09")

test_that("finding high score of marble game", {
  run_marbles(7, 25) %>%
    get_marble_high_score() %>%
    expect_equal(32)

  "10 players; last marble is worth 1618 points: high score is 8317" %>%
    parse_marble_description() %>%
    do.call(run_marbles, .) %>%
    get_marble_high_score() %>%
    expect_equal(8317)

  "13 players; last marble is worth 7999 points: high score is 146373" %>%
    parse_marble_description() %>%
    do.call(run_marbles, .) %>%
    get_marble_high_score() %>%
    expect_equal(146373)

  "17 players; last marble is worth 1104 points: high score is 2764" %>%
    parse_marble_description() %>%
    do.call(run_marbles, .) %>%
    get_marble_high_score() %>%
    expect_equal(2764)

  "21 players; last marble is worth 6111 points: high score is 54718" %>%
    parse_marble_description() %>%
    do.call(run_marbles, .) %>%
    get_marble_high_score() %>%
    expect_equal(54718)

  "30 players; last marble is worth 5807 points: high score is 37305" %>%
    parse_marble_description() %>%
    do.call(run_marbles, .) %>%
    get_marble_high_score() %>%
    expect_equal(37305)
})


test_that("wrapping indices around a circular vector", {
  wrap_around2(1:6, 1:6) %>%
    expect_equal(1:6)

  wrap_around2(7:12, 1:6) %>%
    expect_equal(1:6)

  wrap_around2(0, 1:6) %>%
    expect_equal(1)

  wrap_around2(-1:-6, 1:6) %>%
    expect_equal(6:1)

  new_pos <- c(3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4)

  wrap_around2(-10:10, 1:6) %>%
    expect_equal(new_pos)
})
