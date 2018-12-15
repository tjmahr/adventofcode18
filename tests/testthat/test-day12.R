context("test-day12")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
# initial state:
x <- "#..#.#..##......###...###"

rules <- read_text_lines("
  ...## => #
  ..#.. => #
  .#... => #
  .#.#. => #
  .#.## => #
  .##.. => #
  .#### => #
  #.#.# => #
  #.### => #
  ##.#. => #
  ##.## => #
  ###.. => #
  ###.# => #
  ####. => #")

rules <- stringr::str_split_fixed(rules, " => ", n = 2)
lookup <- setNames(rules[, 2], rules[, 1])


#       1         2         3
#       0         0         0         0
# 0: ...#..#.#..##......###...###...........
# 1: ...#...#....#.....#..#..#..#...........
# 2: ...##..##...##....#..#..#..##..........
# 3: ..#.#...#..#.#....#..#..#...#..........
# 4: ...#.#..#...#.#...#..#..##..##.........
# 5: ....#...##...#.#..#..#...#...#.........
# 6: ....##.#.#....#...#..##..##..##........
# 7: ...#..###.#...##..#...#...#...#........
# 8: ...#....##.#.#.#..##..##..##..##.......
# 9: ...##..#..#####....#...#...#...#.......
# 10: ..#.#..#...#.##....##..##..##..##......
# 11: ...#...##...#.#...#.#...#...#...#......
# 12: ...##.#.#....#.#...#.#..##..##..##.....
# 13: ..#..###.#....#.#...#....#...#...#.....
# 14: ..#....##.#....#.#..##...##..##..##....
# 15: ..##..#..#.#....#....#..#.#...#...#....
# 16: .#.#..#...#.#...##...#...#.#..##..##...
# 17: ..#...##...#.#.#.#...##...#....#...#...
# 18: ..##.#.#....#####.#.#.#...##...##..##..
# 19: .#..###.#..#.#.#######.#.#.#..#.#...#..
# 20: .#....##....#####...#######....#.#..##.
state <- list(pots = x, added = 0, start = 1)
lookup <- setNames(rules[, 2], rules[, 1])

apply_rules(state, lookup) %>% apply_rules(lookup) %>% apply_rules(lookup) %>% apply_rules(lookup)

apply_rules <- function(state, lookup) {
  pots <- state$pots
  added <- 0
  while (!stringr::str_detect(pots, "^[.]{5}")) {
    pots <- paste0(".", pots, collapse = "")
    added <- added + 1
  }

  start <- state$start + added
  while (start < 6) {
    pots <- paste0(".", pots, collapse = "")
    added <- added + 1
    start <- state$start + added
  }

  while (!stringr::str_detect(pots, "[.]{5}$")) {
    pots <- paste0(pots, ".", collapse = "")
  }

  start_item <-
  pots <- pots %>%
    stringr::str_sub(1:(nchar(pots) - 4), 5:nchar(pots))
  matches <- lookup[pots]

  pots <- ifelse(is.na(matches), ".", matches) %>%
    unname() %>%
    paste0(collapse = "")

  list(
    pots = pots,
    start = start - 2)
}
