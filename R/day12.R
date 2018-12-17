#' Day 12: Subterranean Sustainability
#'
#' [Subterranean Sustainability](https://adventofcode.com/2018/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' The year 518 is significantly more underground than your history books
#' implied. Either that, or you've arrived in a [vast cavern
#' network]{title="It's probably this one. Can never be too sure, though."}
#' under the North Pole.
#'
#' After exploring a little, you discover a long tunnel that contains a row
#' of small pots as far as you can see to your left and right. A few of
#' them contain plants - someone is trying to grow things in these
#' geothermally-heated caves.
#'
#' The pots are numbered, with `0` in front of you. To the left, the pots
#' are numbered `-1`, `-2`, `-3`, and so on; to the right, `1`, `2`,
#' `3`.... Your puzzle input contains a list of pots from `0` to the right
#' and whether they do (`#`) or do not (`.`) currently contain a plant, the
#' *initial state*. (No other pots currently contain plants.) For example,
#' an initial state of `#..##....` indicates that pots `0`, `3`, and `4`
#' currently contain plants.
#'
#' Your puzzle input also contains some notes you find on a nearby table:
#' someone has been trying to figure out how these plants *spread* to
#' nearby pots. Based on the notes, for each generation of plants, a given
#' pot has or does not have a plant based on whether that pot (and the two
#' pots on either side of it) had a plant in the last generation. These are
#' written as `LLCRR => N`, where `L` are pots to the left, `C` is the
#' current pot being considered, `R` are the pots to the right, and `N` is
#' whether the current pot will have a plant in the next generation. For
#' example:
#'
#' -   A note like `..#.. => .` means that a pot that contains a plant but
#'     with no plants within two pots of it will not have a plant in it
#'     during the next generation.
#' -   A note like `##.## => .` means that an empty pot with two plants on
#'     each side of it will remain empty in the next generation.
#' -   A note like `.##.# => #` means that a pot has a plant in a given
#'     generation if, in the previous generation, there were plants in that
#'     pot, the one immediately to the left, and the one two pots to the
#'     right, but not in the ones immediately to the right and two to the
#'     left.
#'
#' It's not clear what these plants are for, but you're sure it's
#' important, so you'd like to make sure the current configuration of
#' plants is sustainable by determining what will happen after *`20`
#' generations*.
#'
#' For example, given the following input:
#'
#'     initial state: #..#.#..##......###...###
#'
#'     ...## => #
#'     ..#.. => #
#'     .#... => #
#'     .#.#. => #
#'     .#.## => #
#'     .##.. => #
#'     .#### => #
#'     #.#.# => #
#'     #.### => #
#'     ##.#. => #
#'     ##.## => #
#'     ###.. => #
#'     ###.# => #
#'     ####. => #
#'
#' For brevity, in this example, only the combinations which do produce a
#' plant are listed. (Your input includes all possible combinations.) Then,
#' the next 20 generations will look like this:
#'
#'                      1         2         3
#'            0         0         0         0
#'      0: ...#..#.#..##......###...###...........
#'      1: ...#...#....#.....#..#..#..#...........
#'      2: ...##..##...##....#..#..#..##..........
#'      3: ..#.#...#..#.#....#..#..#...#..........
#'      4: ...#.#..#...#.#...#..#..##..##.........
#'      5: ....#...##...#.#..#..#...#...#.........
#'      6: ....##.#.#....#...#..##..##..##........
#'      7: ...#..###.#...##..#...#...#...#........
#'      8: ...#....##.#.#.#..##..##..##..##.......
#'      9: ...##..#..#####....#...#...#...#.......
#'     10: ..#.#..#...#.##....##..##..##..##......
#'     11: ...#...##...#.#...#.#...#...#...#......
#'     12: ...##.#.#....#.#...#.#..##..##..##.....
#'     13: ..#..###.#....#.#...#....#...#...#.....
#'     14: ..#....##.#....#.#..##...##..##..##....
#'     15: ..##..#..#.#....#....#..#.#...#...#....
#'     16: .#.#..#...#.#...##...#...#.#..##..##...
#'     17: ..#...##...#.#.#.#...##...#....#...#...
#'     18: ..##.#.#....#####.#.#.#...##...##..##..
#'     19: .#..###.#..#.#.#######.#.#.#..#.#...#..
#'     20: .#....##....#####...#######....#.#..##.
#'
#' The generation is shown along the left, where `0` is the initial state.
#' The pot numbers are shown along the top, where `0` labels the center
#' pot, negative-numbered pots extend to the left, and positive pots extend
#' toward the right. Remember, the initial state begins at pot `0`, which
#' is not the leftmost pot used in this example.
#'
#' After one generation, only seven plants remain. The one in pot `0`
#' matched the rule looking for `..#..`, the one in pot `4` matched the
#' rule looking for `.#.#.`, pot `9` matched `.##..`, and so on.
#'
#' In this example, after 20 generations, the pots shown as `#` contain
#' plants, the furthest left of which is pot `-2`, and the furthest right
#' of which is pot `34`. Adding up all the numbers of plant-containing pots
#' after the 20th generation produces `325`.
#'
#' *After `20` generations, what is the sum of the numbers of all pots
#' which contain a plant?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f12a(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a()
#' f12b()
f12a <- function(x) {

}

#' @rdname day12
#' @export
f12b <- function(x) {

}

#' @rdname day12
#' @export
run_plant_rules <- function(pots, lookup_rules, n) {
  pots <- if (stringr::str_detect(pots, "P|E")) {
    pots
  } else {
    pots %>% str_set_plant_substr()
  }
  lookup <- parse_plant_rules(lookup_rules)

  i <- 0
  while (i < n) {
    i <- i + 1
    pots <- apply_plant_rules(pots, lookup)
  }
  pots
}

#' @rdname day12
#' @export
which_pots_have_plants <- function(pots) {
  zero <- stringr::str_locate(pots, "P|E")[1, 1]
  results <- pots %>%
    str_unset_plant() %>%
    stringr::str_locate_all("#")

  hits <- pots %>%
    str_unset_plant() %>%
    stringr::str_locate_all("#")
  hits[[1]][,"start"] - zero
}

apply_plant_rules <- function(pots, lookup) {
  # Pad until five dots at start
  while (!stringr::str_detect(pots, "^[.]{5}")) {
    pots <- paste0(".", pots, collapse = "")
  }

  # ... and end
  while (!stringr::str_detect(pots, "[.]{5}$")) {
    pots <- paste0(pots, ".", collapse = "")
  }

  # Break into bins of 5 plants. These are keys are that used to look up
  # replacement rules
  pot_bins <- stringr::str_sub(
    pots,
    1:(nchar(pots) - 4),
    5:nchar(pots))

  dont_change <- which(substr(pot_bins, 3, 3) %in% c("P", "E"))

  # Remove sentinel from bins where sentinel is not in center
  pot_bins[-dont_change] <- str_unset_plant(pot_bins[-dont_change])

  # Apply replacement rules
  matches <- lookup[pot_bins]

  # Manually set sentinel to empty "E" if not found in rules
  matches[dont_change] <- ifelse(
    is.na(matches[dont_change]),
    "E",
    matches[dont_change])

  # Set pots to empty "." if not found in rules
  str_collapse(unname(ifelse(is.na(matches), ".", matches)))
}


str_pad_plant <- function(x, start = -3, end = 35) {
  parts <- x %>%
    stringr::str_split_fixed("P|E", 2) %>%
    as.vector()

  p1 <- parts[1] %>%
    str_reverse()

  to_add <- abs(start) - nchar(p1)
  if (to_add > 0) {
    p1 <- rep(".", to_add) %>%
      str_collapse() %>%
      str_collapse(p1)
  }

  parts[1] <- p1 %>%
    substr(1, abs(start)) %>%
    str_reverse()

  to_add <- end - nchar(parts[2])
  if (to_add > 0) {
    parts[2] <- rep(".", to_add) %>%
      str_collapse() %>%
      str_collapse(parts[2], .)
  }
  parts[2] <- parts[2] %>%
    substr(1, end)

  str_collapse(
    parts[1],
    stringr::str_extract(x, "P|E"),
    parts[2]
  )
}

# Assign a sentinel value for pot 0
str_set_plant_substr <- function(xs, start = 1L, end = 1L) {
  lookup <- c("#" = "P", "." = "E", "E" = "E", "P" = "P")
  substr(xs, start, end) <- lookup[substr(xs, start, end)]
  xs
}

parse_plant_rules <- function(rules) {
  rules <- rules %>%
    str_set_plant_substr(3, 3) %>%
    str_set_plant_substr(10, 10) %>%
    append(rules, .) %>%
    stringr::str_split_fixed(" => ", n = 2)
  setNames(rules[, 2], rules[, 1])
}


# Assign a sentinel value for pot 0
str_unset_plant <- function(xs) {
    stringr::str_replace(
      stringr::str_replace(xs, "P", "#"),
      "E",
      ".")
}

str_reverse <- function(x) {
  x %>% str_tokenize() %>% rev() %>% str_collapse()
}

str_collapse <- function(...) {
  paste0(..., collapse = "")
}
