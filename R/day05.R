# objects

#' Day 05: Alchemical Reduction
#'
#' [Alchemical Reduction](https://adventofcode.com/2018/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' You've managed to sneak in to the prototype suit manufacturing lab. The
#' Elves are making decent progress, but are still struggling with the
#' suit's size reduction capabilities.
#'
#' While the very latest in 1518 alchemical technology might have solved
#' their problem eventually, you can do better. You scan the chemical
#' composition of the suit's material and discover that it is formed by
#' extremely long [polymers](https://en.wikipedia.org/wiki/Polymer) (one of
#' which is [available]{title="I've always wanted a polymer!"} as your
#' puzzle input).
#'
#' The polymer is formed by smaller *units* which, when triggered, react
#' with each other such that two adjacent units of the same type and
#' opposite polarity are destroyed. Units' types are represented by
#' letters; units' polarity is represented by capitalization. For instance,
#' `r` and `R` are units with the same type but opposite polarity, whereas
#' `r` and `s` are entirely different types and do not react.
#'
#' For example:
#'
#' -   In `aA`, `a` and `A` react, leaving nothing behind.
#' -   In `abBA`, `bB` destroys itself, leaving `aA`. As above, this then
#'     destroys itself, leaving nothing.
#' -   In `abAB`, no two adjacent units are of the same type, and so
#'     nothing happens.
#' -   In `aabAAB`, even though `aa` and `AA` are of the same type, their
#'     polarities match, and so nothing happens.
#'
#' Now, consider a larger example, `dabAcCaCBAcCcaDA`:
#'
#'     dabAcCaCBAcCcaDA  The first 'cC' is removed.
#'     dabAaCBAcCcaDA    This creates 'Aa', which is removed.
#'     dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
#'     dabCBAcaDA        No further actions can be taken.
#'
#' After all possible reactions, the resulting polymer contains *10 units*.
#'
#' *How many units remain after fully reacting the polymer you scanned?*
#' [(Note: in this puzzle and others, the input is large; if you copy/paste
#' your input, make sure you get the whole thing.)]{.quiet}
#'
#' **Part Two**
#'
#' Time to improve the polymer.
#'
#' One of the unit types is causing problems; it\'s preventing the polymer
#' from collapsing as much as it should. Your goal is to figure out which
#' unit type is causing the most problems, remove all instances of it
#' (regardless of polarity), fully react the remaining polymer, and measure
#' its length.
#'
#' For example, again using the polymer `dabAcCaCBAcCcaDA` from above:
#'
#' -   Removing all `A`/`a` units produces `dbcCCBcCcD`. Fully reacting
#'     this polymer produces `dbCBcD`, which has length 6.
#' -   Removing all `B`/`b` units produces `daAcCaCAcCcaDA`. Fully reacting
#'     this polymer produces `daCAcaDA`, which has length 8.
#' -   Removing all `C`/`c` units produces `dabAaBAaDA`. Fully reacting
#'     this polymer produces `daDA`, which has length 4.
#' -   Removing all `D`/`d` units produces `abAcCaCBAcCcaA`. Fully reacting
#'     this polymer produces `abCBAc`, which has length 6.
#'
#' In this example, removing all `C`/`c` units was best, producing the
#' answer *4*.
#'
#' *What is the length of the shortest polymer you can produce* by removing
#' all units of exactly one type and fully reacting the result?
#'
#' @param x a string describing a polymer
#' @return For Part One, `create_polymer(x)` returns a polymer object that can
#'   run a reaction. `run_polymer_reaction(x)` returns the polymer after all
#'   reactions are finished. For Part Two, `simulate_polymer_reactions(x)`
#'   returns a list with the length of the polymer after removing each unit and
#'   letting the resulting polymer react.
#' @export
#' @examples
#' p <- create_polymer("dabAcCaCBAcCcaDA")
#' p$run_reaction()
#' # or
#' run_polymer_reaction("dabAcCaCBAcCcaDA")
#' run_polymer_reaction("dabAcCaCBAcCcaDA", verbose = TRUE)
#'
#' simulate_polymer_reactions(x)
create_polymer <- function(x) {
  chars <- str_tokenize(x)
  left <- 1
  right <- 2

  # helpful in debugging
  print_polymer <- function() {
    l <- left
    r <- right

    if (has_no_chars()) {
      chars_print <- c("", "")
      l <- 1
      r <- 2
    } else if (is_done()) {
      chars_print <- c(chars, "")
    } else {
      chars_print <- chars
    }

    chars_print[l] <- paste0("(", chars_print[l], ")")
    chars_print[r] <- paste0("(", chars_print[r], ")")
    cat(paste0(chars_print, collapse = ""), "\n")
    invisible(chars)
  }

  is_done <- function() {
    length(chars) == 0 || length(chars) < right
  }

  has_no_chars <- function() {
    length(chars) == 0
  }

  step <- function() {
    if (will_react(chars[left], chars[right])) {
      chars <<- chars[-c(left, right)]
      left <<- left - 1
      right <<- right - 1

    } else {
      left <<- left + 1
      right <<- right + 1
    }

    if (left == 0) {
      left <<- 1
      right <<- 2
    }
  }

  run_reaction <- function(verbose = FALSE) {
    while(!is_done()) {
      step()
      if (verbose) print_polymer()
    }
    paste0(chars, collapse = "")
  }

  list(
    run_reaction = run_reaction)
}

#' @rdname day05
#' @param verbose whether to print out the each step of the polymer simulation
#' @export
run_polymer_reaction <- function(x, verbose = FALSE) {
  p <- create_polymer(x)
  p$run_reaction(verbose)
}

will_react <- function(x, y) {
  x != y && tolower(x) == tolower(y)
}

#' @rdname day05
#' @export
simulate_polymer_reactions <- function(x) {
  # we have to check only and all units that appear in the polymer
  units_to_check <- x %>%
    tolower() %>%
    str_tokenize() %>%
    unique() %>%
    sort()

  # run a reaction on polymer after removing a unit
  simulate_one_reaction <- function(unit_to_remove) {
    pattern <- paste0(unit_to_remove, "|", toupper(unit_to_remove))
    x %>%
      stringr::str_remove_all(pattern) %>%
      run_polymer_reaction() %>%
      nchar()
  }

  units_to_check %>%
    lapply(simulate_one_reaction) %>%
    setNames(c(units_to_check))
}
