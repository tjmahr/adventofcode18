# techniques: split-apply-combine

#' Day 03: No Matter How You Slice It
#'
#' [No Matter How You Slice It](https://adventofcode.com/2018/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' The Elves managed to locate the chimney-squeeze prototype fabric for
#' Santa's suit (thanks to [someone]{title="WAS IT YOU"} who helpfully
#' wrote its box IDs on the wall of the warehouse in the middle of the
#' night). Unfortunately, anomalies are still affecting them - nobody can
#' even agree on how to *cut* the fabric.
#'
#' The whole piece of fabric they're working on is a very large square - at
#' least `1000` inches on each side.
#'
#' Each Elf has made a *claim* about which area of fabric would be ideal
#' for Santa's suit. All claims have an ID and consist of a single
#' rectangle with edges parallel to the edges of the fabric. Each claim's
#' rectangle is defined as follows:
#'
#' -   The number of inches between the left edge of the fabric and the
#'     left edge of the rectangle.
#' -   The number of inches between the top edge of the fabric and the top
#'     edge of the rectangle.
#' -   The width of the rectangle in inches.
#' -   The height of the rectangle in inches.
#'
#' A claim like `#123 @ 3,2: 5x4` means that claim ID `123` specifies a
#' rectangle `3` inches from the left edge, `2` inches from the top edge,
#' `5` inches wide, and `4` inches tall. Visually, it claims the square
#' inches of fabric represented by `#` (and ignores the square inches of
#' fabric represented by `.`) in the diagram below:
#'
#'     ...........
#'     ...........
#'     ...#####...
#'     ...#####...
#'     ...#####...
#'     ...#####...
#'     ...........
#'     ...........
#'     ...........
#'
#' The problem is that many of the claims *overlap*, causing two or more
#' claims to cover part of the same areas. For example, consider the
#' following claims:
#'
#'     #1 @ 1,3: 4x4
#'     #2 @ 3,1: 4x4
#'     #3 @ 5,5: 2x2
#'
#' Visually, these claim the following areas:
#'
#'     ........
#'     ...2222.
#'     ...2222.
#'     .11XX22.
#'     .11XX22.
#'     .111133.
#'     .111133.
#'     ........
#'
#' The four square inches marked with `X` are claimed by *both `1` and
#' `2`*. (Claim `3`, while adjacent to the others, does not overlap either
#' of them.)
#'
#' If the Elves all proceed with their own plans, none of them will have
#' enough fabric. *How many square inches of fabric are within two or more
#' claims?*
#'
#' **Part Two**
#'
#' Amidst the chaos, you notice that exactly one claim doesn't overlap by
#' even a single square inch of fabric with any other claim. If you can
#' somehow draw attention to it, maybe the Elves will be able to make
#' Santa's suit after all!
#'
#' For example, in the claims above, only claim `3` is intact after all
#' claims are made.
#'
#' *What is the ID of the only claim that doesn't overlap?*
#'
#' @param x a character vector of fabric claims
#' @return For Part One, `count_overlapping_fabric_claims(x)` returns the total
#'   number of squares that are claimed by more than one fabric claim. For Part
#'   Two, `find_unique_fabric_claim(x)` returns the ID of the claim that does
#'   not overlap any others.
#' @export
#' @examples
#' x <- c(
#'   "#1 @ 1,3: 4x4",
#'   "#2 @ 3,1: 4x4",
#'   "#3 @ 5,5: 2x2")
#' count_overlapping_fabric_claims(x)
#' find_unique_fabric_claim(x)
count_overlapping_fabric_claims <- function(x) {
  claims <- x %>%
    parse_fabric_claims() %>%
    create_fabric_claim_matrix()
  sum(claims > 1)
}

# dataframe of parsed fabric claims => matrix with claim counts on each cell
create_fabric_claim_matrix <- function(df) {
  # initialize a matrix. Not hard-coding the size from problem description so
  # that code can handle the small example in description.
  max_width  <- max(df$from_left + df$width)
  max_height <- max(df$from_top  + df$height)
  m <- matrix(0, nrow = max_height, ncol = max_width)

  claim_list <- df %>%
    split(seq_along_rows(df)) %>%
    purrr::map(as.list)

  for (claim in claim_list) {
    m <- count_fabric_claim(m, claim)
  }

  m
}

#' @importFrom stats setNames
#' @importFrom utils type.convert
parse_fabric_claims <- function(x) {
  # claims like "#1 @ 1,3: 4x4" => a dataframe with columns for each part
  df <- x %>%
    stringr::str_match("(#\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)") %>%
    tibble::as_tibble() %>%
    setNames(
      c("line", "id", "from_left", "from_top", "width", "height")) %>%
    type.convert()
  df$line <- as.character(df$line)
  df$id <- df$id %>%
    stringr::str_remove("#") %>%
    as.numeric()
  df
}

# Add values from a fabric claim to a matrix
count_fabric_claim <- function(m, claim) {
  # adding 1 to `from_` values here because
  # - 1 row from top means row 2
  # - 0 rows from top means row 1
  rows <- seq(from = 1 + claim$from_top,  length.out = claim$height)
  cols <- seq(from = 1 + claim$from_left, length.out = claim$width)
  m[rows, cols] <- m[rows, cols] + 1
  m
}

seq_along_rows <- function(data) {
  seq_len(nrow(data))
}


#' @rdname day03
#' @export
find_unique_fabric_claim <- function(x) {
  df_claims <- parse_fabric_claims(x)

  # Take one pass over the claims to find the overlapping claims
  claim_matrix <- create_fabric_claim_matrix(df_claims)

  # Take another pass to find whether any squares in a claim overlap with
  # another claim
  unique_claims <- check_fabric_claims(df_claims, claim_matrix)

  as.numeric(unique_claims$id)
}

# (dataframe of parsed fabric claims, matrix of claim counts)
#   => rows of dataframe for claims that don't overlap any others
check_fabric_claims <- function(df, m) {
  claim_list <- df %>%
    split(seq_along_rows(df)) %>%
    purrr::map(as.list)

  # Update this vector with IDs of claims whose cells in the main matrix
  # are all 1's
  passes <- character(0)

  for (claim in claim_list) {
    rows <- seq(from = 1 + claim$from_top,  length.out = claim$height)
    cols <- seq(from = 1 + claim$from_left, length.out = claim$width)

    if (all(m[rows, cols] == 1)) {
      passes <- c(passes, claim$id)
    }
  }

  df[df$id %in% passes, ]
}
