#' Create the files for an Advent of Code day
#'
#' @param day integer giving the day
#' @param open whether to open the created files. Defaults to `TRUE` in an
#'   interactive R session.
#' @return `NULL`
#'
#' @details
#'
#' Creates a file for writing the functions to solve the problem: `R/dayxx.R`.
#' The text of the challenge is downloaded and inserted into the roxygen block.
#' One caveat is that you will have to manually add the markdown text for Part
#' Two yourself.
#'
#' Also, creates a file for unit tests: `tests/testthat/test-dayxx.R`. This is
#' good place to test that the examples in the problem description work.
#'
#' Finally, creates a solution file: `inst/run-dayxx.R`. You should download
#' your personalized challenge input as `inst/inputxx.txt`. Your solution file
#' should read in this file and apply your functions to it. Once your solution
#' passes on the site, store it in `R/data-solutions.R`. Then the solution file
#' can load in your previous answer, rerun your solution, and check whether your
#' code no longer obtains the same solution.
#' @export
use_day <- function(day, open = interactive()) {
  url <- sprintf("https://adventofcode.com/2018/day/%s", day)

  data <- list(
    dd_number = sprintf("%02.f", day),
    url = url,
    title = NA,
    part_1 = NA
  )

  files <- get_day_files(1)
  test_name <- sprintf("day%s", data$dd_number)

  page <- xml2::read_html(url)
  article <- xml2::xml_find_first(page, "/html/body/main/article")

  title <- xml2::xml_find_first(page, "/html/body/main/article/h2")
  title <- unlist(xml2::as_list(title))
  title <- stringr::str_replace(title, "--- Day \\d+: ", "")
  title <- stringr::str_replace(title, " ---", "")
  data$title <- title

  temp <- tempfile(fileext = ".html")
  xml2::write_html(article, temp)
  z <- knitr::pandoc(temp, "markdown", encoding = "UTF-8")
  lines <- readr::read_lines(z)
  lines <- lines[-c(1, 2, 3)]
  lines <- paste0("#' ", lines, collapse = "\n")
  data$part_1 <- lines

  usethis::use_template(
    "day.R",
    save_as = files$main,
    package = "adventofcode18",
    data = data,
    open = open
  )

  usethis::use_template(
    "solution.R",
    save_as = files$solution,
    package = "adventofcode18",
    data = data,
    open = open
  )

  usethis::use_test(
    name = test_name
  )

  invisible(NULL)
}

remove_day <- function(day) {
  file.remove(unlist(get_day_files(day)))
}

get_day_files <- function(day) {
  dd_number <- sprintf("%02.f", day)
  list(
    main = sprintf("R/day%s.R", dd_number),
    solution = sprintf("inst/run-day%s.R", dd_number),
    test = sprintf("tests/testthat/test-day%s.R", dd_number)
  )
}
