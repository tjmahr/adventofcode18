context("test-day07")

test_that("multiplication works", {
  lines <- "
  Step C must be finished before step A can begin.
  Step C must be finished before step F can begin.
  Step A must be finished before step B can begin.
  Step A must be finished before step D can begin.
  Step B must be finished before step E can begin.
  Step D must be finished before step E can begin.
  Step F must be finished before step E can begin."

  x <- read_text_lines(lines)
  df <- read_sleigh_instructions(x)
  steps <- sort_sleigh_steps(df)
  expect_equal(paste0(steps, collapse = ""), "CABDFE")
})
