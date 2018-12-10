context("test-day08")

test_that("Extracting metadata values from license", {
  a <- "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  metadata <- parse_license(a)
  expect_equal(sum(metadata), 138)

  b <- "0 3 10 11 12"
  expect_equal(parse_license2(b), 33)

  d <- "0 1 99"
  expect_equal(parse_license2(d), 99)

  c <- "1 1 0 1 99 2"
  expect_equal(parse_license2(c), 0)

  expect_equal(parse_license2(a), 66)
})
