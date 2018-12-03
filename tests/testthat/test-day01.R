context("test-day01")

test_that("summing frequencies", {
  expect_equal(sum_frequency(c(+1, -2, +3, +1)), 3)
  expect_equal(sum_frequency(c(+1, +1, +1)),  3)
  expect_equal(sum_frequency(c(+1, +1, -2)),  0)
  expect_equal(sum_frequency(c(-1, -2, -3)), -6)
})

test_that("finding first repeated sum in a stream", {
  expect_equal(analyze_frequency_stream(c(+1, -1)), 0)
  expect_equal(analyze_frequency_stream(c(+3, +3, +4, -2, -4)),  10)
  expect_equal(analyze_frequency_stream(c(-6, +3, +8, +5, -6)),  5)
  expect_equal(analyze_frequency_stream(c(+7, +7, -2, -7, -4)), 14)
})
