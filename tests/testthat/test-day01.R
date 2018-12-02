context("test-day01")

test_that("summing frequencies", {
  expect_equal(sum_frequency(c(+1, -2, +3, +1)), 3)
  expect_equal(sum_frequency(c(+1, +1, +1)),  3)
  expect_equal(sum_frequency(c(+1, +1, -2)),  0)
  expect_equal(sum_frequency(c(-1, -2, -3)), -6)
})
