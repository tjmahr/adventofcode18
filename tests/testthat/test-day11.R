context("test-day11")

test_that("Computing a cell's power level", {
  compute_cell_power_level(3, 5, 8) %>%
    expect_equal(4)

  compute_cell_power_level(122, 79, 57) %>%
    expect_equal(-5)

  compute_cell_power_level(217, 196, 39) %>%
    expect_equal(0)

  compute_cell_power_level(101, 153, 71) %>%
    expect_equal(4)
})

test_that("Finding best 3x3 pool in a power grid", {
  # "For grid serial number 18, the largest total 3x3 square has a top-left
  # corner of 33,45 (with a total power of 29); these fuel cells appear in the
  # middle of this 5x5 region"
  test_sub <- c(
    -2, -4,  4,  4,  4,
    -4,  4,  4,  4, -5,
    4,  3,  3,  4, -4,
    1,  1,  2,  4, -3,
    -1,  0,  2, -5, -2)
  test_sub <- matrix(test_sub, ncol = 5, byrow = TRUE)

  m <- create_power_cell_matrix(300, 18)
  m_sub <- m[0:4 + 45 - 1, 0:4 + 33 - 1]
  expect_equal(m_sub, test_sub)

  peak_corner <- pool_power_cells(m, 3)
  expect_equal(peak_corner$x, 33)
  expect_equal(peak_corner$y, 45)
  expect_equal(peak_corner$power, 29)

  # "For grid serial number 42, the largest 3x3 square's top-left is 21,61 (with
  # a total power of 30); they are in the middle of this region:
  test_sub <- c(
    -3,  4,  2,  2,  2,
    -4,  4,  3,  3,  4,
    -5,  3,  3,  4, -4,
    4,  3,  3,  4, -3,
    3,  3,  3, -5, -1)
  test_sub <- matrix(test_sub, ncol = 5, byrow = TRUE)

  m <- create_power_cell_matrix(300, 42)
  m_sub <- m[0:4 + 61 - 1, 0:4 + 21 - 1]
  expect_equal(m_sub, test_sub)

  peak_corner <- pool_power_cells(m, 3)
  expect_equal(peak_corner$x, 21)
  expect_equal(peak_corner$y, 61)
  expect_equal(peak_corner$power, 30)
})


test_that("Finding best pool size for a power grid", {
  # "For grid serial number 18, the largest total square (with a total power of
  # 113) is 16x16 and has a top-left corner of 90,269, so its identifier is
  # 90,269,16."
  m <- create_power_cell_matrix(300, 18)
  results <- search_power_cell_pools(m, to_search = 14:20)
  best_row <- results[which.max(results$power), ]
  expect_equal(
    paste(best_row$x, best_row$y, best_row$span, sep = ","),
    "90,269,16"
  )

  # "For grid serial number 42, the largest total square (with a total power of
  # 119) is 12x12 and has a top-left corner of 232,251, so its identifier is
  # 232,251,12."
  m <- create_power_cell_matrix(300, 42)
  results <- search_power_cell_pools(m, to_search = 10:14)
  best_row <- results[which.max(results$power), ]
  expect_equal(
    paste(best_row$x, best_row$y, best_row$span, sep = ","),
    "232,251,12"
  )
})
