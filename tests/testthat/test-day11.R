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

  m <- create_power_cell_matrix(300, 18)

  test_sub <- c(
    -2, -4, 4,  4,  4,
    -4,  4, 4,  4, -5,
     4,  3, 3,  4, -4,
     1,  1, 2,  4, -3,
    -1,  0, 2, -5, -2)
  test_sub <- matrix(test_sub, ncol = 5, byrow = TRUE)

  m_sub <- m[0:4 + 45 - 1, 0:4 + 33 - 1]
  expect_equal(m_sub, test_sub)

})

#
#
# compute_cell_power_level(1:5, rep(1, 5), 18)
#
# d <- expand.grid(x = 1:5, y = 1:5)
#
# m <- matrix(nrow = 5, ncol = 5)
# span <- 300
# m[1, 1:5] <- compute_cell_power_level(1:5, 1, 18)
# m
#
# # xs <- rep(1:span, span)
# # ys <- rep(1:span, each = span)
#

#
#
# m <- matrix(nrow = 300, ncol = 300)
# for (x in seq_len(span)) {
#   for (y in seq_len(span)) {
#     m[y, x] <- paste0(x, ",", y)
#   }
# }
# # For grid serial number 18, the largest total 3x3 square has a top-left corner of 33,45 (
#
# m2 <- paste0(xs, ",", ys) %>%
#   matrix(ncol = span)
#
#
# head(m)
#
#
#
#
# m2[0:4 + 45, 0:4 + 33]
#
# compute_cell_power_level(xs, ys, 18)
#
# For grid serial number 18, the largest total 3x3 square has a top-left corner of 33,45 (with a total power of 29); these fuel cells appear in the middle of this 5x5 region:
# x <- 0:4 + 33
# y <- 0 + 5
# rack_id <- x + 10
# power_level <- (rack_id * y + serial_number) * rack_id
# hundredth <- (power_level %% 1000) %/% 100
# hundredth - 5
#
#
#
# -2  -4   4   4   4
# -4   4   4   4  -5
# 4   3   3   4  -4
# 1   1   2   4  -3
# -1   0   2  -5  -2
#
#
# apply()
# -2  -4   4   4   4
# -4   4   4   4  -5
# 4   3   3   4  -4
# 1   1   2   4  -3
# -1   0   2  -5  -2