context("test-day03")

test_that("creating a matrix of fabric claims", {
  expected_claims <- c(
    0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1,
    0, 1, 1, 2, 2, 1, 1,
    0, 1, 1, 2, 2, 1, 1,
    0, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 1, 1, 1)
  expected_claims <- matrix(expected_claims, nrow = 7, byrow = TRUE)

  claim_input <- c(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2")
  claim_df <- parse_fabric_claims(claim_input)
  expect_equal(create_fabric_claim_matrix(claim_df), expected_claims)

  expect_equal(count_overlapping_fabric_claims(claim_input), 4)
})
