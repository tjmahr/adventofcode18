context("test-day02")

test_that("box IDs can be checksummed", {
  x <- c("abcdef", "bababc", "abbcde", "abcccd",
         "aabcdd", "abcdee", "ababab")
  expect_equal(checksum_box_id(x), 12)
})

test_that("nearest neighboring box IDs can be found", {
  x <- c("abcde", "fghij", "klmno", "pqrst",
         "fguij", "axcye", "wvxyz")

  expect_equal(find_one_character_neighbors(x), "fgij")
})
