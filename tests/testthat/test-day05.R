context("test-day05")

test_that("can identify reactive characters", {
  expect_false(will_react("a", "b"))
  expect_true(will_react("A", "a"))
  expect_true(will_react("a", "A"))
})

test_that("can run reactions", {
  "aA" %>%
    run_polymer_reaction() %>%
    expect_equal("")

  "abBA" %>%
    run_polymer_reaction() %>%
    expect_equal("")

  "abAB" %>%
    run_polymer_reaction() %>%
    expect_equal("abAB")

  "aabAAB" %>%
    run_polymer_reaction() %>%
    expect_equal("aabAAB")

  "dabAcCaCBAcCcaDA" %>%
    run_polymer_reaction() %>%
    expect_equal("dabCBAcaDA")
})

test_that("can simulate reactions after removing each unit type", {
  x <- "dabAcCaCBAcCcaDA"
  expected <- structure(list(6, 8, 4, 6), names = c("a", "b", "c", "d"))

  expect_equal(
    simulate_polymer_reactions(x),
    expected
  )
})
