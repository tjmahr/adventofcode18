context("test-day06")

test_that("finding finites areas near beacons (input points)", {
  lines <- "
  1, 1
  1, 6
  8, 3
  3, 4
  5, 5
  8, 9"

  df_beacons <- lines %>%
    read_text_lines() %>%
    read_beacon_coordinates()

  # Match labels of description
  df_beacons$label <- LETTERS[as.integer(df_beacons$label)]

  grid <- search_grid_around_beacons(df_beacons)
  areas <- measure_finite_beacon_areas(grid)

  expect_equal(areas$D, 9)
  expect_equal(areas$E, 17)
})

test_that("finding safe areas near beacons (input points)", {
  lines <- "
  1, 1
  1, 6
  8, 3
  3, 4
  5, 5
  8, 9"

  df_beacons <- lines %>%
    read_text_lines() %>%
    read_beacon_coordinates()

  safe_grid <- search_grid_for_safe_regions(df_beacons, 32)
  areas <- measure_safe_area(safe_grid)

  expect_equal(areas$`#`, 16)
})

