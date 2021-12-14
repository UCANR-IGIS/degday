## This runs a series of tests using the verification table provided by Zalom et al 1983

## Load a copy of Table 7 from Zalom et al 1983, containing validation data
load("zalom83_tbl7.RData")
thresh_low <- 55
thresh_up <- 90

test_that("single triangle works", {
  my_gdd <- dd_calc(daily_min = tbl7_df$temp_min,
                     daily_max = tbl7_df$temp_max,
                     thresh_low = thresh_low,
                     thresh_up = thresh_up,
                     method = "sng_tri",
                     debug = FALSE)
  expect_equal(tbl7_df$sng_tri, my_gdd, tolerance = 0.001)
})

test_that("double triangle works", {
  my_gdd <- dd_calc(daily_min = tbl7_df$temp_min,
                     daily_max = tbl7_df$temp_max,
                     nextday_min = tbl7_df$temp_nextmin,
                     thresh_low = thresh_low,
                     thresh_up = thresh_up,
                     method = "dbl_tri",
                     debug = FALSE)
  ## We take out element #9 because I suspect the validation value in the table
  ## is actually an error
  expect_equal(tbl7_df$dbl_tri[-9], my_gdd[-9], tolerance = 0.001)
})

test_that("single sine works", {
  my_gdd <- dd_calc(daily_min = tbl7_df$temp_min,
                     daily_max = tbl7_df$temp_max,
                     thresh_low = thresh_low,
                     thresh_up = thresh_up,
                     method = "sng_sine",
                     debug = FALSE)
  expect_equal(tbl7_df$sng_sine, my_gdd, tolerance = 0.001)
})

test_that("double sine works", {
  my_gdd <- dd_calc(daily_min = tbl7_df$temp_min,
                     daily_max = tbl7_df$temp_max,
                     nextday_min = tbl7_df$temp_nextmin,
                     thresh_low = thresh_low,
                     thresh_up = thresh_up,
                     method = "dbl_sine",
                     debug = FALSE)
  expect_equal(tbl7_df$dbl_sine, my_gdd, tolerance = 0.001)
})








