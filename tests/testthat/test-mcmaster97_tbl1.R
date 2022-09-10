## This runs a series of tests using the verification table provided by
## McMaster and Wilhelm (1997) https://doi.org/10.1016/S0168-1923(97)00027-0
## For the simple average method 1 and 2

## Load `mcmaster_tab1_tbl` which is a copy of Table 1 from McMaster and Wilhelm (1997)
load("mcmaster97_tbl1.RData")

test_that("simple average method 1 works", {
  gdd_method1_vec <- dd_simp_avg(daily_min = mcmaster97_tabl1_df$tmin,
                                 daily_max = mcmaster97_tabl1_df$tmax,
                                 thresh_low = 0,
                                 simp_avg_zero_method = 1,
                                 quiet = TRUE)

  expect_equal(gdd_method1_vec, mcmaster97_tabl1_df$gdd_method1, tolerance = 0.001)
})

test_that("simple average method 2 works", {
  gdd_method2_vec <- dd_simp_avg(daily_min = mcmaster97_tabl1_df$tmin,
                                 daily_max = mcmaster97_tabl1_df$tmax,
                                 thresh_low = 0,
                                 simp_avg_zero_method = 2,
                                 quiet = TRUE)

  expect_equal(gdd_method2_vec, mcmaster97_tabl1_df$gdd_method2, tolerance = 0.001)
})

