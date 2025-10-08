test_that("Logistic apply works - data.frame", {
  sl_logistic <- cal_estimate_logistic(segment_logistic, Class, smooth = FALSE)
  ap_logistic <- cal_apply(segment_logistic, sl_logistic)

  pred_good <- ap_logistic$.pred_good
  expect_equal(mean(pred_good), 0.3425743, tolerance = 0.000001)
  expect_equal(sd(pred_good), 0.2993934, tolerance = 0.000001)
})

test_that("Logistic apply works - tune_results", {
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_binary()
  tl_logistic <- cal_estimate_logistic(tct, smooth = FALSE)
  tap_logistic <- cal_apply(tct, tl_logistic)
  expect_equal(
    testthat_cal_binary_count(),
    nrow(tap_logistic)
  )
})

test_that("Logistic spline apply works", {
  sl_gam <- cal_estimate_logistic(segment_logistic, Class)
  ap_gam <- cal_apply(segment_logistic, sl_gam)

  pred_good <- ap_gam$.pred_good
  expect_equal(mean(pred_good), 0.3425743, tolerance = 0.000001)
  expect_equal(sd(pred_good), 0.2987027, tolerance = 0.000001)
})

test_that("Logistic spline apply works - tune_results", {
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_binary()
  tl_gam <- cal_estimate_logistic(tct)
  tap_gam <- cal_apply(tct, tl_gam)
  expect_equal(
    testthat_cal_binary_count(),
    nrow(tap_gam)
  )
})