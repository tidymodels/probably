test_that("Isotonic apply works - data.frame", {
  set.seed(100)

  sl_isotonic <- cal_estimate_isotonic(segment_logistic, Class)
  ap_isotonic <- cal_apply(segment_logistic, sl_isotonic)

  pred_good <- ap_isotonic$.pred_good
  expect_equal(mean(pred_good), 0.3015192, tolerance = 0.000001)
  expect_equal(sd(pred_good), 0.2870775, tolerance = 0.000001)
})

test_that("Isotonic apply works - tune_results", {
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_binary()
  tl_isotonic <- cal_estimate_isotonic(tct)
  tap_isotonic <- cal_apply(tct, tl_isotonic)
  expect_equal(
    testthat_cal_binary_count(),
    nrow(tap_isotonic)
  )
})

test_that("Isotonic Bootstrapped apply works - data.frame", {
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)
  ap_boot <- cal_apply(segment_logistic, sl_boot)

  expect_true(all(ap_boot$.pred_poor + ap_boot$.pred_good == 1))
})

test_that("Isotonic Bootstrapped apply works - tune_results", {
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_binary()
  tl_boot <- cal_estimate_isotonic_boot(tct)
  tap_boot <- cal_apply(tct, tl_boot)
  expect_equal(
    testthat_cal_binary_count(),
    nrow(tap_boot)
  )
})

# ------------------------------------------------------------------------------

test_that("Beta apply works - data.frame", {
  skip_if_not_installed("betacal")
  sl_beta <- cal_estimate_beta(segment_logistic, Class)
  ap_beta <- cal_apply(segment_logistic, sl_beta)

  pred_good <- ap_beta$.pred_good
  expect_equal(mean(pred_good), 0.3425743, tolerance = 0.000001)
  expect_equal(sd(pred_good), 0.294565, tolerance = 0.000001)
})

test_that("Beta apply works - tune_results", {
  skip_if_not_installed("betacal")
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_binary()
  tl_beta <- cal_estimate_beta(tct)
  tap_beta <- cal_apply(tct, tl_beta)
  expect_equal(
    testthat_cal_binary_count(),
    nrow(tap_beta)
  )
})

# ------------------------------------------------------------------------------

test_that("Passing the data frame first returns expected abort message", {
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)

  expect_error(
    cal_apply(sl_boot, segment_logistic)
  )
})

test_that("Passing a tune_results without saved predictions causes error", {
  skip_if_not_installed("betacal")
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_binary()
  tl_beta <- cal_estimate_beta(tct)
  expect_error(cal_apply(tune::ames_grid_search, tl_beta))
})

test_that("Passing a calibration object as the first arg fails", {
  skip_if_not_installed("betacal")
  sl_beta <- cal_estimate_beta(segment_logistic, Class)
  expect_error(cal_apply(sl_beta, segment_logistic))
})
