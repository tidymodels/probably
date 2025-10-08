test_that("Linear apply works - data.frame", {
  sl_linear <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = FALSE)
  ap_linear <- cal_apply(boosting_predictions_oob, sl_linear)

  pred <- ap_linear$.pred
  expect_equal(mean(pred), 14.87123, tolerance = 0.000001)
  expect_equal(sd(pred), 14.94483, tolerance = 0.000001)
})

test_that("Linear apply works - tune_results", {
  tct <- testthat_cal_reg()
  tl_linear <- cal_estimate_linear(tct, smooth = FALSE)
  tap_linear <- cal_apply(tct, tl_linear)
  expect_equal(
    testthat_cal_reg_count(),
    nrow(tap_linear)
  )
})

test_that("Linear spline apply works", {
  sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome)
  ap_gam <- cal_apply(boosting_predictions_oob, sl_gam)

  pred <- ap_gam$.pred
  expect_equal(mean(pred), 14.87123, tolerance = 0.000001)
  expect_equal(sd(pred), 15.00711, tolerance = 0.000001)
})

test_that("Linear spline apply works - tune_results", {
  tct <- testthat_cal_reg()
  tl_gam <- cal_estimate_linear(tct)
  tap_gam <- cal_apply(tct, tl_gam)
  expect_equal(
    testthat_cal_reg_count(),
    nrow(tap_gam)
  )
})