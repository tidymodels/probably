test_that("Logistic apply work", {
  sl_logistic <- cal_estimate_logistic(segment_logistic, Class, smooth = FALSE)
  ap_logistic <- cal_apply(segment_logistic, sl_logistic)

  pred_good <-  ap_logistic$.pred_good
  expect_equal(mean(pred_good), 0.3425743, tolerance = 0.000001)
  expect_equal(sd(pred_good), 0.2993934, tolerance = 0.000001)
})

test_that("Logistic spline apply work", {
  sl_gam <- cal_estimate_logistic(segment_logistic, Class)
  ap_gam <- cal_apply(segment_logistic, sl_gam)

  pred_good <-  ap_gam$.pred_good
  expect_equal(mean(pred_good), 0.3425743, tolerance = 0.000001)
  expect_equal(sd(pred_good), 0.2987027, tolerance = 0.000001)
})

test_that("Isotonic apply work", {
  sl_isotonic <- cal_estimate_isotonic(segment_logistic, Class)
  ap_isotonic <- cal_apply(segment_logistic, sl_isotonic)

  pred_good <-  ap_isotonic$.pred_good
  expect_equal(mean(pred_good), 0.3015192, tolerance = 0.000001)
  expect_equal(sd(pred_good), 0.2870775, tolerance = 0.000001)
})

test_that("Isotonic Bootstrapped apply work", {
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)
  ap_boot <- cal_apply(segment_logistic, sl_boot)

  expect_true(all(ap_boot$.pred_poor + ap_boot$.pred_good == 1))
})

test_that("Passing the data frame first returns expected abort message", {
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)

  expect_error(
    cal_apply(sl_boot, segment_logistic)
    )
})
