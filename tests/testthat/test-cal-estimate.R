test_that("Logistic estimates work", {
  sl_logistic <- cal_estimate_logistic(segment_logistic, Class)
  expect_cal_type(sl_logistic, "binary")
  expect_cal_method(sl_logistic, "Logistic")
  expect_cal_estimate(sl_logistic, "glm")
  expect_snapshot(print(sl_logistic))
})

test_that("Logistic spline estimates work", {
  sl_gam <- cal_estimate_logistic_spline(segment_logistic, Class)
  expect_cal_type(sl_gam, "binary")
  expect_cal_method(sl_gam, "Logistic Spline")
  expect_cal_estimate(sl_gam, "gam")
  expect_snapshot(print(sl_gam))
})

test_that("Isotonic estimates work", {
  sl_isotonic <- cal_estimate_isotonic(segment_logistic, Class)
  expect_cal_type(sl_isotonic, "binary")
  expect_cal_method(sl_isotonic, "Isotonic")
  expect_cal_estimate(sl_isotonic, "data.frame")
  expect_snapshot(print(sl_isotonic))
})

test_that("Isotonic Bootstrapped estimates work", {
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)
  expect_cal_type(sl_boot, "binary")
  expect_cal_method(sl_boot, "Isotonic Bootstrapped")
  expect_cal_estimate(sl_boot, "data.frame")
  expect_snapshot(print(sl_boot))
})

test_that("Non-default names used for estimate columns", {
  new_segment <- segment_logistic
  colnames(new_segment) <- c("poor", "good", "Class")

  expect_snapshot(
    cal_estimate_isotonic(new_segment, Class, c(good, poor))
  )
})

test_that("Test exceptions", {
  expect_error(
    cal_estimate_isotonic(segment_logistic, Class, dplyr::starts_with("bad"))
  )
})
