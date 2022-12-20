test_that("Logistic estimates work", {
  sl_logistic <- cal_estimate_logistic(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_logistic, "binary")
  expect_cal_method(sl_logistic, "Logistic")
  expect_cal_estimate(sl_logistic, "glm")
  expect_cal_rows(sl_logistic)
  expect_snapshot(print(sl_logistic))
})

test_that("Logistic spline estimates work", {
  sl_gam <- cal_estimate_logistic(segment_logistic, Class)
  expect_cal_type(sl_gam, "binary")
  expect_cal_method(sl_gam, "Logistic Spline")
  expect_cal_estimate(sl_gam, "gam")
  expect_cal_rows(sl_gam)
  expect_snapshot(print(sl_gam))
})

test_that("Isotonic estimates work", {
  sl_isotonic <- cal_estimate_isotonic(segment_logistic, Class)
  expect_cal_type(sl_isotonic, "binary")
  expect_cal_method(sl_isotonic, "Isotonic")
  expect_cal_estimate(sl_isotonic, "data.frame")
  expect_cal_rows(sl_isotonic)
  expect_snapshot(print(sl_isotonic))
})

test_that("Isotonic Bootstrapped estimates work", {
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)
  expect_cal_type(sl_boot, "binary")
  expect_cal_method(sl_boot, "Bootstrapped Isotonic Regression")
  expect_snapshot(print(sl_boot))
})

test_that("Beta estimates work", {
  sl_beta <- cal_estimate_beta(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_beta, "binary")
  expect_cal_method(sl_beta, "Beta")
  expect_cal_rows(sl_beta)
  expect_snapshot(print(sl_beta))
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
