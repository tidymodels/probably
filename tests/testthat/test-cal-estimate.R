# --------------------------------- Logistic -----------------------------------
test_that("Logistic estimates work - data.frame", {
  sl_logistic <- cal_estimate_logistic(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_logistic, "binary")
  expect_cal_method(sl_logistic, "Logistic")
  expect_cal_estimate(sl_logistic, "butchered_glm")
  expect_cal_rows(sl_logistic)
  expect_snapshot(print(sl_logistic))
})

test_that("Logistic estimates work - tune_results", {
  tl_logistic <- cal_estimate_logistic(testthat_cal_binary(), smooth = FALSE)
  expect_cal_type(tl_logistic, "binary")
  expect_cal_method(tl_logistic, "Logistic")
  expect_cal_estimate(tl_logistic, "butchered_glm")
  expect_snapshot(print(tl_logistic))
})

# ----------------------------- Logistic Spline --------------------------------
test_that("Logistic spline estimates work - data.frame", {
  sl_gam <- cal_estimate_logistic(segment_logistic, Class)
  expect_cal_type(sl_gam, "binary")
  expect_cal_method(sl_gam, "Logistic Spline")
  expect_cal_estimate(sl_gam, "butchered_gam")
  expect_cal_rows(sl_gam)
  expect_snapshot(print(sl_gam))
})

test_that("Logistic spline estimates work - tune_results", {
  tl_gam <- cal_estimate_logistic(testthat_cal_binary())
  expect_cal_type(tl_gam, "binary")
  expect_cal_method(tl_gam, "Logistic Spline")
  expect_cal_estimate(tl_gam, "butchered_gam")
  expect_snapshot(print(tl_gam))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_gam))
  )
})

# --------------------------------- Isotonic -----------------------------------
test_that("Isotonic estimates work - data.frame", {
  set.seed(100)
  sl_isotonic <- cal_estimate_isotonic(segment_logistic, Class)
  expect_cal_type(sl_isotonic, "binary")
  expect_cal_method(sl_isotonic, "Isotonic")
  expect_cal_rows(sl_isotonic)
  expect_snapshot(print(sl_isotonic))
})

test_that("Isotonic estimates work - tune_results", {
  set.seed(100)
  tl_isotonic <- cal_estimate_isotonic(testthat_cal_binary())
  expect_cal_type(tl_isotonic, "binary")
  expect_cal_method(tl_isotonic, "Isotonic")
  expect_snapshot(print(tl_isotonic))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_isotonic))
  )
})

test_that("Isotonic linear estimates work - data.frame", {
  sl_logistic <- cal_estimate_isotonic(boosting_predictions_oob, outcome, estimate = .pred)
  expect_cal_type(sl_logistic, "regression")
  expect_cal_method(sl_logistic, "Isotonic")
  expect_cal_rows(sl_logistic, 2000)
  expect_snapshot(print(sl_logistic))
})

# -------------------------- Isotonic Bootstrapped -----------------------------
test_that("Isotonic Bootstrapped estimates work", {
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)
  expect_cal_type(sl_boot, "binary")
  expect_cal_method(sl_boot, "Bootstrapped Isotonic Regression")
  expect_snapshot(print(sl_boot))
})

test_that("Isotonic Bootstrapped estimates work - tune_results", {
  set.seed(100)
  tl_isotonic <- cal_estimate_isotonic_boot(testthat_cal_binary())
  expect_cal_type(tl_isotonic, "binary")
  expect_cal_method(tl_isotonic, "Bootstrapped Isotonic Regression")
  expect_snapshot(print(tl_isotonic))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_isotonic))
  )
})

# ----------------------------------- Beta -------------------------------------
test_that("Beta estimates work - data.frame", {
  sl_beta <- cal_estimate_beta(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_beta, "binary")
  expect_cal_method(sl_beta, "Beta")
  expect_cal_rows(sl_beta)
  expect_snapshot(print(sl_beta))
})

test_that("Beta estimates work - tune_results", {
  tl_beta <- cal_estimate_beta(testthat_cal_binary())
  expect_cal_type(tl_beta, "binary")
  expect_cal_method(tl_beta, "Beta")
  expect_snapshot(print(tl_beta))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_beta))
  )
})

# ------------------------------ Multinomial -----------------------------------
test_that("Multinomial estimates work - data.frame", {
  sp_multi <- cal_estimate_multinomial(species_probs, Species, smooth = FALSE)
  expect_cal_type(sp_multi, "multiclass")
  expect_cal_method(sp_multi, "Multinomial")
  expect_cal_rows(sp_multi, n = 110)
  expect_snapshot(print(sp_multi))

  sp_smth_multi <- cal_estimate_multinomial(species_probs, Species, smooth = TRUE)
  expect_cal_type(sp_smth_multi, "multiclass")
  expect_cal_method(sp_smth_multi, "Multinomial")
  expect_cal_rows(sp_smth_multi, n = 110)
  expect_snapshot(print(sp_smth_multi))
})

test_that("Multinomial estimates work - tune_results", {
  tl_multi <- cal_estimate_multinomial(testthat_cal_multiclass(), smooth = FALSE)
  expect_cal_type(tl_multi, "multiclass")
  expect_cal_method(tl_multi, "Multinomial")
  expect_snapshot(print(tl_multi))

  expect_equal(
    testthat_cal_multiclass() %>%
      tune::collect_predictions(summarize = TRUE) %>%
      nrow(),
    testthat_cal_multiclass() %>%
      cal_apply(tl_multi) %>%
      nrow()
  )

  tl_smth_multi <- cal_estimate_multinomial(testthat_cal_multiclass(), smooth = TRUE)
  expect_cal_type(tl_smth_multi, "multiclass")
  expect_cal_method(tl_smth_multi, "Multinomial")
  expect_snapshot(print(tl_smth_multi))

  expect_equal(
    testthat_cal_multiclass() %>%
      tune::collect_predictions(summarize = TRUE) %>%
      nrow(),
    testthat_cal_multiclass() %>%
      cal_apply(tl_smth_multi) %>%
      nrow()
  )
})

test_that("Passing a binary outcome causes error", {
  expect_error(
    cal_estimate_multinomial(segment_logistic, Class)
  )
})

# --------------------------------- Linear -----------------------------------
test_that("Linear estimates work - data.frame", {
  sl_logistic <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = FALSE)
  expect_cal_type(sl_logistic, "regression")
  expect_cal_method(sl_logistic, "Linear")
  expect_cal_estimate(sl_logistic, "butchered_glm")
  expect_cal_rows(sl_logistic, 2000)
  expect_snapshot(print(sl_logistic))
})

test_that("Linear estimates work - tune_results", {
  tl_linear <- cal_estimate_linear(testthat_cal_reg(), outcome, smooth = FALSE)
  expect_cal_type(tl_linear, "regression")
  expect_cal_method(tl_linear, "Linear")
  expect_cal_estimate(tl_linear, "butchered_glm")
  expect_snapshot(print(tl_linear))
})

# ----------------------------- Linear Spline --------------------------------
test_that("Linear spline estimates work - data.frame", {
  sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome)
  expect_cal_type(sl_gam, "regression")
  expect_cal_method(sl_gam, "Linear Spline")
  expect_cal_estimate(sl_gam, "butchered_gam")
  expect_cal_rows(sl_gam, 2000)
  expect_snapshot(print(sl_gam))
})

test_that("Linear spline estimates work - tune_results", {
  tl_gam <- cal_estimate_linear(testthat_cal_reg(), outcome)
  expect_cal_type(tl_gam, "regression")
  expect_cal_method(tl_gam, "Linear Spline")
  expect_cal_estimate(tl_gam, "butchered_gam")
  expect_snapshot(print(tl_gam))

  expect_equal(
    testthat_cal_reg_count(),
    nrow(cal_apply(testthat_cal_reg(), tl_gam))
  )
})


# ----------------------------------- Other ------------------------------------
test_that("Non-default names used for estimate columns", {
  new_segment <- segment_logistic
  colnames(new_segment) <- c("poor", "good", "Class")

  set.seed(100)
  expect_snapshot(
    cal_estimate_isotonic(new_segment, Class, c(good, poor))
  )
})

test_that("Test exceptions", {
  expect_error(
    cal_estimate_isotonic(segment_logistic, Class, dplyr::starts_with("bad"))
  )
})
