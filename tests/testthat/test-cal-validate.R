test_that("Logistic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_logistic(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_logistic(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Linear validation works", {
  df <- testthat_cal_reg_sampled()

  res_sum <- cal_validate_linear(df, outcome)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_linear(df, outcome, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Isotonic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_isotonic(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_isotonic(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Bootstrapped Isotonic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_isotonic_boot(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_isotonic_boot(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Beta validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_beta(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_beta(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Setting summarize to FALSE returns new columns", {
  suppressPackageStartupMessages(library(yardstick))

  df <- testthat_cal_sampled()

  no_res <- cal_validate_beta(
    df, Class,
    summarize = FALSE,
    save_details = TRUE
    )

  expect_snapshot(no_res)

})

test_that("check metric direction", {
  library(yardstick)
  reg_stats <- metric_set(rmse, ccc)

  df <- testthat_cal_reg_sampled()

  res_sum <-
    cal_validate_linear(df, outcome, metrics = reg_stats) %>%
    dplyr::distinct(.metric, direction) %>%
    dplyr::arrange(.metric, direction) %>%
    dplyr::select(.metric, direction)
  metric_df <- tibble::as_tibble(reg_stats)%>%
    dplyr::distinct(.metric = metric, direction) %>%
    dplyr::arrange(.metric, direction) %>%
    dplyr::select(.metric, direction)

  expect_equal(res_sum, metric_df)

})

# ------------------------------------------------------------------------------


test_that("Logistic validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()

  res_sum <- cal_validate_logistic(res$binary)
  expect_equal(
    names(res_sum),
    c(".metric", ".estimator", "direction", "stage", ".estimate")
  )
  expect_equal(nrow(res_sum), 2)

  expect_snapshot_warning(cal_validate_logistic(res$binary, truth = "huh?"))
})

test_that("Isotonic regression validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()

  res_sum <- cal_validate_isotonic(res$binary)
  expect_equal(
    names(res_sum),
    c(".metric", ".estimator", "direction", "stage", ".estimate")
  )
  expect_equal(nrow(res_sum), 2)
  expect_snapshot_warning(cal_validate_isotonic(res$binary, truth = "huh?"))
})

test_that("Bootstrapped isotonic regression validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()

  res_sum <- cal_validate_isotonic_boot(res$binary)
  expect_equal(
    names(res_sum),
    c(".metric", ".estimator", "direction", "stage", ".estimate")
  )
  expect_equal(nrow(res_sum), 2)
  expect_snapshot_warning(cal_validate_isotonic_boot(res$binary, truth = "huh?"))
})

test_that("Beta calibration validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()

  res_sum <- cal_validate_beta(res$binary)
  expect_equal(
    names(res_sum),
    c(".metric", ".estimator", "direction", "stage", ".estimate")
  )
  expect_equal(nrow(res_sum), 2)
  expect_snapshot_warning(cal_validate_beta(res$binary, truth = "huh?"))
})

test_that("Multinomial calibration validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()

  res_sum <- cal_validate_multinomial(res$multin)
  expect_equal(
    names(res_sum),
    c(".metric", ".estimator", "direction", "stage", ".estimate")
  )
  expect_equal(nrow(res_sum), 2)
  expect_snapshot_warning(cal_validate_multinomial(res$multin, truth = "huh?"))
})

test_that("Linear validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()

  mtr <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  res_sum <- cal_validate_linear(res$reg, metrics = mtr)
  expect_equal(
    names(res_sum),
    c(".metric", ".estimator", "direction", "stage", ".estimate")
  )
  expect_equal(nrow(res_sum), 4)
  expect_equal(
    res_sum$direction,
    rep(c("minimize", "maximize"), each = 2)
  )
  expect_snapshot_warning(cal_validate_linear(res$reg, truth = "huh?"))

  res_unsum <-
    cal_validate_linear(
      res$reg,
      metrics = mtr,
      summarize = FALSE,
      smooth = FALSE,
      save_details = TRUE
    )
  expect_equal(
    names(res_unsum),
    c("splits", "id", ".metrics", ".notes", ".predictions", "calibration",
      "validation", "stats_after", "stats_before")
  )
  expect_equal(nrow(res_unsum), 10)

  mod <- res_unsum$calibration[[1]]$estimates[[1]]$estimate
  expect_true(inherits(mod, "lm"))
})

