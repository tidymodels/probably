test_that("Logistic validation with data frame input", {
  df <- testthat_cal_sampled()
  val_obj <- cal_validate_logistic(df, Class)
  val_with_pred <- cal_validate_logistic(df, Class, save_pred = TRUE, smooth = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_poor", ".pred_good", "Class", ".row", ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})


test_that("Beta validation with data frame input", {
  df <- testthat_cal_sampled()
  val_obj <- cal_validate_beta(df, Class)
  val_with_pred <- cal_validate_beta(df, Class, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_poor", ".pred_good", "Class", ".row", ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})


test_that("Isotonic validation classification with data frame input", {
  df <- testthat_cal_sampled()
  val_obj <- cal_validate_isotonic(df, Class)
  val_with_pred <- cal_validate_isotonic(df, Class, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_poor", ".pred_good", "Class", ".row", ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})

test_that("Bootstrapped Isotonic classification validation with data frame input", {
  df <- testthat_cal_sampled()
  val_obj <- cal_validate_isotonic_boot(df, Class)
  val_with_pred <- cal_validate_isotonic_boot(df, Class, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_poor", ".pred_good", "Class", ".row", ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})

test_that("Multinomial classification validation with data frame input", {
  df <- rsample::vfold_cv(testthat_cal_sim_multi())
  val_obj <- cal_validate_multinomial(df, class)
  val_with_pred <- cal_validate_multinomial(df, class, save_pred = TRUE, smooth = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_one", ".pred_two", ".pred_three", "class", ".row", ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})

# ------------------------------------------------------------------------------

test_that("Linear validation with data frame input", {
  df <- testthat_cal_reg_sampled()
  val_obj <- cal_validate_linear(df, outcome)
  val_with_pred <- cal_validate_linear(df, outcome, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  # expect_equal(
  #   names(val_with_pred$.predictions_cal[[1]]),
  #   c("outcome", ".pred", "id", ".row",)
  # )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})

test_that("Isotonic validation regression with data frame input", {
  skip("Not working")
  df <- testthat_cal_reg_sampled()
  val_obj <- cal_validate_isotonic(df, outcome)
  val_with_pred <- cal_validate_isotonic(df, outcome, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  # expect_equal(
  #   names(val_with_pred$.predictions_cal[[1]]),
  #   c("outcome", ".pred", "id", ".row",)
  # )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})

test_that("Bootstrapped Isotonic regression validation with data frame input", {
  skip("Not working")
  df <- testthat_cal_reg_sampled()
  val_obj <- cal_validate_isotonic_boot(df, outcome)
  val_with_pred <- cal_validate_isotonic_boot(df, outcome, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(names(val_obj), c("splits", "id", ".metrics", ".metrics_cal"))

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  # expect_equal(
  #   names(val_with_pred$.predictions_cal[[1]]),
  #   c("outcome", ".pred", "id", ".row",)
  # )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
})

# ------------------------------------------------------------------------------

test_that("Logistic validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()
  val_obj <- cal_validate_logistic(res$binary)
  val_with_pred <- cal_validate_logistic(res$binary, save_pred = TRUE, smooth = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".row", "outcome", ".config", ".pred_class_1", ".pred_class_2",  ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

})

test_that("Isotonic classification validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()
  val_obj <- cal_validate_isotonic(res$binary)
  val_with_pred <- cal_validate_isotonic(res$binary, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".row", "outcome", ".config", ".pred_class_1", ".pred_class_2",  ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
  expect_snapshot_warning(cal_validate_isotonic(res$binary, truth = "huh?"))
})

test_that("Bootstrapped isotonic classification validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()
  val_obj <- cal_validate_isotonic_boot(res$binary)
  val_with_pred <- cal_validate_isotonic_boot(res$binary, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".row", "outcome", ".config", ".pred_class_1", ".pred_class_2",  ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
})

test_that("Beta calibration validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()
  val_obj <- cal_validate_beta(res$binary)
  val_with_pred <- cal_validate_beta(res$binary, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".row", "outcome", ".config", ".pred_class_1", ".pred_class_2",  ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
})

test_that("Multinomial calibration validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()
  val_obj <- cal_validate_multinomial(res$multin)
  val_with_pred <- cal_validate_multinomial(res$multin, save_pred = TRUE, smooth = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(df))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".row", "outcome", ".config", ".pred_one", ".pred_two", ".pred_three", ".pred_class")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
})

# ------------------------------------------------------------------------------
# TODO change this and add other calibration methods

test_that("Linear validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()

  mtr <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  val_obj <- cal_validate_linear(res$reg, metrics = mtr)
  expect_equal(
    names(val_obj),
    c(".metric", ".estimator", "direction", "stage", ".estimate")
  )
  expect_equal(nrow(val_obj), 4)
  expect_equal(
    val_obj$direction,
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


# ------------------------------------------------------------------------------


test_that("validation functions error with tune_results input", {
  expect_snapshot_error(
    cal_validate_beta(testthat_cal_binary())
  )
  expect_snapshot_error(
    cal_validate_isotonic(testthat_cal_binary())
  )
  expect_snapshot_error(
    cal_validate_isotonic_boot(testthat_cal_binary())
  )
  expect_snapshot_error(
    cal_validate_linear(testthat_cal_binary())
  )
  expect_snapshot_error(
    cal_validate_logistic(testthat_cal_binary())
  )
  expect_snapshot_error(
    cal_validate_multinomial(testthat_cal_binary())
  )
})


