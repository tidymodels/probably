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

  # ----------------------------------------------------------------------------

  met <- collect_metrics(val_obj)
  expect_equal(met$.type, c("uncalibrated", "calibrated"))
  expect_equal(
    names(met),
    c(".metric", ".type", ".estimator", "mean", "n", "std_err", ".config")
  )

  met_rs <- collect_metrics(val_obj, summarize = FALSE)
  expect_equal(sort(unique(met_rs$.type)), c("calibrated", "uncalibrated"))
  expect_equal(
    names(met_rs),
    c("id", ".metric", ".type", ".estimator", ".estimate", ".config")
  )
  expect_equal(nrow(met_rs), nrow(df) * 2)

  expect_snapshot_error(collect_predictions(val_obj))

  pred_rs <- collect_predictions(val_with_pred)
  expect_equal(sort(unique(pred_rs$.type)), c("calibrated"))

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(pred_rs),
    c(
      ".pred_class", ".pred_poor", ".pred_good", "Class", ".row", ".config",
      ".type"
    )
  )
  expect_equal(nrow(pred_rs), nrow(df$splits[[1]]$data))
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
  val_with_pred <- cal_validate_linear(df, outcome, save_pred = TRUE, smooth = FALSE)

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
    c("outcome", ".pred", "id", ".row")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
})

test_that("Isotonic validation regression with data frame input", {
  df <- testthat_cal_reg_sampled()
  val_obj <- cal_validate_isotonic(df, outcome, estimate = .pred)
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
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c("outcome", ".pred", "id", ".row")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
})

test_that("Bootstrapped Isotonic regression validation with data frame input", {
  df <- testthat_cal_reg_sampled()
  val_obj <- cal_validate_isotonic_boot(df, outcome, estimate = .pred)
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
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c("outcome", ".pred", "id", ".row")
  )
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
  expect_equal(nrow(val_obj), nrow(res$binary))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$binary))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_class_1", ".pred_class_2", ".row", "outcome", ".config", ".pred_class")
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
  expect_equal(nrow(val_obj), nrow(res$binary))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$binary))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_class_1", ".pred_class_2", ".row", "outcome", ".config", ".pred_class")
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
  expect_equal(nrow(val_obj), nrow(res$binary))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$binary))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_class_1", ".pred_class_2", ".row", "outcome", ".config", ".pred_class")
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
  expect_equal(nrow(val_obj), nrow(res$binary))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$binary))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_class_1", ".pred_class_2", ".row", "outcome", ".config", ".pred_class")
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
  expect_equal(nrow(val_obj), nrow(res$multin))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$multin))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred_one", ".pred_two", ".pred_three", ".row", "outcome", ".config", ".pred_class")
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
  val_with_pred <- cal_validate_linear(res$reg, save_pred = TRUE, smooth = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(res$reg))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$reg))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred", ".row", "outcome", ".config")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

  expect_equal(val_obj$.metrics[[1]]$.metric, c("rmse", "rsq"))

  # ----------------------------------------------------------------------------

  met <- collect_metrics(val_obj)
  expect_equal(met$.type, rep(c("uncalibrated", "calibrated"), each = 2))
  expect_equal(
    names(met),
    c(".metric", ".type", ".estimator", "mean", "n", "std_err", ".config")
  )

  met_rs <- collect_metrics(val_obj, summarize = FALSE)
  expect_equal(sort(unique(met_rs$.type)), c("calibrated", "uncalibrated"))
  expect_equal(
    names(met_rs),
    c("id", ".metric", ".type", ".estimator", ".estimate", ".config")
  )
  expect_equal(nrow(met_rs), nrow(res$reg) * 2 * 2)

  pred <- collect_predictions(val_obj)
  expect_equal(sort(unique(pred$.type)), c("uncalibrated"))

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(pred),
    c(".pred", ".row", "outcome", ".config", ".type")
  )
  expect_equal(nrow(pred), nrow(val_obj$splits[[1]]$data))

  pred_rs <- collect_predictions(val_with_pred)
  expect_equal(sort(unique(pred_rs$.type)), c("calibrated", "uncalibrated"))

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(pred_rs),
    c(".pred", ".row", "outcome", ".config", ".type")
  )
  expect_equal(nrow(pred_rs), nrow(val_obj$splits[[1]]$data) * 2)
})


test_that("Isotonic regression validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()
  mtr <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  val_obj <- cal_validate_isotonic(res$reg, estimate = .pred, metrics = mtr)
  val_with_pred <- cal_validate_isotonic(res$reg, save_pred = TRUE, smooth = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(res$reg))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$reg))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred", ".row", "outcome", ".config")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

  expect_equal(val_obj$.metrics[[1]]$.metric, c("rmse", "rsq"))
})


test_that("Isotonic bootstrapped regression validation with `fit_resamples`", {
  res <- testthat_cal_fit_rs()
  mtr <- yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  val_obj <- cal_validate_isotonic_boot(res$reg, estimate = .pred, metrics = mtr)
  val_with_pred <- cal_validate_isotonic_boot(res$reg, save_pred = TRUE, smooth = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(res$reg))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal")
  )

  expect_s3_class(val_with_pred, "data.frame")
  expect_s3_class(val_with_pred, "cal_rset")
  expect_equal(nrow(val_with_pred), nrow(res$reg))
  expect_equal(
    names(val_with_pred),
    c("splits", "id", ".notes", ".predictions", ".metrics", ".metrics_cal", ".predictions_cal")
  )

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    names(val_with_pred$.predictions_cal[[1]]),
    c(".pred", ".row", "outcome", ".config")
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )

  expect_equal(val_obj$.metrics[[1]]$.metric, c("rmse", "rsq"))
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
