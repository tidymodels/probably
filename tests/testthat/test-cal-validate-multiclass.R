test_that("Isotonic validation with data frame input - Multiclass", {
  df <- rsample::vfold_cv(testthat_cal_sim_multi())
  val_obj <- cal_validate_isotonic(df, class)
  val_with_pred <- cal_validate_isotonic(df, class, save_pred = TRUE)

  expect_s3_class(val_obj, "data.frame")
  expect_s3_class(val_obj, "cal_rset")
  expect_equal(nrow(val_obj), nrow(df))
  expect_equal(
    names(val_obj),
    c("splits", "id", ".metrics", ".metrics_cal")
  )

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

test_that("Isotonic validation with `fit_resamples` - Multiclass", {
  res <- testthat_cal_fit_rs()
  val_obj <- cal_validate_isotonic(res$multin)
  val_with_pred <- cal_validate_isotonic(res$multin, save_pred = TRUE)

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
    sort(names(val_with_pred$.predictions_cal[[1]])),
    sort(c(".pred_one", ".pred_two", ".pred_three", ".row", "outcome", ".config", ".pred_class"))
  )
  expect_equal(
    purrr::map_int(val_with_pred$splits, ~ holdout_length(.x)),
    purrr::map_int(val_with_pred$.predictions_cal, nrow)
  )
})
