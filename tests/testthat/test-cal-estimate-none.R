test_that("no calibration works - data.frame", {
  skip_if_not_installed("modeldata")

  ## Regression

  nope_reg <- cal_estimate_none(boosting_predictions_oob, outcome)
  expect_cal_type(nope_reg, "regression")
  expect_cal_method(nope_reg, "No calibration")
  expect_cal_rows(nope_reg, 2000)
  expect_snapshot(print(nope_reg))
  expect_equal(
    cal_apply(boosting_predictions_oob, nope_reg),
    boosting_predictions_oob
  )

  reg_group_data <- boosting_predictions_oob |>
    dplyr::mutate(group = .pred > 0.5)

  nope_reg_group <- cal_estimate_none(reg_group_data, outcome, .by = group)
  expect_cal_type(nope_reg_group, "regression")
  expect_cal_method(nope_reg_group, "No calibration")
  expect_cal_rows(nope_reg_group, 2000)
  expect_snapshot(print(nope_reg_group))
  expect_equal(
    cal_apply(reg_group_data, nope_reg_group),
    reg_group_data
  )

  expect_snapshot_error(
    boosting_predictions_oob |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_none(outcome, .by = c(group1, group2))
  )

  expect_snapshot_error(
    cal_estimate_none(boosting_predictions_oob, outcome, smooth = TRUE)
  )

  ## Binary classification

  nope_binary <- cal_estimate_none(segment_logistic, Class)
  expect_cal_type(nope_binary, "binary")
  expect_cal_method(nope_binary, "No calibration")
  expect_cal_rows(nope_binary)
  expect_snapshot(print(nope_binary))
  expect_equal(
    cal_apply(segment_logistic, nope_binary),
    segment_logistic
  )

  expect_snapshot_error(
    segment_logistic |> cal_estimate_none(truth = Class, estimate = .pred_poor)
  )

  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_none(Class, .by = c(group1, group2))
  )

  ## Multinomial classification

  nope_multi <- cal_estimate_none(species_probs, Species)
  expect_cal_type(nope_multi, "multiclass")
  expect_cal_method(nope_multi, "No calibration")
  expect_cal_rows(nope_multi, n = 110)
  expect_snapshot(print(nope_multi))
  expect_equal(
    cal_apply(species_probs, nope_multi),
    species_probs
  )

  expect_snapshot_error(
    species_probs |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_none(Species, .by = c(group1, group2))
  )

})

test_that("no calibration works - tune_results", {
  skip_if_not_installed("modeldata")

  ## Regression
  reg_pred <- collect_predictions(testthat_cal_reg())
  nope_reg <- cal_estimate_none(testthat_cal_reg(), outcome)
  expect_cal_type(nope_reg, "regression")
  expect_cal_method(nope_reg, "No calibration")
  expect_snapshot(print(nope_reg))
  expect_equal(
    cal_apply(reg_pred, nope_reg),
    reg_pred
  )

  expect_snapshot_error(
    cal_estimate_none(testthat_cal_reg(), outcome, do_something = FALSE)
  )

  ## Binary classification

  binary_pred <- collect_predictions(testthat_cal_binary())
  nope_binary <- cal_estimate_none(testthat_cal_binary())
  expect_cal_type(nope_binary, "binary")
  expect_cal_method(nope_binary, "No calibration")
  expect_snapshot(print(nope_binary))
  expect_equal(
    cal_apply(binary_pred, nope_binary),
    binary_pred
  )

  ## Multinomial classification

  multi_pred <- collect_predictions(testthat_cal_multiclass())
  nope_multi <- cal_estimate_none(testthat_cal_multiclass())
  expect_cal_type(nope_multi, "multiclass")
  expect_cal_method(nope_multi, "No calibration")
  expect_snapshot(print(nope_multi))
  expect_equal(
    cal_apply(multi_pred, nope_multi),
    multi_pred
  )

})

test_that("no calibration fails - grouped_df", {

  expect_snapshot_error(
    cal_estimate_none(dplyr::group_by(mtcars, vs))
  )

})

