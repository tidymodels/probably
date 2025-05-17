test_that("Linear estimates work - data.frame", {
  skip_if_not_installed("modeldata")

  sl_linear <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = FALSE)
  expect_cal_type(sl_linear, "regression")
  expect_cal_method(sl_linear, "Linear calibration")
  expect_cal_estimate(sl_linear, "butchered_glm")
  expect_cal_rows(sl_linear, 2000)
  expect_snapshot(print(sl_linear))

  sl_linear_group <- boosting_predictions_oob |>
    dplyr::mutate(group = .pred > 0.5) |>
    cal_estimate_linear(outcome, smooth = FALSE, .by = group)

  expect_cal_type(sl_linear_group, "regression")
  expect_cal_method(sl_linear_group, "Linear calibration")
  expect_cal_estimate(sl_linear_group, "butchered_glm")
  expect_cal_rows(sl_linear_group, 2000)
  expect_snapshot(print(sl_linear_group))

  expect_snapshot_error(
    boosting_predictions_oob |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_linear(outcome, smooth = FALSE, .by = c(group1, group2))
  )

})

test_that("Linear estimates work - tune_results", {
  tl_linear <- cal_estimate_linear(testthat_cal_reg(), outcome, smooth = FALSE)
  expect_cal_type(tl_linear, "regression")
  expect_cal_method(tl_linear, "Linear calibration")
  expect_cal_estimate(tl_linear, "butchered_glm")
  expect_snapshot(print(tl_linear))

})

test_that("Linear estimates errors - grouped_df", {
  expect_snapshot_error(
    cal_estimate_linear(dplyr::group_by(mtcars, vs))
  )
})

# ----------------------------- Linear Spline --------------------------------
test_that("Linear spline estimates work - data.frame", {
  skip_if_not_installed("modeldata")

  sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome)
  expect_cal_type(sl_gam, "regression")
  expect_cal_method(sl_gam, "Generalized additive model calibration")
  expect_cal_estimate(sl_gam, "butchered_gam")
  expect_cal_rows(sl_gam, 2000)
  expect_snapshot(print(sl_gam))

  sl_gam_group <- boosting_predictions_oob |>
    dplyr::mutate(group = .pred > 0.5) |>
    cal_estimate_linear(outcome, .by = group)

  expect_cal_type(sl_gam_group, "regression")
  expect_cal_method(sl_gam_group, "Generalized additive model calibration")
  expect_cal_estimate(sl_gam_group, "butchered_gam")
  expect_cal_rows(sl_gam_group, 2000)
  expect_snapshot(print(sl_gam_group))

  expect_snapshot_error(
    boosting_predictions_oob |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_linear(outcome, .by = c(group1, group2))
  )
})

test_that("Linear spline estimates work - tune_results", {
  tl_gam <- cal_estimate_linear(testthat_cal_reg(), outcome)
  expect_cal_type(tl_gam, "regression")
  expect_cal_method(tl_gam, "Generalized additive model calibration")
  expect_cal_estimate(tl_gam, "butchered_gam")
  expect_snapshot(print(tl_gam))

  expect_equal(
    testthat_cal_reg_count(),
    nrow(cal_apply(testthat_cal_reg(), tl_gam))
  )
})

test_that("Linear spline switches to linear if too few unique", {
  skip_if_not_installed("modeldata")

  boosting_predictions_oob$.pred <- rep(
    x = 1:5,
    length.out = nrow(boosting_predictions_oob)
  )

  expect_snapshot(
    sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = TRUE)
  )
  sl_lm <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = FALSE)

  expect_identical(
    sl_gam$estimates,
    sl_lm$estimates
  )

  expect_snapshot(
    sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id, smooth = TRUE)
  )
  sl_lm <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id, smooth = FALSE)

  expect_identical(
    sl_gam$estimates,
    sl_lm$estimates
  )
})

test_that("Linear spline switches to linear if too few unique", {
  skip_if_not_installed("modeldata")

  boosting_predictions_oob$.pred <- rep(
    x = 1:5,
    length.out = nrow(boosting_predictions_oob)
  )

  expect_snapshot(
    sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = TRUE)
  )
  sl_lm <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = FALSE)

  expect_identical(
    sl_gam$estimate,
    sl_lm$estimate
  )

  expect_snapshot(
    sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id, smooth = TRUE)
  )
  sl_lm <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id, smooth = FALSE)

  expect_identical(
    sl_gam$estimate,
    sl_lm$estimate
  )
})

