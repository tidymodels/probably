test_that("Isotonic estimates work - data.frame", {
  skip_if_not_installed("modeldata")

  set.seed(100)
  sl_isotonic <- cal_estimate_isotonic(segment_logistic, Class)
  expect_cal_type(sl_isotonic, "binary")
  expect_cal_method(sl_isotonic, "Isotonic regression calibration")
  expect_cal_rows(sl_isotonic)
  expect_snapshot(print(sl_isotonic))

  set.seed(100)
  sl_isotonic_group <- segment_logistic |>
    dplyr::mutate(group = .pred_poor > 0.5) |>
    cal_estimate_isotonic(Class, .by = group)

  expect_cal_type(sl_isotonic_group, "binary")
  expect_cal_method(sl_isotonic_group, "Isotonic regression calibration")
  expect_cal_rows(sl_isotonic_group)
  expect_snapshot(print(sl_isotonic_group))

  set.seed(100)
  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_isotonic(Class, .by = c(group1, group2))
  )

})

test_that("Isotonic estimates work - tune_results", {
  skip_if_not_installed("modeldata")

  set.seed(100)
  tl_isotonic <- cal_estimate_isotonic(testthat_cal_binary())
  expect_cal_type(tl_isotonic, "binary")
  expect_cal_method(tl_isotonic, "Isotonic regression calibration")
  expect_snapshot(print(tl_isotonic))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_isotonic))
  )

  # ------------------------------------------------------------------------------
  # multinomial outcomes

  set.seed(100)
  mtnl_isotonic <- cal_estimate_isotonic(testthat_cal_multiclass())
  expect_cal_type(mtnl_isotonic, "one_vs_all")
  expect_cal_method(mtnl_isotonic, "Isotonic regression calibration")
  expect_snapshot(print(mtnl_isotonic))

  expect_equal(
    testthat_cal_multiclass_count(),
    nrow(cal_apply(testthat_cal_multiclass(), mtnl_isotonic))
  )
})

test_that("Isotonic estimates errors - grouped_df", {
  expect_snapshot_error(
    cal_estimate_isotonic(dplyr::group_by(mtcars, vs))
  )
})

test_that("Isotonic linear estimates work - data.frame", {
  skip_if_not_installed("modeldata")

  set.seed(2983)
  sl_logistic <- cal_estimate_isotonic(boosting_predictions_oob, outcome, estimate = .pred)
  expect_cal_type(sl_logistic, "regression")
  expect_cal_method(sl_logistic, "Isotonic regression calibration")
  expect_cal_rows(sl_logistic, 2000)
  expect_snapshot(print(sl_logistic))

  set.seed(38)
  sl_logistic_group <- boosting_predictions_oob |>
    cal_estimate_isotonic(outcome, estimate = .pred, .by = id)

  expect_cal_type(sl_logistic_group, "regression")
  expect_cal_method(sl_logistic_group, "Isotonic regression calibration")
  expect_cal_rows(sl_logistic_group, 2000)
  expect_snapshot(print(sl_logistic_group))

  expect_snapshot_error(
    boosting_predictions_oob |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_isotonic(outcome, estimate = .pred, .by = c(group1, group2))
  )
})

# -------------------------- Isotonic Bootstrapped -----------------------------
test_that("Isotonic Bootstrapped estimates work - data.frame", {
  skip_if_not_installed("modeldata")

  set.seed(1)
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)
  expect_cal_type(sl_boot, "binary")
  expect_cal_method(sl_boot, "Bootstrapped isotonic regression calibration")
  expect_snapshot(print(sl_boot))

  sl_boot_group <- segment_logistic |>
    dplyr::mutate(group = .pred_poor > 0.5) |>
    cal_estimate_isotonic_boot(Class, .by = group)

  expect_cal_type(sl_boot_group, "binary")
  expect_cal_method(sl_boot_group, "Bootstrapped isotonic regression calibration")
  expect_snapshot(print(sl_boot_group))

  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_isotonic_boot(Class, .by = c(group1, group2))
  )

})

test_that("Isotonic Bootstrapped estimates work - tune_results", {
  skip_if_not_installed("modeldata")

  set.seed(100)
  tl_isotonic <- cal_estimate_isotonic_boot(testthat_cal_binary())
  expect_cal_type(tl_isotonic, "binary")
  expect_cal_method(tl_isotonic, "Bootstrapped isotonic regression calibration")
  expect_snapshot(print(tl_isotonic))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_isotonic))
  )

  # ------------------------------------------------------------------------------
  # multinomial outcomes

  set.seed(100)
  mtnl_isotonic <- cal_estimate_isotonic_boot(testthat_cal_multiclass())
  expect_cal_type(mtnl_isotonic, "one_vs_all")
  expect_cal_method(mtnl_isotonic, "Bootstrapped isotonic regression calibration")
  expect_snapshot(print(mtnl_isotonic))

  expect_equal(
    testthat_cal_multiclass_count(),
    nrow(cal_apply(testthat_cal_multiclass(), mtnl_isotonic))
  )
})

test_that("Isotonic Bootstrapped estimates errors - grouped_df", {
  expect_snapshot_error(
    cal_estimate_isotonic_boot(dplyr::group_by(mtcars, vs))
  )
})
