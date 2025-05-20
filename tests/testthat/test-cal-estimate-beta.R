test_that("Beta estimates work - data.frame", {
  skip_if_not_installed("betacal")
  sl_beta <- cal_estimate_beta(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_beta, "binary")
  expect_cal_method(sl_beta, "Beta calibration")
  expect_cal_rows(sl_beta)
  expect_snapshot(print(sl_beta))

  sl_beta_group <- segment_logistic |>
    dplyr::mutate(group = .pred_poor > 0.5) |>
    cal_estimate_beta(Class, smooth = FALSE, .by = group)

  expect_cal_type(sl_beta_group, "binary")
  expect_cal_method(sl_beta_group, "Beta calibration")
  expect_cal_rows(sl_beta_group)
  expect_snapshot(print(sl_beta_group))

  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_beta(Class, smooth = FALSE, .by = c(group1, group2))
  )

})

test_that("Beta estimates work - tune_results", {
  skip_if_not_installed("betacal")
  skip_if_not_installed("modeldata")

  tl_beta <- cal_estimate_beta(testthat_cal_binary())
  expect_cal_type(tl_beta, "binary")
  expect_cal_method(tl_beta, "Beta calibration")
  expect_snapshot(print(tl_beta))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_beta))
  )

  # ------------------------------------------------------------------------------
  # multinomial outcomes

  set.seed(100)
  suppressWarnings(
    mtnl_beta <- cal_estimate_beta(testthat_cal_multiclass())
  )
  expect_cal_type(mtnl_beta, "one_vs_all")
  expect_cal_method(mtnl_beta, "Beta calibration")
  expect_snapshot(print(mtnl_beta))

  expect_equal(
    testthat_cal_multiclass_count(),
    nrow(cal_apply(testthat_cal_multiclass(), mtnl_beta))
  )
})

test_that("Beta estimates errors - grouped_df", {
  skip_if_not_installed("betacal")
  expect_snapshot_error(
    cal_estimate_beta(dplyr::group_by(mtcars, vs))
  )
})
