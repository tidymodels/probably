test_that("Logistic estimates work - data.frame", {
  skip_if_not_installed("modeldata")

  sl_logistic <- cal_estimate_logistic(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_logistic, "binary")
  expect_cal_method(sl_logistic, "Logistic regression calibration")
  expect_cal_estimate(sl_logistic, "butchered_glm")
  expect_cal_rows(sl_logistic)
  expect_snapshot(print(sl_logistic))

  expect_snapshot_error(
    segment_logistic |> cal_estimate_logistic(truth = Class, estimate = .pred_poor)
  )

  data(hpc_cv, package = "yardstick")
  expect_snapshot_error(
    modeldata::hpc_cv |> cal_estimate_logistic(truth = obs, estimate = c(VF:L))
  )

  sl_logistic_group <- segment_logistic |>
    dplyr::mutate(group = .pred_poor > 0.5) |>
    cal_estimate_logistic(Class, .by = group, smooth = FALSE)

  expect_cal_type(sl_logistic_group, "binary")
  expect_cal_method(sl_logistic_group, "Logistic regression calibration")
  expect_cal_estimate(sl_logistic_group, "butchered_glm")
  expect_cal_rows(sl_logistic_group)
  expect_snapshot(print(sl_logistic_group))

  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_logistic(Class, .by = c(group1, group2), smooth = FALSE)
  )

  # ------------------------------------------------------------------------------

  data(two_class_example, package = "modeldata")
  two_cls_plist <- two_class_example[0,]
  two_cls_mod <-
    cal_estimate_logistic(two_class_example, truth = truth, estimate = c(Class1, Class2))

  two_cls_res <- cal_apply(two_class_example, two_cls_mod, pred_class = predicted)
  expect_equal(two_cls_res[0,], two_cls_plist)

})

test_that("Logistic estimates work - tune_results", {
  skip_if_not_installed("modeldata")

  tl_logistic <- cal_estimate_logistic(testthat_cal_binary(), smooth = FALSE)
  expect_cal_type(tl_logistic, "binary")
  expect_cal_method(tl_logistic, "Logistic regression calibration")
  expect_cal_estimate(tl_logistic, "butchered_glm")
  expect_snapshot(print(tl_logistic))

  expect_snapshot_error(
    cal_estimate_logistic(testthat_cal_multiclass(), smooth = FALSE)
  )
})

test_that("Logistic estimates errors - grouped_df", {
  expect_snapshot_error(
    cal_estimate_logistic(dplyr::group_by(mtcars, vs), smooth = FALSE)
  )
})

# ----------------------------- Logistic Spline --------------------------------
test_that("Logistic spline estimates work - data.frame", {
  sl_gam <- cal_estimate_logistic(segment_logistic, Class)
  expect_cal_type(sl_gam, "binary")
  expect_cal_method(sl_gam, "Generalized additive model calibration")
  expect_cal_estimate(sl_gam, "butchered_gam")
  expect_cal_rows(sl_gam)
  expect_snapshot(print(sl_gam))

  sl_gam_group <- segment_logistic |>
    dplyr::mutate(group = .pred_poor > 0.5) |>
    cal_estimate_logistic(Class, .by = group)

  expect_cal_type(sl_gam_group, "binary")
  expect_cal_method(sl_gam_group, "Generalized additive model calibration")
  expect_cal_estimate(sl_gam_group, "butchered_gam")
  expect_cal_rows(sl_gam_group)
  expect_snapshot(print(sl_gam_group))

  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_logistic(Class, .by = c(group1, group2))
  )
})

test_that("Logistic spline estimates work - tune_results", {
  skip_if_not_installed("modeldata")

  tl_gam <- cal_estimate_logistic(testthat_cal_binary())
  expect_cal_type(tl_gam, "binary")
  expect_cal_method(tl_gam, "Generalized additive model calibration")
  expect_cal_estimate(tl_gam, "butchered_gam")
  expect_snapshot(print(tl_gam))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_gam))
  )
})

test_that("Logistic spline switches to linear if too few unique", {
  skip_if_not_installed("modeldata")

  segment_logistic$.pred_good <- rep(
    x = 1,
    length.out = nrow(segment_logistic)
  )

  expect_snapshot(
    sl_gam <- cal_estimate_logistic(segment_logistic, Class, smooth = TRUE)
  )
  sl_lm <- cal_estimate_logistic(segment_logistic, Class, smooth = FALSE)

  expect_identical(
    sl_gam$estimates,
    sl_lm$estimates
  )

  segment_logistic$id <- rep(
    x = 1:2,
    length.out = nrow(segment_logistic)
  )
  expect_snapshot(
    sl_gam <- cal_estimate_logistic(segment_logistic, Class, .by = id, smooth = TRUE)
  )
  sl_lm <- cal_estimate_logistic(segment_logistic, Class, .by = id, smooth = FALSE)

  expect_identical(
    sl_gam$estimates,
    sl_lm$estimates
  )
})
