test_that("Multinomial estimates work - data.frame", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("nnet")

  sp_multi <- cal_estimate_multinomial(species_probs, Species, smooth = FALSE)
  expect_cal_type(sp_multi, "multiclass")
  expect_cal_method(sp_multi, "Multinomial regression calibration")
  expect_cal_rows(sp_multi, n = 110)
  expect_snapshot(print(sp_multi))

  sp_smth_multi <- cal_estimate_multinomial(species_probs, Species, smooth = TRUE)
  expect_cal_type(sp_smth_multi, "multiclass")
  expect_cal_method(sp_smth_multi, "Generalized additive model calibration")
  expect_cal_rows(sp_smth_multi, n = 110)
  expect_snapshot(print(sp_smth_multi))

  sl_multi_group <- species_probs |>
    dplyr::mutate(group = .pred_bobcat > 0.5) |>
    cal_estimate_multinomial(Species, smooth = FALSE, .by = group)

  expect_cal_type(sl_multi_group, "multiclass")
  expect_cal_method(sl_multi_group, "Multinomial regression calibration")
  expect_cal_rows(sl_multi_group, n = 110)
  expect_snapshot(print(sl_multi_group))

  expect_snapshot_error(
    species_probs |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_estimate_multinomial(Species, smooth = FALSE, .by = c(group1, group2))
  )

  mltm_configs <-
    mnl_with_configs() |>
    cal_estimate_multinomial(truth = obs, estimate = c(VF:L), smooth = FALSE)
})

test_that("Multinomial estimates work - tune_results", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("nnet")

  tl_multi <- cal_estimate_multinomial(testthat_cal_multiclass(), smooth = FALSE)
  expect_cal_type(tl_multi, "multiclass")
  expect_cal_method(tl_multi, "Multinomial regression calibration")
  expect_snapshot(print(tl_multi))

  expect_equal(
    testthat_cal_multiclass() |>
      tune::collect_predictions(summarize = TRUE) |>
      nrow(),
    testthat_cal_multiclass() |>
      cal_apply(tl_multi) |>
      nrow()
  )

  tl_smth_multi <- cal_estimate_multinomial(testthat_cal_multiclass(), smooth = TRUE)
  expect_cal_type(tl_smth_multi, "multiclass")
  expect_cal_method(tl_smth_multi, "Generalized additive model calibration")
  expect_snapshot(print(tl_smth_multi))

  expect_equal(
    testthat_cal_multiclass() |>
      tune::collect_predictions(summarize = TRUE) |>
      nrow(),
    testthat_cal_multiclass() |>
      cal_apply(tl_smth_multi) |>
      nrow()
  )
})

test_that("Multinomial estimates errors - grouped_df", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("nnet")

  expect_snapshot_error(
    cal_estimate_multinomial(dplyr::group_by(mtcars, vs))
  )
})

test_that("Passing a binary outcome causes error", {
  expect_error(
    cal_estimate_multinomial(segment_logistic, Class)
  )
})

test_that("Multinomial spline switches to linear if too few unique", {
  skip_if_not_installed("modeldata")

  smol_species_probs <-
    species_probs |>
    dplyr::slice_head(n = 2, by = Species)

  expect_snapshot(
    sl_gam <- cal_estimate_multinomial(smol_species_probs, Species, smooth = TRUE)
  )
  sl_glm <- cal_estimate_multinomial(smol_species_probs, Species, smooth = FALSE)

  expect_identical(
    sl_gam$estimates,
    sl_glm$estimates
  )

  smol_by_species_probs <-
    species_probs |>
    dplyr::slice_head(n = 4, by = Species) |>
    dplyr::mutate(id = rep(1:2, 6))

  expect_snapshot(
    sl_gam <- cal_estimate_multinomial(smol_by_species_probs, Species, .by = id, smooth = TRUE)
  )
  sl_glm <- cal_estimate_multinomial(smol_by_species_probs, Species, .by = id, smooth = FALSE)

  expect_identical(
    sl_gam$estimates,
    sl_glm$estimates
  )
})
