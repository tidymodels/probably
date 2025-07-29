# --------------------------------- Logistic -----------------------------------
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
  expect_equal(
    required_pkgs(sl_logistic_group),
    "probably"
  )

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
  expect_equal(
    required_pkgs(two_cls_mod),
    c("mgcv", "probably")
  )

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
    required_pkgs(tl_gam),
    c("mgcv", "probably")
  )

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
    sl_gam$estimates[[1]]$estimate[[1]],
    sl_lm$estimates[[1]]$estimate[[1]]
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
    sl_gam$estimates[[1]]$estimate[[1]],
    sl_lm$estimates[[1]]$estimate[[1]]
  )
})

# --------------------------------- Isotonic -----------------------------------
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

# ----------------------------------- Beta -------------------------------------
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

# ------------------------------ Multinomial -----------------------------------
test_that("Multinomial estimates work - data.frame", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("nnet")

  sp_multi <- cal_estimate_multinomial(species_probs, Species, smooth = FALSE)
  expect_cal_type(sp_multi, "multiclass")
  expect_cal_method(sp_multi, "Multinomial regression calibration")
  expect_cal_rows(sp_multi, n = 110)
  expect_snapshot(print(sp_multi))
  expect_equal(
    required_pkgs(sp_multi),
    c("nnet", "probably")
  )

  sp_smth_multi <- cal_estimate_multinomial(species_probs, Species, smooth = TRUE)
  expect_cal_type(sp_smth_multi, "multiclass")
  expect_cal_method(sp_smth_multi, "Generalized additive model calibration")
  expect_cal_rows(sp_smth_multi, n = 110)
  expect_snapshot(print(sp_smth_multi))
  expect_equal(
    required_pkgs(sp_smth_multi),
    c("mgcv", "probably")
  )

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

# --------------------------------- Linear -----------------------------------
test_that("Linear estimates work - data.frame", {
  skip_if_not_installed("modeldata")

  sl_linear <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = FALSE)
  expect_cal_type(sl_linear, "regression")
  expect_cal_method(sl_linear, "Linear calibration")
  expect_cal_estimate(sl_linear, "butchered_glm")
  expect_cal_rows(sl_linear, 2000)
  expect_snapshot(print(sl_linear))
  expect_equal(
    required_pkgs(sl_linear),
    c("probably")
  )

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
  expect_equal(
    required_pkgs(sl_gam),
    c("mgcv", "probably")
  )

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
    sl_gam$estimates[[1]]$estimate[[1]],
    sl_lm$estimates[[1]]$estimate[[1]]
  )

  expect_snapshot(
    sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id, smooth = TRUE)
  )
  sl_lm <- cal_estimate_linear(boosting_predictions_oob, outcome, .by = id, smooth = FALSE)

  expect_identical(
    sl_gam$estimates[[1]]$estimate[[1]],
    sl_lm$estimates[[1]]$estimate[[1]]
  )
})

# ----------------------------------- Other ------------------------------------
test_that("Non-default names used for estimate columns", {
  skip_if_not_installed("modeldata")

  new_segment <- segment_logistic
  colnames(new_segment) <- c("poor", "good", "Class")

  set.seed(100)
  expect_snapshot(
    cal_estimate_isotonic(new_segment, Class, c(good, poor))
  )
})

test_that("Test exceptions", {
  expect_error(
    cal_estimate_isotonic(segment_logistic, Class, dplyr::starts_with("bad"))
  )
})

test_that("non-standard column names", {
  library(dplyr)
  # issue 145
  seg <- segment_logistic |>
    rename_with(~ paste0(.x, "-1"), matches(".pred")) |>
    mutate(
      Class = paste0(Class,"-1"),
      Class = factor(Class),
      .pred_class = ifelse(`.pred_poor-1` >= 0.5, "poor-1", "good-1")
    )
  calib <- cal_estimate_isotonic(seg, Class)
  new_pred <- cal_apply(seg, calib, pred_class = .pred_class)
  expect_named(new_pred, c(".pred_poor-1", ".pred_good-1", "Class", ".pred_class"))

})
