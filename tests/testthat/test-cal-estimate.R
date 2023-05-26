# --------------------------------- Logistic -----------------------------------
test_that("Logistic estimates work - data.frame", {
  sl_logistic <- cal_estimate_logistic(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_logistic, "binary")
  expect_cal_method(sl_logistic, "Logistic regression")
  expect_cal_estimate(sl_logistic, "butchered_glm")
  expect_cal_rows(sl_logistic)
  expect_snapshot(print(sl_logistic))

  expect_snapshot_error(
    segment_logistic %>% cal_estimate_logistic(truth = Class, estimate = .pred_poor)
  )

  data(hpc_cv, package = "yardstick")
  expect_snapshot_error(
    hpc_cv %>% cal_estimate_logistic(truth = obs, estimate = c(VF:L))
  )

  sl_logistic_group <- segment_logistic %>%
    dplyr::mutate(group = .pred_poor > 0.5) %>%
    cal_estimate_logistic(Class, .by = group, smooth = FALSE)
  expect_false(are_groups_configs(sl_logistic_group))

  expect_cal_type(sl_logistic_group, "binary")
  expect_cal_method(sl_logistic_group, "Logistic regression")
  expect_cal_estimate(sl_logistic_group, "butchered_glm")
  expect_cal_rows(sl_logistic_group)
  expect_snapshot(print(sl_logistic_group))

  expect_snapshot_error(
    segment_logistic %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_logistic(Class, .by = c(group1, group2), smooth = FALSE)
  )

  lgst_configs <-
    bin_with_configs() %>%
    cal_estimate_logistic(truth = Class, smooth = FALSE)
  expect_true(are_groups_configs(lgst_configs))

})

test_that("Logistic estimates work - tune_results", {
  tl_logistic <- cal_estimate_logistic(testthat_cal_binary(), smooth = FALSE)
  expect_cal_type(tl_logistic, "binary")
  expect_cal_method(tl_logistic, "Logistic regression")
  expect_cal_estimate(tl_logistic, "butchered_glm")
  expect_snapshot(print(tl_logistic))
  expect_true(are_groups_configs(tl_logistic))

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
  expect_cal_method(sl_gam, "Generalized additive model")
  expect_cal_estimate(sl_gam, "butchered_gam")
  expect_cal_rows(sl_gam)
  expect_snapshot(print(sl_gam))

  sl_gam_group <- segment_logistic %>%
    dplyr::mutate(group = .pred_poor > 0.5) %>%
    cal_estimate_logistic(Class, .by = group)

  expect_cal_type(sl_gam_group, "binary")
  expect_cal_method(sl_gam_group, "Generalized additive model")
  expect_cal_estimate(sl_gam_group, "butchered_gam")
  expect_cal_rows(sl_gam_group)
  expect_snapshot(print(sl_gam_group))

  expect_snapshot_error(
    segment_logistic %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_logistic(Class, .by = c(group1, group2))
  )

  lgst_configs <-
    bin_with_configs() %>%
    cal_estimate_logistic(truth = Class, smooth = TRUE)
  expect_true(are_groups_configs(lgst_configs))
})

test_that("Logistic spline estimates work - tune_results", {
  tl_gam <- cal_estimate_logistic(testthat_cal_binary())
  expect_cal_type(tl_gam, "binary")
  expect_cal_method(tl_gam, "Generalized additive model")
  expect_cal_estimate(tl_gam, "butchered_gam")
  expect_snapshot(print(tl_gam))
  expect_true(are_groups_configs(tl_gam))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_gam))
  )
})

# --------------------------------- Isotonic -----------------------------------
test_that("Isotonic estimates work - data.frame", {
  set.seed(100)
  sl_isotonic <- cal_estimate_isotonic(segment_logistic, Class)
  expect_cal_type(sl_isotonic, "binary")
  expect_cal_method(sl_isotonic, "Isotonic regression")
  expect_cal_rows(sl_isotonic)
  expect_snapshot(print(sl_isotonic))

  sl_isotonic_group <- segment_logistic %>%
    dplyr::mutate(group = .pred_poor > 0.5) %>%
    cal_estimate_isotonic(Class, .by = group)

  expect_cal_type(sl_isotonic_group, "binary")
  expect_cal_method(sl_isotonic_group, "Isotonic regression")
  expect_cal_rows(sl_isotonic_group)
  expect_snapshot(print(sl_isotonic_group))

  expect_snapshot_error(
    segment_logistic %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_isotonic(Class, .by = c(group1, group2))
  )

  iso_configs <-
    bin_with_configs() %>%
    cal_estimate_isotonic(truth = Class)
  expect_true(are_groups_configs(iso_configs))

  mltm_configs <-
    mnl_with_configs() %>%
    cal_estimate_isotonic(truth = obs, estimate = c(VF:L))
  expect_true(are_groups_configs(mltm_configs))
})

test_that("Isotonic estimates work - tune_results", {
  set.seed(100)
  tl_isotonic <- cal_estimate_isotonic(testthat_cal_binary())
  expect_cal_type(tl_isotonic, "binary")
  expect_cal_method(tl_isotonic, "Isotonic regression")
  expect_snapshot(print(tl_isotonic))
  expect_true(are_groups_configs(tl_isotonic))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_isotonic))
  )

  # ------------------------------------------------------------------------------
  # multinomial outcomes

  set.seed(100)
  mtnl_isotonic <- cal_estimate_isotonic(testthat_cal_multiclass())
  expect_cal_type(mtnl_isotonic, "one_vs_all")
  expect_cal_method(mtnl_isotonic, "Isotonic regression")
  expect_snapshot(print(mtnl_isotonic))
  expect_true(are_groups_configs(mtnl_isotonic))

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
  sl_logistic <- cal_estimate_isotonic(boosting_predictions_oob, outcome, estimate = .pred)
  expect_cal_type(sl_logistic, "regression")
  expect_cal_method(sl_logistic, "Isotonic regression")
  expect_cal_rows(sl_logistic, 2000)
  expect_snapshot(print(sl_logistic))

  sl_logistic_group <- boosting_predictions_oob %>%
    cal_estimate_isotonic(outcome, estimate = .pred, .by = id)

  expect_cal_type(sl_logistic_group, "regression")
  expect_cal_method(sl_logistic_group, "Isotonic regression")
  expect_cal_rows(sl_logistic_group, 2000)
  expect_snapshot(print(sl_logistic_group))

  expect_snapshot_error(
    boosting_predictions_oob %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_isotonic(outcome, estimate = .pred, .by = c(group1, group2))
  )

  iso_configs <-
    reg_with_configs() %>%
    cal_estimate_isotonic(truth = solubility, estimate = prediction)
  expect_true(are_groups_configs(iso_configs))
})

# -------------------------- Isotonic Bootstrapped -----------------------------
test_that("Isotonic Bootstrapped estimates work - data.frame", {
  set.seed(1)
  sl_boot <- cal_estimate_isotonic_boot(segment_logistic, Class)
  expect_cal_type(sl_boot, "binary")
  expect_cal_method(sl_boot, "Bootstrapped isotonic regression")
  expect_snapshot(print(sl_boot))

  sl_boot_group <- segment_logistic %>%
    dplyr::mutate(group = .pred_poor > 0.5) %>%
    cal_estimate_isotonic_boot(Class, .by = group)

  expect_cal_type(sl_boot_group, "binary")
  expect_cal_method(sl_boot_group, "Bootstrapped isotonic regression")
  expect_snapshot(print(sl_boot_group))
  expect_false(are_groups_configs(sl_boot_group))

  expect_snapshot_error(
    segment_logistic %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_isotonic_boot(Class, .by = c(group1, group2))
  )

  isobt_configs <-
    bin_with_configs() %>%
    cal_estimate_isotonic_boot(truth = Class)
  expect_true(are_groups_configs(isobt_configs))

  mltm_configs <-
    mnl_with_configs() %>%
    cal_estimate_isotonic_boot(truth = obs, estimate = c(VF:L))
  expect_true(are_groups_configs(mltm_configs))

})

test_that("Isotonic Bootstrapped estimates work - tune_results", {
  set.seed(100)
  tl_isotonic <- cal_estimate_isotonic_boot(testthat_cal_binary())
  expect_cal_type(tl_isotonic, "binary")
  expect_cal_method(tl_isotonic, "Bootstrapped isotonic regression")
  expect_snapshot(print(tl_isotonic))
  expect_true(are_groups_configs(tl_isotonic))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_isotonic))
  )

  # ------------------------------------------------------------------------------
  # multinomial outcomes

  set.seed(100)
  mtnl_isotonic <- cal_estimate_isotonic_boot(testthat_cal_multiclass())
  expect_cal_type(mtnl_isotonic, "one_vs_all")
  expect_cal_method(mtnl_isotonic, "Bootstrapped isotonic regression")
  expect_snapshot(print(mtnl_isotonic))
  expect_true(are_groups_configs(mtnl_isotonic))

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
  sl_beta <- cal_estimate_beta(segment_logistic, Class, smooth = FALSE)
  expect_cal_type(sl_beta, "binary")
  expect_cal_method(sl_beta, "Beta calibration")
  expect_cal_rows(sl_beta)
  expect_snapshot(print(sl_beta))

  sl_beta_group <- segment_logistic %>%
    dplyr::mutate(group = .pred_poor > 0.5) %>%
    cal_estimate_beta(Class, smooth = FALSE, .by = group)

  expect_cal_type(sl_beta_group, "binary")
  expect_cal_method(sl_beta_group, "Beta calibration")
  expect_cal_rows(sl_beta_group)
  expect_snapshot(print(sl_beta_group))

  expect_snapshot_error(
    segment_logistic %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_beta(Class, smooth = FALSE, .by = c(group1, group2))
  )

  beta_configs <-
    bin_with_configs() %>%
    cal_estimate_beta(truth = Class)
  expect_true(are_groups_configs(beta_configs))

  mltm_configs <-
    mnl_with_configs() %>%
    cal_estimate_beta(truth = obs, estimate = c(VF:L))
  expect_true(are_groups_configs(mltm_configs))
})

test_that("Beta estimates work - tune_results", {
  tl_beta <- cal_estimate_beta(testthat_cal_binary())
  expect_cal_type(tl_beta, "binary")
  expect_cal_method(tl_beta, "Beta calibration")
  expect_snapshot(print(tl_beta))
  expect_true(are_groups_configs(tl_beta))

  expect_equal(
    testthat_cal_binary_count(),
    nrow(cal_apply(testthat_cal_binary(), tl_beta))
  )

  # ------------------------------------------------------------------------------
  # multinomial outcomes

  set.seed(100)
  suppressWarnings(
    mtnl_isotonic <- cal_estimate_beta(testthat_cal_multiclass())
  )
  expect_cal_type(mtnl_isotonic, "one_vs_all")
  expect_cal_method(mtnl_isotonic, "Beta calibration")
  expect_snapshot(print(mtnl_isotonic))
  expect_true(are_groups_configs(mtnl_isotonic))

  expect_equal(
    testthat_cal_multiclass_count(),
    nrow(cal_apply(testthat_cal_multiclass(), mtnl_isotonic))
  )
})

test_that("Beta estimates errors - grouped_df", {
  expect_snapshot_error(
    cal_estimate_beta(dplyr::group_by(mtcars, vs))
  )
})

# ------------------------------ Multinomial -----------------------------------
test_that("Multinomial estimates work - data.frame", {
  sp_multi <- cal_estimate_multinomial(species_probs, Species, smooth = FALSE)
  expect_cal_type(sp_multi, "multiclass")
  expect_cal_method(sp_multi, "Multinomial regression")
  expect_cal_rows(sp_multi, n = 110)
  expect_snapshot(print(sp_multi))

  sp_smth_multi <- cal_estimate_multinomial(species_probs, Species, smooth = TRUE)
  expect_cal_type(sp_smth_multi, "multiclass")
  expect_cal_method(sp_smth_multi, "Generalized additive model")
  expect_cal_rows(sp_smth_multi, n = 110)
  expect_snapshot(print(sp_smth_multi))

  sl_multi_group <- species_probs %>%
    dplyr::mutate(group = .pred_bobcat > 0.5) %>%
    cal_estimate_multinomial(Species, smooth = FALSE, .by = group)

  expect_cal_type(sl_multi_group, "multiclass")
  expect_cal_method(sl_multi_group, "Multinomial regression")
  expect_cal_rows(sl_multi_group, n = 110)
  expect_snapshot(print(sl_multi_group))

  expect_snapshot_error(
    species_probs %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_multinomial(Species, smooth = FALSE, .by = c(group1, group2))
  )

  mltm_configs <-
    mnl_with_configs() %>%
    cal_estimate_multinomial(truth = obs, estimate = c(VF:L), smooth = FALSE)
  expect_true(are_groups_configs(mltm_configs))
})

test_that("Multinomial estimates work - tune_results", {
  tl_multi <- cal_estimate_multinomial(testthat_cal_multiclass(), smooth = FALSE)
  expect_cal_type(tl_multi, "multiclass")
  expect_cal_method(tl_multi, "Multinomial regression")
  expect_snapshot(print(tl_multi))
  expect_true(are_groups_configs(tl_multi))

  expect_equal(
    testthat_cal_multiclass() %>%
      tune::collect_predictions(summarize = TRUE) %>%
      nrow(),
    testthat_cal_multiclass() %>%
      cal_apply(tl_multi) %>%
      nrow()
  )

  tl_smth_multi <- cal_estimate_multinomial(testthat_cal_multiclass(), smooth = TRUE)
  expect_cal_type(tl_smth_multi, "multiclass")
  expect_cal_method(tl_smth_multi, "Generalized additive model")
  expect_snapshot(print(tl_smth_multi))

  expect_equal(
    testthat_cal_multiclass() %>%
      tune::collect_predictions(summarize = TRUE) %>%
      nrow(),
    testthat_cal_multiclass() %>%
      cal_apply(tl_smth_multi) %>%
      nrow()
  )
})

test_that("Multinomial estimates errors - grouped_df", {
  expect_snapshot_error(
    cal_estimate_multinomial(dplyr::group_by(mtcars, vs))
  )
})

test_that("Passing a binary outcome causes error", {
  expect_error(
    cal_estimate_multinomial(segment_logistic, Class)
  )
})

# --------------------------------- Linear -----------------------------------
test_that("Linear estimates work - data.frame", {
  sl_logistic <- cal_estimate_linear(boosting_predictions_oob, outcome, smooth = FALSE)
  expect_cal_type(sl_logistic, "regression")
  expect_cal_method(sl_logistic, "Linear")
  expect_cal_estimate(sl_logistic, "butchered_glm")
  expect_cal_rows(sl_logistic, 2000)
  expect_snapshot(print(sl_logistic))
  expect_false(are_groups_configs(sl_logistic))

  sl_logistic_group <- boosting_predictions_oob %>%
    dplyr::mutate(group = .pred > 0.5) %>%
    cal_estimate_linear(outcome, smooth = FALSE, .by = group)

  expect_cal_type(sl_logistic_group, "regression")
  expect_cal_method(sl_logistic_group, "Linear")
  expect_cal_estimate(sl_logistic_group, "butchered_glm")
  expect_cal_rows(sl_logistic_group, 2000)
  expect_snapshot(print(sl_logistic_group))

  expect_snapshot_error(
    boosting_predictions_oob %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_linear(outcome, smooth = FALSE, .by = c(group1, group2))
  )

  lin_configs <-
    reg_with_configs() %>%
    cal_estimate_linear(truth = solubility, estimate = prediction, smooth = FALSE)
  expect_true(are_groups_configs(lin_configs))
})

test_that("Linear estimates work - tune_results", {
  tl_linear <- cal_estimate_linear(testthat_cal_reg(), outcome, smooth = FALSE)
  expect_cal_type(tl_linear, "regression")
  expect_cal_method(tl_linear, "Linear")
  expect_cal_estimate(tl_linear, "butchered_glm")
  expect_snapshot(print(tl_linear))
  expect_true(are_groups_configs(tl_linear))
})

test_that("Linear estimates errors - grouped_df", {
  expect_snapshot_error(
    cal_estimate_linear(dplyr::group_by(mtcars, vs))
  )
})

# ----------------------------- Linear Spline --------------------------------
test_that("Linear spline estimates work - data.frame", {
  sl_gam <- cal_estimate_linear(boosting_predictions_oob, outcome)
  expect_cal_type(sl_gam, "regression")
  expect_cal_method(sl_gam, "Generalized additive model")
  expect_cal_estimate(sl_gam, "butchered_gam")
  expect_cal_rows(sl_gam, 2000)
  expect_snapshot(print(sl_gam))

  sl_gam_group <- boosting_predictions_oob %>%
    dplyr::mutate(group = .pred > 0.5) %>%
    cal_estimate_linear(outcome, .by = group)

  expect_cal_type(sl_gam_group, "regression")
  expect_cal_method(sl_gam_group, "Generalized additive model")
  expect_cal_estimate(sl_gam_group, "butchered_gam")
  expect_cal_rows(sl_gam_group, 2000)
  expect_snapshot(print(sl_gam_group))

  expect_snapshot_error(
    boosting_predictions_oob %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_estimate_linear(outcome, .by = c(group1, group2))
  )

  lin_configs <-
    reg_with_configs() %>%
    cal_estimate_linear(truth = solubility, estimate = prediction, smooth = TRUE)
  expect_true(are_groups_configs(lin_configs))
})

test_that("Linear spline estimates work - tune_results", {
  tl_gam <- cal_estimate_linear(testthat_cal_reg(), outcome)
  expect_cal_type(tl_gam, "regression")
  expect_cal_method(tl_gam, "Generalized additive model")
  expect_cal_estimate(tl_gam, "butchered_gam")
  expect_snapshot(print(tl_gam))
  expect_true(are_groups_configs(tl_gam))

  expect_equal(
    testthat_cal_reg_count(),
    nrow(cal_apply(testthat_cal_reg(), tl_gam))
  )
})


# ----------------------------------- Other ------------------------------------
test_that("Non-default names used for estimate columns", {
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
