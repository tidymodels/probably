test_that("Binary breaks functions work", {
  x10 <- .cal_table_breaks(segment_logistic, Class, .pred_good, event_level = "first")

  expect_equal(
    x10$predicted_midpoint,
    seq(0.05, 0.95, by = 0.10)
  )

  expect_s3_class(
    cal_plot_breaks(segment_logistic, Class, .pred_good),
    "ggplot"
  )

  x11 <- .cal_table_breaks(testthat_cal_binary())

  expect_equal(
    x11$predicted_midpoint,
    rep(seq(0.05, 0.95, by = 0.10), times = 8)
  )

  expect_s3_class(
    cal_plot_breaks(testthat_cal_binary()),
    "ggplot"
  )

  brks_configs <-
    bin_with_configs() |> cal_plot_breaks(truth = Class, estimate = .pred_good)
  expect_true(has_facet(brks_configs))
})


test_that("Binary breaks functions work with group argument", {
  skip_if_not_installed("ggplot2", minimum_version = "3.5.2.9000")
  res <- segment_logistic |>
    dplyr::mutate(id = dplyr::row_number() %% 2) |>
    cal_plot_breaks(Class, .pred_good, .by = id)

  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0,],
    dplyr::tibble(
      id = factor(0, levels = paste(0:1)),
      predicted_midpoint = double(), event_rate = double(), events = double(),
      total = integer(), lower = double(), upper = double()
    )
  )

  expect_equal(
    rlang::expr_text(res$mapping$x),
    "~predicted_midpoint"
  )
  expect_equal(
    rlang::expr_text(res$mapping$colour),
    "~id"
  )
  expect_equal(
    rlang::expr_text(res$mapping$fill),
    "~id"
  )

  expect_snapshot(get_labs(res))

  expect_equal(length(res$layers), 4)

  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_plot_breaks(Class, .pred_good, .by = c(group1, group2))
  )
})

test_that("Multi-class breaks functions work", {
  skip_if_not_installed("modeldata")

  x10 <- .cal_table_breaks(species_probs, Species, dplyr::starts_with(".pred"))

  expect_equal(
    x10$predicted_midpoint,
    rep(seq(0.05, 0.95, by = 0.10), times = 3)
  )

  expect_s3_class(
    cal_plot_breaks(species_probs, Species),
    "ggplot"
  )

  x11 <- .cal_table_breaks(testthat_cal_multiclass())

  expect_equal(
    sort(unique(x11$predicted_midpoint)),
    seq(0.05, 0.95, by = 0.10)
  )

  multi_configs <- cal_plot_breaks(testthat_cal_multiclass())
  # should be faceted by .config and class
  expect_s3_class(multi_configs, "ggplot")
  expect_true(inherits(multi_configs$facet, "FacetGrid"))

  expect_error(
    cal_plot_breaks(species_probs, Species, event_level = "second")
  )

  # ------------------------------------------------------------------------------
  # multinomial outcome, binary logistic plots

  multi_configs_from_tune <-
    testthat_cal_multiclass() |> cal_plot_breaks()
  expect_s3_class(multi_configs_from_tune, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_tune$facet, "FacetGrid"))

  multi_configs_from_df <-
    mnl_with_configs() |> cal_plot_breaks(truth = obs, estimate = c(VF:L))
  expect_s3_class(multi_configs_from_df, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_df$facet, "FacetGrid"))
})


test_that("breaks plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_breaks(dplyr::group_by(mtcars, vs))
  )
})

test_that("Numeric groups are supported", {
  grp_df <- segment_logistic
  grp_df$num_group <- rep(c(1, 2), times = 505)

  p <- grp_df |>
    cal_plot_breaks(Class, .pred_good, .by = num_group)

  expect_s3_class(p, "ggplot")
})

test_that("Some general exceptions", {
  expect_error(
    .cal_table_breaks(tune::ames_grid_search),
    "The `tune_results` object does not contain columns with predictions"
  )
  expect_warning(
    cal_plot_breaks(segment_logistic, Class),
  )
})

test_that("don't facet if there is only one .config", {
  class_data <- testthat_cal_binary()

  class_data$.predictions <- lapply(
    class_data$.predictions,
    function(x) dplyr::filter(x, .config == "Preprocessor1_Model1")
  )

  res_breaks <- cal_plot_breaks(class_data)

  expect_null(res_breaks$data[[".config"]])
  expect_s3_class(res_breaks, "ggplot")
})

test_that("custom names for cal_plot_breaks()", {
  data(segment_logistic)
  segment_logistic_1 <- dplyr::rename(segment_logistic, good_prob = .pred_good)
  p <- cal_plot_breaks(segment_logistic_1, Class, good_prob)
  expect_s3_class(p, "ggplot")
})

test_that("Event level handling works", {
  x7 <- .cal_table_breaks(segment_logistic, Class, .pred_good, event_level = "second")
  expect_equal(
    which(x7$predicted_midpoint == min(x7$predicted_midpoint)),
    which(x7$event_rate == max(x7$event_rate))
  )

  expect_snapshot_error(
    .cal_table_breaks(segment_logistic, Class, .pred_good, event_level = "invalid")
  )
})

test_that("Groupings that may not match work", {
  model <- glm(Class ~ .pred_good, segment_logistic, family = "binomial")

  preds <- 1 - predict(model, segment_logistic, type = "response")

  combined <- dplyr::bind_rows(
    dplyr::mutate(segment_logistic, source = "original"),
    dplyr::mutate(segment_logistic, .pred_good = preds, source = "glm")
  )

  x50 <- combined |>
    dplyr::group_by(source) |>
    .cal_table_breaks(Class, .pred_good)

  expect_equal(
    unique(x50$predicted_midpoint),
    seq(0.05, 0.95, by = 0.10)
  )
})

test_that("Groups are respected", {
  preds <- segment_logistic |>
    dplyr::mutate(source = "logistic") |>
    dplyr::bind_rows(segment_naive_bayes) |>
    dplyr::mutate(source = ifelse(is.na(source), "nb", source)) |>
    dplyr::group_by(source)

  x40 <- .cal_table_breaks(preds, Class, .pred_good)

  expect_equal(as.integer(table(x40$source)), c(10, 10))

  expect_equal(unique(x40$source), c("logistic", "nb"))
})
