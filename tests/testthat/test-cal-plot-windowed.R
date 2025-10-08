
test_that("Binary windowed functions work", {
  skip_if_not_installed("modeldata")

  x30 <- .cal_table_windowed(
    segment_logistic,
    truth = Class,
    estimate = .pred_good,
    step_size = 0.11,
    window_size = 0.10
  )

  x30_1 <- segment_logistic |>
    dplyr::mutate(x = dplyr::case_when(
      .pred_good <= 0.05 ~ 1,
      .pred_good >= 0.06 & .pred_good <= 0.16 ~ 2,
      .pred_good >= 0.17 & .pred_good <= 0.27 ~ 3,
      .pred_good >= 0.28 & .pred_good <= 0.38 ~ 4,
      .pred_good >= 0.39 & .pred_good <= 0.49 ~ 5,
      .pred_good >= 0.50 & .pred_good <= 0.60 ~ 6,
      .pred_good >= 0.61 & .pred_good <= 0.71 ~ 7,
      .pred_good >= 0.72 & .pred_good <= 0.82 ~ 8,
      .pred_good >= 0.83 & .pred_good <= 0.93 ~ 9,
      .pred_good >= 0.94 & .pred_good <= 1 ~ 10,
    )) |>
    dplyr::filter(!is.na(x)) |>
    dplyr::count(x)

  expect_equal(
    x30$total,
    x30_1$n
  )

  x31 <- cal_plot_windowed(segment_logistic, Class, .pred_good)

  expect_s3_class(x31, "ggplot")
  expect_false(has_facet(x31))

  x32 <- .cal_table_windowed(
    testthat_cal_binary(),
    step_size = 0.11,
    window_size = 0.10
  )

  x32_1 <- testthat_cal_binary() |>
    tune::collect_predictions(summarize = TRUE) |>
    dplyr::mutate(x = dplyr::case_when(
      .pred_class_1 <= 0.05 ~ 1,
      .pred_class_1 >= 0.06 & .pred_class_1 <= 0.16 ~ 2,
      .pred_class_1 >= 0.17 & .pred_class_1 <= 0.27 ~ 3,
      .pred_class_1 >= 0.28 & .pred_class_1 <= 0.38 ~ 4,
      .pred_class_1 >= 0.39 & .pred_class_1 <= 0.49 ~ 5,
      .pred_class_1 >= 0.50 & .pred_class_1 <= 0.60 ~ 6,
      .pred_class_1 >= 0.61 & .pred_class_1 <= 0.71 ~ 7,
      .pred_class_1 >= 0.72 & .pred_class_1 <= 0.82 ~ 8,
      .pred_class_1 >= 0.83 & .pred_class_1 <= 0.93 ~ 9,
      .pred_class_1 >= 0.94 & .pred_class_1 <= 1 ~ 10,
    )) |>
    dplyr::filter(!is.na(x)) |>
    dplyr::count(.config, x)

  expect_equal(
    x32$total,
    x32_1$n
  )

  x33 <- cal_plot_windowed(testthat_cal_binary())

  expect_s3_class(x33, "ggplot")
  expect_true(has_facet(x33))

  win_configs <-
    bin_with_configs() |> cal_plot_windowed(truth = Class, estimate = .pred_good)
  expect_true(has_facet(win_configs))


  # ------------------------------------------------------------------------------
  # multinomial outcome, binary windowed plots

  multi_configs_from_tune <-
    testthat_cal_multiclass() |> cal_plot_windowed()
  expect_s3_class(multi_configs_from_tune, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_tune$facet, "FacetGrid"))


  multi_configs_from_df <-
    mnl_with_configs() |> cal_plot_windowed(truth = obs, estimate = c(VF:L))
  expect_s3_class(multi_configs_from_df, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_df$facet, "FacetGrid"))
})

test_that("windowed plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_windowed(dplyr::group_by(mtcars, vs))
  )
})

test_that("don't facet if there is only one .config", {
  class_data <- testthat_cal_binary()

  class_data$.predictions <- lapply(
    class_data$.predictions,
    function(x) dplyr::filter(x, .config == "Preprocessor1_Model1")
  )

  res_windowed <- cal_plot_windowed(class_data)

  expect_null(res_windowed$data[[".config"]])
  expect_s3_class(res_windowed, "ggplot")
})


test_that("Groupings that may not match work", {
  model <- glm(Class ~ .pred_good, segment_logistic, family = "binomial")

  preds <- 1 - predict(model, segment_logistic, type = "response")

  combined <- dplyr::bind_rows(
    dplyr::mutate(segment_logistic, source = "original"),
    dplyr::mutate(segment_logistic, .pred_good = preds, source = "glm")
  )

  x51 <- combined |>
    dplyr::group_by(source) |>
    .cal_table_windowed(
      truth = Class,
      estimate = .pred_good,
      step_size = 0.11,
      window_size = 0.10
    )

  x51_1 <- combined |>
    dplyr::mutate(x = dplyr::case_when(
      .pred_good <= 0.05 ~ 1,
      .pred_good >= 0.06 & .pred_good <= 0.16 ~ 2,
      .pred_good >= 0.17 & .pred_good <= 0.27 ~ 3,
      .pred_good >= 0.28 & .pred_good <= 0.38 ~ 4,
      .pred_good >= 0.39 & .pred_good <= 0.49 ~ 5,
      .pred_good >= 0.50 & .pred_good <= 0.60 ~ 6,
      .pred_good >= 0.61 & .pred_good <= 0.71 ~ 7,
      .pred_good >= 0.72 & .pred_good <= 0.82 ~ 8,
      .pred_good >= 0.83 & .pred_good <= 0.93 ~ 9,
      .pred_good >= 0.94 & .pred_good <= 1 ~ 10,
    )) |>
    dplyr::filter(!is.na(x)) |>
    dplyr::count(source, x)

  expect_equal(
    x51$total,
    x51_1$n
  )
})

test_that("Groups are respected", {
  preds <- segment_logistic |>
    dplyr::mutate(source = "logistic") |>
    dplyr::bind_rows(segment_naive_bayes) |>
    dplyr::mutate(source = ifelse(is.na(source), "nb", source)) |>
    dplyr::group_by(source)

  x42 <- .cal_table_windowed(preds, Class, .pred_good)

  expect_equal(as.integer(table(x42$source)), c(21, 21))

  expect_equal(unique(x42$source), c("logistic", "nb"))
})
