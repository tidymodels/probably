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
    bin_with_configs() %>% cal_plot_breaks(truth = Class, estimate = .pred_good)
  expect_true(has_facet(brks_configs))
})

test_that("Binary breaks functions work with group argument", {
  res <- segment_logistic %>%
    dplyr::mutate(id = dplyr::row_number() %% 2) %>%
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
  expect_equal(
    res$labels,
    list(x = "Bin Midpoint", y = "Event Rate", colour = "id", fill = "id",
         intercept = "intercept", slope = "slope", ymin = "lower",
         ymax = "upper")
  )
  expect_equal(length(res$layers), 4)

  expect_snapshot_error(
    segment_logistic %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_plot_breaks(Class, .pred_good, .by = c(group1, group2))
  )
})

test_that("Multi-class breaks functions work", {
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
    testthat_cal_multiclass() %>% cal_plot_breaks()
  expect_s3_class(multi_configs_from_tune, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_tune$facet, "FacetGrid"))

  multi_configs_from_df <-
    mnl_with_configs() %>% cal_plot_breaks(truth = obs, estimate = c(VF:L))
  expect_s3_class(multi_configs_from_df, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_df$facet, "FacetGrid"))
})

test_that("breaks plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_breaks(dplyr::group_by(mtcars, vs))
  )
})

test_that("Binary logistic functions work", {
  x20 <- .cal_table_logistic(segment_logistic, Class, .pred_good)

  model20 <- mgcv::gam(Class ~ s(.pred_good, k = 10),
    data = segment_logistic,
    family = binomial()
  )

  preds20 <- predict(model20,
    data.frame(.pred_good = seq(0, 1, by = .01)),
    type = "response"
  )

  expect_equal(sd(x20$prob), sd(preds20), tolerance = 0.000001)
  expect_equal(mean(x20$prob), mean(1 - preds20), tolerance = 0.000001)

  x21 <- cal_plot_logistic(segment_logistic, Class, .pred_good)

  expect_s3_class(x21, "ggplot")
  expect_false(has_facet(x21))

  x22 <- .cal_table_logistic(testthat_cal_binary())


  x22_1 <- testthat_cal_binary() %>%
    tune::collect_predictions(summarize = TRUE) %>%
    dplyr::group_by(.config) %>%
    dplyr::group_map(~ {
      model <- mgcv::gam(
        class ~ s(.pred_class_1, k = 10),
        data = .x,
        family = binomial()
      )
      preds <- predict(model,
        data.frame(.pred_class_1 = seq(0, 1, by = .01)),
        type = "response"
      )
      1 - preds
    }) %>%
    purrr::reduce(c)

  expect_equal(sd(x22$prob), sd(x22_1), tolerance = 0.000001)
  expect_equal(mean(x22$prob), mean(x22_1), tolerance = 0.000001)

  x23 <- cal_plot_logistic(testthat_cal_binary())

  expect_s3_class(x23, "ggplot")
  expect_true(has_facet(x23))

  x24 <- .cal_table_logistic(segment_logistic, Class, .pred_good, smooth = FALSE)

  model24 <- stats::glm(Class ~ .pred_good, data = segment_logistic, family = binomial())

  preds24 <- predict(model24,
    data.frame(.pred_good = seq(0, 1, by = .01)),
    type = "response"
  )

  expect_equal(sd(x24$prob), sd(preds24), tolerance = 0.000001)
  expect_equal(mean(x24$prob), mean(1 - preds24), tolerance = 0.000001)

  x25 <- .cal_table_logistic(
    segment_logistic,
    Class,
    .pred_poor,
    event_level = "second"
  )

  expect_equal(
    which(x25$prob == max(x25$prob)),
    nrow(x25)
  )

  lgst_configs <-
    bin_with_configs() %>% cal_plot_logistic(truth = Class, estimate = .pred_good)
  expect_true(has_facet(lgst_configs))

  # ------------------------------------------------------------------------------
  # multinomial outcome, binary logistic plots

  multi_configs_from_tune <-
    testthat_cal_multiclass() %>% cal_plot_logistic(smooth = FALSE)
  expect_s3_class(multi_configs_from_tune, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_tune$facet, "FacetGrid"))


  multi_configs_from_df <-
    mnl_with_configs() %>% cal_plot_logistic(truth = obs, estimate = c(VF:L))
  expect_s3_class(multi_configs_from_df, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_df$facet, "FacetGrid"))
})

test_that("Binary logistic functions work with group argument", {
  res <- segment_logistic %>%
    dplyr::mutate(id = dplyr::row_number() %% 2) %>%
    cal_plot_logistic(Class, .pred_good, .by = id)

  expect_s3_class(
    res,
    "ggplot"
  )
  expect_true(has_facet(res))

  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0,],
    dplyr::tibble(
      id = factor(0, levels = paste(0:1)),
      estimate = double(), prob = double(), lower = double(), upper = double()
    )
  )

  expect_equal(
    rlang::expr_text(res$mapping$x),
    "~estimate"
  )
  expect_equal(
    rlang::expr_text(res$mapping$colour),
    "~id"
  )
  expect_equal(
    rlang::expr_text(res$mapping$fill),
    "~id"
  )
  expect_equal(
    res$labels,
    list(x = "Probability", y = "Predicted Event Rate", colour = "id",
         fill = "id", intercept = "intercept", slope = "slope", ymin = "lower",
         ymax = "upper")
  )
  expect_equal(length(res$layers), 3)

  expect_snapshot_error(
    segment_logistic %>%
      dplyr::mutate(group1 = 1, group2 = 2) %>%
      cal_plot_logistic(Class, .pred_good, .by = c(group1, group2))
  )

  lgst_configs <-
    bin_with_configs() %>% cal_plot_logistic(truth = Class, estimate = .pred_good)
  expect_true(has_facet(lgst_configs))
})

test_that("logistic plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_logistic(dplyr::group_by(mtcars, vs))
  )
})

test_that("Binary windowed functions work", {
  x30 <- .cal_table_windowed(
    segment_logistic,
    truth = Class,
    estimate = .pred_good,
    step_size = 0.11,
    window_size = 0.10
  )

  x30_1 <- segment_logistic %>%
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
    )) %>%
    dplyr::filter(!is.na(x)) %>%
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

  x32_1 <- testthat_cal_binary() %>%
    tune::collect_predictions(summarize = TRUE) %>%
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
    )) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::count(.config, x)

  expect_equal(
    x32$total,
    x32_1$n
  )

  x33 <- cal_plot_windowed(testthat_cal_binary())

  expect_s3_class(x33, "ggplot")
  expect_true(has_facet(x33))

  win_configs <-
    bin_with_configs() %>% cal_plot_windowed(truth = Class, estimate = .pred_good)
  expect_true(has_facet(win_configs))


  # ------------------------------------------------------------------------------
  # multinomial outcome, binary windowed plots

  multi_configs_from_tune <-
    testthat_cal_multiclass() %>% cal_plot_windowed()
  expect_s3_class(multi_configs_from_tune, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_tune$facet, "FacetGrid"))


  multi_configs_from_df <-
    mnl_with_configs() %>% cal_plot_windowed(truth = obs, estimate = c(VF:L))
  expect_s3_class(multi_configs_from_df, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_df$facet, "FacetGrid"))
})

test_that("windowed plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_windowed(dplyr::group_by(mtcars, vs))
  )
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


test_that("Groups are respected", {
  preds <- segment_logistic %>%
    dplyr::mutate(source = "logistic") %>%
    dplyr::bind_rows(segment_naive_bayes) %>%
    dplyr::mutate(source = ifelse(is.na(source), "nb", source)) %>%
    dplyr::group_by(source)

  x40 <- .cal_table_breaks(preds, Class, .pred_good)

  expect_equal(as.integer(table(x40$source)), c(10, 10))

  expect_equal(unique(x40$source), c("logistic", "nb"))

  x41 <- .cal_table_logistic(preds, Class, .pred_good)

  expect_equal(as.integer(table(x41$source)), c(101, 101))

  expect_equal(unique(x41$source), c("logistic", "nb"))

  x42 <- .cal_table_windowed(preds, Class, .pred_good)

  expect_equal(as.integer(table(x42$source)), c(21, 21))

  expect_equal(unique(x42$source), c("logistic", "nb"))
})

test_that("Groupings that may not match work", {
  model <- glm(Class ~ .pred_good, segment_logistic, family = "binomial")

  preds <- 1 - predict(model, segment_logistic, type = "response")

  combined <- dplyr::bind_rows(
    dplyr::mutate(segment_logistic, source = "original"),
    dplyr::mutate(segment_logistic, .pred_good = preds, source = "glm")
  )

  x50 <- combined %>%
    dplyr::group_by(source) %>%
    .cal_table_breaks(Class, .pred_good)

  expect_equal(
    unique(x50$predicted_midpoint),
    seq(0.05, 0.95, by = 0.10)
  )

  x51 <- combined %>%
    dplyr::group_by(source) %>%
    .cal_table_windowed(
      truth = Class,
      estimate = .pred_good,
      step_size = 0.11,
      window_size = 0.10
    )

  x51_1 <- combined %>%
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
    )) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::count(source, x)

  expect_equal(
    x51$total,
    x51_1$n
  )
})

test_that("Numeric groups are supported", {
  grp_df <- segment_logistic
  grp_df$num_group <- rep(c(1, 2), times = 505)

  p <- grp_df %>%
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

# ------------------------------------------------------------------------------

test_that("regression functions work", {
  skip_if(R.version[["arch"]] != "aarch64") # see note below

  obj <- testthat_cal_reg()

  res <- cal_plot_regression(boosting_predictions_oob, outcome, .pred)
  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0,],
    dplyr::tibble(outcome = numeric(0), .pred = numeric(0), id = character(0))
  )

  expect_equal(
    rlang::expr_text(res$mapping$x),
    "~outcome"
  )
  expect_equal(
    rlang::expr_text(res$mapping$y),
    "~.pred"
  )
  expect_null(res$mapping$colour)
  expect_null(res$mapping$fill)

  expect_equal(
    res$labels,
    list(x = "Observed", y = "Predicted", colour = "colour", fill = "fill",
         intercept = "intercept", slope = "slope")
  )
  expect_equal(length(res$layers), 3)


  res <- cal_plot_regression(boosting_predictions_oob, outcome, .pred, .by = id)
  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0,],
    dplyr::tibble(outcome = numeric(0), .pred = numeric(0), id = character(0))
  )

  expect_equal(
    rlang::expr_text(res$mapping$x),
    "~outcome"
  )
  expect_equal(
    rlang::expr_text(res$mapping$y),
    "~.pred"
  )
  expect_null(res$mapping$colour)
  expect_null(res$mapping$fill)

  expect_equal(
    res$labels,
    list(x = "Observed", y = "Predicted", colour = "colour", fill = "fill",
         intercept = "intercept", slope = "slope")
  )
  expect_equal(length(res$layers), 3)


  res <- cal_plot_regression(obj)
  expect_s3_class(res, "ggplot")

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    res$data[0,],
    dplyr::tibble(.pred = numeric(0), .row = numeric(0),
                   predictor_01 = integer(0), outcome = numeric(0),
                   .config = character())
  )

  expect_equal(
    rlang::expr_text(res$mapping$x),
    "~outcome"
  )
  expect_equal(
    rlang::expr_text(res$mapping$y),
    "~.pred"
  )
  expect_null(res$mapping$colour)
  expect_null(res$mapping$fill)

  expect_equal(
    res$labels,
    list(x = "Observed", y = "Predicted", colour = "colour", fill = "fill",
         intercept = "intercept", slope = "slope")
  )
  expect_equal(length(res$layers), 3)


  res <- print(cal_plot_regression(obj), alpha = 1 / 5, smooth = FALSE)
  expect_s3_class(res, "ggplot")

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    res$data[0,],
    dplyr::tibble(.pred = numeric(0), .row = numeric(0),
                   predictor_01 = integer(0), outcome = numeric(0),
                   .config = character())
  )

  expect_equal(
    rlang::expr_text(res$mapping$x),
    "~outcome"
  )
  expect_equal(
    rlang::expr_text(res$mapping$y),
    "~.pred"
  )
  expect_null(res$mapping$colour)
  expect_null(res$mapping$fill)

  expect_equal(
    res$labels,
    list(x = "Observed", y = "Predicted", colour = "colour", fill = "fill",
         intercept = "intercept", slope = "slope")
  )
  expect_equal(length(res$layers), 3)


  res <- cal_plot_regression(boosting_predictions_oob, outcome, .pred, smooth = FALSE)
  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0,],
    dplyr::tibble(outcome = numeric(0), .pred = numeric(0),
                   id = character())
  )

  expect_equal(
    rlang::expr_text(res$mapping$x),
    "~outcome"
  )
  expect_equal(
    rlang::expr_text(res$mapping$y),
    "~.pred"
  )
  expect_null(res$mapping$colour)
  expect_null(res$mapping$fill)

  expect_equal(
    res$labels,
    list(x = "Observed", y = "Predicted", colour = "colour", fill = "fill",
         intercept = "intercept", slope = "slope")
  )
  expect_equal(length(res$layers), 3)


})

test_that("regression plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_regression(dplyr::group_by(mtcars, vs))
  )
})

# ------------------------------------------------------------------------------

test_that("don't facet if there is only one .config", {
  class_data <- testthat_cal_binary()

  class_data$.predictions <- lapply(
    class_data$.predictions,
    function(x) dplyr::filter(x, .config == "Preprocessor1_Model1")
  )

  res_breaks <- cal_plot_breaks(class_data)

  expect_null(res_breaks$data[[".config"]])
  expect_s3_class(res_breaks, "ggplot")

  res_logistic <- cal_plot_logistic(class_data)

  expect_null(res_logistic$data[[".config"]])
  expect_s3_class(res_logistic, "ggplot")

  res_windowed <- cal_plot_windowed(class_data)

  expect_null(res_windowed$data[[".config"]])
  expect_s3_class(res_windowed, "ggplot")

  reg_data <- testthat_cal_reg()

  reg_data$.predictions <- lapply(
    reg_data$.predictions,
    function(x) dplyr::filter(x, .config == "Preprocessor01_Model1")
  )

  res_regression <- cal_plot_regression(reg_data)

  expect_null(res_regression$data[[".config"]])
  expect_s3_class(res_regression, "ggplot")
})
