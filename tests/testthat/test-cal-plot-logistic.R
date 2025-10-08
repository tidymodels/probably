test_that("Binary logistic functions work", {
  skip_if_not_installed("modeldata")

  x20 <- .cal_table_logistic(segment_logistic, Class, .pred_good)

  model20 <- mgcv::gam(
    Class ~ s(.pred_good, k = 10),
    data = segment_logistic,
    family = binomial()
  )

  preds20 <- predict(
    model20,
    data.frame(.pred_good = seq(0, 1, by = .01)),
    type = "response"
  )

  expect_equal(sd(x20$prob), sd(preds20), tolerance = 0.000001)
  expect_equal(mean(x20$prob), mean(1 - preds20), tolerance = 0.000001)

  x21 <- cal_plot_logistic(segment_logistic, Class, .pred_good)

  expect_s3_class(x21, "ggplot")
  expect_false(has_facet(x21))

  x22 <- .cal_table_logistic(testthat_cal_binary())

  x22_1 <- testthat_cal_binary() |>
    tune::collect_predictions(summarize = TRUE) |>
    dplyr::group_by(.config) |>
    dplyr::group_map(
      ~ {
        model <- mgcv::gam(
          class ~ s(.pred_class_1, k = 10),
          data = .x,
          family = binomial()
        )
        preds <- predict(
          model,
          data.frame(.pred_class_1 = seq(0, 1, by = .01)),
          type = "response"
        )
        1 - preds
      }
    ) |>
    purrr::reduce(c)

  expect_equal(sd(x22$prob), sd(x22_1), tolerance = 0.000001)
  expect_equal(mean(x22$prob), mean(x22_1), tolerance = 0.000001)

  x23 <- cal_plot_logistic(testthat_cal_binary())

  expect_s3_class(x23, "ggplot")
  expect_true(has_facet(x23))

  x24 <- .cal_table_logistic(
    segment_logistic,
    Class,
    .pred_good,
    smooth = FALSE
  )

  model24 <- stats::glm(
    Class ~ .pred_good,
    data = segment_logistic,
    family = binomial()
  )

  preds24 <- predict(
    model24,
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
    bin_with_configs() |>
    cal_plot_logistic(truth = Class, estimate = .pred_good)
  expect_true(has_facet(lgst_configs))

  # ------------------------------------------------------------------------------
  # multinomial outcome, binary logistic plots

  multi_configs_from_tune <-
    testthat_cal_multiclass() |> cal_plot_logistic(smooth = FALSE)
  expect_s3_class(multi_configs_from_tune, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_tune$facet, "FacetGrid"))

  multi_configs_from_df <-
    mnl_with_configs() |> cal_plot_logistic(truth = obs, estimate = c(VF:L))
  expect_s3_class(multi_configs_from_df, "ggplot")
  # should be faceted by .config and class
  expect_true(inherits(multi_configs_from_df$facet, "FacetGrid"))
})

test_that("Binary logistic functions work with group argument", {
  skip_if_not_installed("ggplot2", minimum_version = "3.5.2.9000")
  res <- segment_logistic |>
    dplyr::mutate(id = dplyr::row_number() %% 2) |>
    cal_plot_logistic(Class, .pred_good, .by = id)

  expect_s3_class(
    res,
    "ggplot"
  )
  expect_true(has_facet(res))

  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0, ],
    dplyr::tibble(
      id = factor(0, levels = paste(0:1)),
      estimate = double(),
      prob = double(),
      lower = double(),
      upper = double()
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

  expect_snapshot(get_labs(res))

  expect_equal(length(res$layers), 3)

  expect_snapshot_error(
    segment_logistic |>
      dplyr::mutate(group1 = 1, group2 = 2) |>
      cal_plot_logistic(Class, .pred_good, .by = c(group1, group2))
  )

  lgst_configs <-
    bin_with_configs() |>
    cal_plot_logistic(truth = Class, estimate = .pred_good)
  expect_true(has_facet(lgst_configs))
})

test_that("logistic plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_logistic(dplyr::group_by(mtcars, vs))
  )
})

test_that("don't facet if there is only one .config", {
  class_data <- testthat_cal_binary()

  class_data$.predictions <- lapply(
    class_data$.predictions,
    function(x) dplyr::filter(x, .config == "Preprocessor1_Model1")
  )

  res_logistic <- cal_plot_logistic(class_data)

  expect_null(res_logistic$data[[".config"]])
  expect_s3_class(res_logistic, "ggplot")
})


test_that("Groups are respected", {
  preds <- segment_logistic |>
    dplyr::mutate(source = "logistic") |>
    dplyr::bind_rows(segment_naive_bayes) |>
    dplyr::mutate(source = ifelse(is.na(source), "nb", source)) |>
    dplyr::group_by(source)

  x41 <- .cal_table_logistic(preds, Class, .pred_good)

  expect_equal(as.integer(table(x41$source)), c(101, 101))

  expect_equal(unique(x41$source), c("logistic", "nb"))
})
