test_that("regression functions work", {
  skip_if_not_installed("ggplot2", minimum_version = "3.5.2.9000")
  skip_if(R.version[["arch"]] != "aarch64") # see note below

  obj <- testthat_cal_reg()

  res <- cal_plot_regression(boosting_predictions_oob, outcome, .pred)
  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0, ],
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

  expect_snapshot(get_labs(res))

  expect_equal(length(res$layers), 3)

  res <- cal_plot_regression(boosting_predictions_oob, outcome, .pred, .by = id)
  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0, ],
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

  expect_snapshot(get_labs(res))

  expect_equal(length(res$layers), 3)

  res <- cal_plot_regression(obj)
  expect_s3_class(res, "ggplot")

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    res$data[0, ],
    dplyr::tibble(
      .pred = numeric(0),
      .row = numeric(0),
      predictor_01 = integer(0),
      outcome = numeric(0),
      .config = character()
    )
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

  expect_snapshot(get_labs(res))

  expect_equal(length(res$layers), 3)

  res <- print(cal_plot_regression(obj), alpha = 1 / 5, smooth = FALSE)
  expect_s3_class(res, "ggplot")

  skip_if_not_installed("tune", "1.2.0")
  expect_equal(
    res$data[0, ],
    dplyr::tibble(
      .pred = numeric(0),
      .row = numeric(0),
      predictor_01 = integer(0),
      outcome = numeric(0),
      .config = character()
    )
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

  expect_snapshot(get_labs(res))

  expect_equal(length(res$layers), 3)

  res <- cal_plot_regression(
    boosting_predictions_oob,
    outcome,
    .pred,
    smooth = FALSE
  )
  expect_s3_class(res, "ggplot")

  expect_equal(
    res$data[0, ],
    dplyr::tibble(outcome = numeric(0), .pred = numeric(0), id = character())
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

  expect_snapshot(get_labs(res))

  expect_equal(length(res$layers), 3)
})

test_that("regression plot function errors - grouped_df", {
  expect_snapshot_error(
    cal_plot_regression(dplyr::group_by(mtcars, vs))
  )
})

test_that("don't facet if there is only one .config", {
  reg_data <- testthat_cal_reg()

  reg_data$.predictions <- lapply(
    reg_data$.predictions,
    function(x) dplyr::filter(x, .config == "Preprocessor01_Model1")
  )

  res_regression <- cal_plot_regression(reg_data)

  expect_null(res_regression$data[[".config"]])
  expect_s3_class(res_regression, "ggplot")
})
