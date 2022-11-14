test_that("Binary breaks functions work", {

  x10 <- .cal_binary_table_breaks(segment_logistic, Class, .pred_good)

  x10_1 <- segment_logistic %>%
    dplyr::group_by(floor(.pred_good * 10)) %>%
    dplyr::summarise(median(.pred_good)) %>%
    dplyr::pull()

  expect_equal(sd(x10$predicted_midpoint), sd(x10_1), tolerance = 0.000001)
  expect_equal(mean(x10$predicted_midpoint), mean(x10_1), tolerance = 0.000001)

  expect_s3_class(
    cal_plot_breaks(segment_logistic, Class, .pred_good),
    "ggplot"
    )

  expect_error(
    cal_plot_breaks(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x12 <- .cal_binary_table_breaks(testthat_cal_tune_results())

  x12_1 <- testthat_cal_tune_results() %>%
    tune::collect_predictions(summarize = TRUE) %>%
    dplyr::group_by(.config, floor(.pred_class_1 * 10)) %>%
    dplyr::summarise(median(.pred_class_1), .groups = "keep") %>%
    dplyr::pull()

  expect_equal(sd(x12$predicted_midpoint), sd(x12_1), tolerance = 0.000001)
  expect_equal(mean(x12$predicted_midpoint), mean(x12_1), tolerance = 0.000001)

  expect_s3_class(
    cal_plot_breaks(testthat_cal_tune_results()),
    "ggplot"
    )
})

test_that("Binary logistic functions work", {
  x20 <- .cal_binary_table_logistic(segment_logistic, Class, .pred_good)

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

  expect_error(
    cal_plot_logistic(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x22 <- .cal_binary_table_logistic(testthat_cal_tune_results())


  x22_1 <- testthat_cal_tune_results() %>%
    tune::collect_predictions(summarize = TRUE) %>%
    dplyr::group_by(.config) %>%
    dplyr::group_map(~{
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

  x23 <- cal_plot_logistic(testthat_cal_tune_results())

  expect_s3_class(x23, "ggplot")

  x24 <- .cal_binary_table_logistic(segment_logistic, Class, .pred_good, smooth = FALSE)

  model24 <- stats::glm(Class ~ .pred_good, data = segment_logistic, family = binomial())

  preds24 <- predict(model24,
                     data.frame(.pred_good = seq(0, 1, by = .01)),
                     type = "response"
                     )

  expect_equal(sd(x24$prob), sd(preds24), tolerance = 0.000001)
  expect_equal(mean(x24$prob), mean(1- preds24), tolerance = 0.000001)

  x25 <- .cal_binary_table_logistic(
    segment_logistic,
    Class,
    .pred_poor,
    event_level = "second"
  )
})

test_that("Binary windowed functions work", {
  x30 <- .cal_binary_table_windowed(segment_logistic, Class, .pred_good)

  expect_equal(sd(x30$predicted_midpoint), 0.3324836, tolerance = 0.000001)
  expect_equal(mean(x30$predicted_midpoint), 0.3625673, tolerance = 0.000001)

  x31 <- cal_plot_windowed(segment_logistic, Class, .pred_good)

  expect_s3_class(x31, "ggplot")

  expect_error(
    cal_plot_windowed(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x32 <- .cal_binary_table_windowed(testthat_cal_tune_results())

  expect_equal(sd(x32$predicted_midpoint), 0.3571706, tolerance = 0.000001)
  expect_equal(mean(x32$predicted_midpoint), 0.4374119, tolerance = 0.000001)

  x33 <- cal_plot_windowed(testthat_cal_tune_results())

  expect_s3_class(x33, "ggplot")
})

test_that("Event level handling works", {
  x7 <- .cal_binary_table_breaks(segment_logistic, Class, .pred_good, event_level = "second")
  expect_equal(
    which(x7$predicted_midpoint == min(x7$predicted_midpoint)),
    which(x7$event_rate == max(x7$event_rate))
  )

  expect_error(
    .cal_binary_table_breaks(segment_logistic, Class, .pred_good, event_level = "invalid"),
    "Invalid event_level entry. Valid entries are 'first' and 'second'"
  )
})
