test_that("Binary breaks functions work", {

  x10 <- .cal_binary_table_breaks(segment_logistic, Class, .pred_good)

  expect_equal(
    x10$predicted_midpoint,
    seq(0.05, 0.95, by = 0.10)
    )

  expect_s3_class(
    cal_plot_breaks(segment_logistic, Class, .pred_good),
    "ggplot"
    )

  expect_error(
    cal_plot_breaks(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x11 <- .cal_binary_table_breaks(testthat_cal_tune_results())

  expect_equal(
    x11$predicted_midpoint,
    rep(seq(0.05, 0.95, by = 0.10), times = 8)
    )

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

  expect_equal(
    which(x25$prob == max(x25$prob)),
    nrow(x25)
  )
})

test_that("Binary windowed functions work", {

  x30 <- .cal_binary_table_windowed(
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
    count(x)

  expect_equal(
    x30$total,
    x30_1$n
  )

  x31 <- cal_plot_windowed(segment_logistic, Class, .pred_good)

  expect_s3_class(x31, "ggplot")

  expect_error(
    cal_plot_windowed(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x32 <- .cal_binary_table_windowed(
    testthat_cal_tune_results(),
    step_size = 0.11,
    window_size = 0.10
    )

  x32_1 <- testthat_cal_tune_results() %>%
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


test_that("Groups are respected", {
  preds <- segment_logistic %>%
    dplyr::mutate(source = "logistic") %>%
    dplyr::bind_rows(segment_naive_bayes) %>%
    dplyr::mutate(source = ifelse(is.na(source), "nb", source)) %>%
    dplyr::group_by(source)

  x40 <- .cal_binary_table_breaks(preds, Class, .pred_good)

  expect_equal(as.integer(table(x40$source)), c(10, 10))

  expect_equal(unique(x40$source), c("logistic", "nb"))

  x41 <- .cal_binary_table_logistic(preds, Class, .pred_good)

  expect_equal(as.integer(table(x41$source)), c(101, 101))

  expect_equal(unique(x41$source), c("logistic", "nb"))

  x42 <- .cal_binary_table_windowed(preds, Class, .pred_good)

  expect_equal(as.integer(table(x42$source)), c(21, 21))

  expect_equal(unique(x42$source), c("logistic", "nb"))
})

test_that("Groupings that may not match work", {
  model <- glm(Class ~ .pred_good, segment_logistic, family = "binomial")

  preds <- 1 -  predict(model, segment_logistic, type = "response")

  combined <- dplyr::bind_rows(
    dplyr::mutate(segment_logistic, source = "original"),
    dplyr::mutate(segment_logistic, .pred_good = preds, source = "glm")
  )

  x50 <- combined %>%
    group_by(source) %>%
    .cal_binary_table_breaks(Class, .pred_good)

  expect_equal(
    unique(x50$predicted_midpoint),
    seq(0.05, 0.95, by = 0.10)
  )

  x51 <- combined %>%
    group_by(source) %>%
    .cal_binary_table_windowed(
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
    count(source, x)

  expect_equal(
    x51$total,
    x51_1$n
  )

})

test_that("Numeric groups are supported", {
  grp_df <- segment_logistic
  grp_df$num_group <- rep(c(1,2), times = 505)

  p <- grp_df %>%
    dplyr::group_by(num_group) %>%
    cal_plot_breaks(Class, .pred_good)

  expect_s3_class(p, "ggplot")
})
