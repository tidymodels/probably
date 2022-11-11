test_that("Binary breaks functions work", {
  x10 <- cal_binary_table_breaks(segment_logistic, Class, .pred_good)

  expect_equal(sd(x10$predicted_midpoint), 0.3063903, tolerance = 0.000001)
  expect_equal(mean(x10$predicted_midpoint), 0.4964808, tolerance = 0.000001)

  x11 <- cal_binary_plot_breaks(segment_logistic, Class, .pred_good)

  expect_s3_class(x11, "ggplot")

  expect_error(
    cal_binary_plot_breaks(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x12 <- cal_binary_table_breaks(testthat_get_tune_results())

  expect_equal(sd(x12$predicted_midpoint), 0.2982188, tolerance = 0.000001)
  expect_equal(mean(x12$predicted_midpoint), 0.5010704, tolerance = 0.000001)

  x13 <- cal_binary_plot_breaks(testthat_get_tune_results())

  expect_s3_class(x13, "ggplot")
})

test_that("Binary logistic functions work", {
  x20 <- cal_binary_table_logistic(segment_logistic, Class, .pred_good)

  expect_equal(sd(x20$prob), 0.2530259, tolerance = 0.000001)
  expect_equal(mean(x20$prob), 0.4581547, tolerance = 0.000001)

  x21 <- cal_binary_plot_logistic(segment_logistic, Class, .pred_good)

  expect_s3_class(x21, "ggplot")

  expect_error(
    cal_binary_plot_logistic(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x22 <- cal_binary_table_logistic(testthat_get_tune_results())

  expect_equal(sd(x22$prob), 0.2784706, tolerance = 0.000001)
  expect_equal(mean(x22$prob), 0.4765672, tolerance = 0.000001)

  x23 <- cal_binary_plot_logistic(testthat_get_tune_results())

  expect_s3_class(x23, "ggplot")

  x24 <- cal_binary_table_logistic(segment_logistic, Class, .pred_good, smooth = FALSE)

  expect_equal(sd(x24$prob), 0.2744587, tolerance = 0.000001)
  expect_equal(mean(x24$prob), 0.4391935, tolerance = 0.000001)
})

test_that("Binary windowed functions work", {
  x30 <- cal_binary_table_windowed(segment_logistic, Class, .pred_good)

  expect_equal(sd(x30$predicted_midpoint), 0.3324836, tolerance = 0.000001)
  expect_equal(mean(x30$predicted_midpoint), 0.3625673, tolerance = 0.000001)

  x31 <- cal_binary_plot_windowed(segment_logistic, Class, .pred_good)

  expect_s3_class(x31, "ggplot")

  expect_error(
    cal_binary_plot_windowed(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )

  x32 <- cal_binary_table_windowed(testthat_get_tune_results())

  expect_equal(sd(x32$predicted_midpoint), 0.3571706, tolerance = 0.000001)
  expect_equal(mean(x32$predicted_midpoint), 0.4374119, tolerance = 0.000001)

  x33 <- cal_binary_plot_windowed(testthat_get_tune_results())

  expect_s3_class(x33, "ggplot")
})

test_that("Event level handling works", {
  x7 <- cal_binary_table_breaks(segment_logistic, Class, .pred_good, event_level = "second")
  expect_equal(
    which(x7$predicted_midpoint == min(x7$predicted_midpoint)),
    which(x7$event_rate == max(x7$event_rate))
  )

  expect_error(
    cal_binary_table_breaks(segment_logistic, Class, .pred_good, event_level = "invalid"),
    "Invalid event_level entry. Valid entries are 'first' and 'second'"
  )
})
