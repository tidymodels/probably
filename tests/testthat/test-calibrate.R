test_that("Binary breaks functions work",{

  x1 <- cal_binary_table_breaks(segment_logistic, Class, .pred_good)

  expect_equal(sd(x1$predicted_midpoint), 0.3063903, tolerance = 0.000001)
  expect_equal(mean(x1$predicted_midpoint), 0.4964808, tolerance = 0.000001)

  x2 <- cal_binary_plot_breaks(segment_logistic, Class, .pred_good)

  expect_s3_class(x2, "ggplot")

  expect_error(
    cal_binary_plot_breaks(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )
})

test_that("Binary logistic functions work",{

  x3 <- cal_binary_table_logistic(segment_logistic, Class, .pred_good)

  expect_equal(sd(x3$prob), 0.2530259, tolerance = 0.000001)
  expect_equal(mean(x3$prob), 0.4581547, tolerance = 0.000001)

  x4 <- cal_binary_plot_logistic(segment_logistic, Class, .pred_good)

  expect_s3_class(x4, "ggplot")

  expect_error(
    cal_binary_plot_logistic(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )
})

test_that("Binary windowed functions work",{

  x5 <- cal_binary_table_windowed(segment_logistic, Class, .pred_good)

  expect_equal(sd(x5$predicted_midpoint), 0.3121576, tolerance = 0.000001)
  expect_equal(mean(x5$predicted_midpoint), 0.3111778, tolerance = 0.000001)

  x6 <- cal_binary_plot_windowed(segment_logistic, Class, .pred_good)

  expect_s3_class(x6, "ggplot")

  expect_error(
    cal_binary_plot_windowed(species_probs, Species, .pred_bobcat),
    "'Species' does not have 2 levels"
  )
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
