test_that("Logistic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_logistic(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 1)

  res_not_sum <- cal_validate_logistic(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Isotonic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_isotonic(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 1)

  res_not_sum <- cal_validate_isotonic(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Beta validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_beta(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 1)

  res_not_sum <- cal_validate_beta(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})
