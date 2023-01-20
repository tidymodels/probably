test_that("Logistic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_logistic(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_logistic(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Isotonic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_isotonic(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_isotonic(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Bootstrapped Isotonic validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_isotonic_boot(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_isotonic_boot(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Beta validation works", {
  df <- testthat_cal_sampled()

  res_sum <- cal_validate_beta(df, Class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_beta(df, Class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Setting summarize to FALSE returns new columns", {
  df <- testthat_cal_sampled()

  no_res <- cal_validate_beta(
    df, Class,
    summarize = FALSE,
    save_details = TRUE
    )

  expect_snapshot(no_res)

})
