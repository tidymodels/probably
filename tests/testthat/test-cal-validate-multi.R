test_that("Isotonic validation with data frame input - Multiclass", {
  df <- rsample::vfold_cv(testthat_cal_sim_multi())

  res_sum <- cal_validate_isotonic(df, class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_isotonic(df, class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Bootstrapped Isotonic validation with data frame input - Multiclass", {
  df <- rsample::vfold_cv(testthat_cal_sim_multi())

  res_sum <- cal_validate_isotonic_boot(df, class)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_isotonic_boot(df, class, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})

test_that("Multinomial validation with data frame input", {
  df <- rsample::vfold_cv(species_probs)

  res_sum <- cal_validate_multinomial(df, Species)
  expect_s3_class(res_sum, "data.frame")
  expect_equal(nrow(res_sum), 2)

  res_not_sum <- cal_validate_multinomial(df, Species, summarize = FALSE)
  expect_s3_class(res_not_sum, "data.frame")
  expect_equal(nrow(res_not_sum), 10)

})
