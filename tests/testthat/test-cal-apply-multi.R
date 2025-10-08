test_that("Multinomial apply works - data.frame", {
  sl_multinomial <- cal_estimate_multinomial(species_probs, Species, smooth = FALSE)
  ap_multinomial <- cal_apply(species_probs, sl_multinomial)

  pred_bobcat <- ap_multinomial$.pred_bobcat
  expect_equal(mean(pred_bobcat), 0.5181842, tolerance = 0.000001)
  expect_equal(sd(pred_bobcat), 0.3264982, tolerance = 0.000001)
})

test_that("Logistic apply works - tune_results", {
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_multiclass()
  tl_multinomial <- cal_estimate_multinomial(tct, smooth = FALSE)
  tap_multinomial <- cal_apply(tct, tl_multinomial)
  
  expect_equal(
    testthat_cal_multiclass_count(),
    nrow(tap_multinomial)
  )
})

test_that("Multinomial spline apply works", {
  sl_gam <- cal_estimate_multinomial(species_probs, Species)
  ap_gam <- cal_apply(species_probs, sl_gam)

  pred_bobcat <- ap_gam$.pred_bobcat
  expect_equal(mean(pred_bobcat), 0.5181818, tolerance = 0.000001)
  expect_equal(sd(pred_bobcat), 0.3274646, tolerance = 0.000001)
})

test_that("Multinomial spline apply works - tune_results", {
  skip_if_not_installed("modeldata")

  tct <- testthat_cal_multiclass()
  tl_gam <- cal_estimate_multinomial(tct)
  tap_gam <- cal_apply(tct, tl_gam)

  expect_equal(
    testthat_cal_multiclass_count(),
    nrow(tap_gam)
  )
})
