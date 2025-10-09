test_that("attaches the butcher class", {
  skip_if_not_installed("butcher")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(rsample))
  suppressPackageStartupMessages(library(tune))
  suppressPackageStartupMessages(library(parsnip))
  suppressPackageStartupMessages(library(butcher))

  set.seed(2)
  sim_train <- sim_regression(500)
  sim_cal <- sim_regression(200)

  # We'll use a neural network model
  mlp_spec <-
    mlp(hidden_units = 5, penalty = 0.01) |>
    set_mode("regression")

  mlp_wflow <-
    workflow() |>
    add_model(mlp_spec) |>
    add_formula(outcome ~ .)

  fit <- fit(mlp_wflow, data = sim_train)

  c_int <- int_conformal_split(fit, sim_cal)
  c_int <- butcher(c_int)
  expect_s3_class(c_int, "butchered_int_conformal_split")

  c_int <- int_conformal_full(fit, sim_cal)
  c_int <- butcher(c_int)
  expect_s3_class(c_int, "butchered_int_conformal_full")

  c_int <- int_conformal_quantile(fit, sim_cal, sim_cal)
  c_int <- butcher(c_int)
  expect_s3_class(c_int, "butchered_int_conformal_quantile")

  ctrl <- control_resamples(save_pred = TRUE, extract = I)

  res <- mlp_wflow |>
    fit_resamples(resamples = vfold_cv(sim_train, v = 2), control = ctrl)

  fit <- int_conformal_cv(res)

  c_int <- int_conformal_cv(res)
  c_int <- butcher(c_int)
  expect_s3_class(c_int, "butchered_int_conformal_cv")
})

test_that("butcher works", {
  skip_if_not_installed("butcher")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(rsample))
  suppressPackageStartupMessages(library(tune))
  suppressPackageStartupMessages(library(parsnip))
  suppressPackageStartupMessages(library(butcher))

  set.seed(2)
  sim_train <- sim_regression(500)
  sim_cal <- sim_regression(200)

  # We'll use a neural network model
  mlp_spec <-
    mlp(hidden_units = 5, penalty = 0.01) |>
    set_mode("regression")

  mlp_wflow <-
    workflow() |>
    add_model(mlp_spec) |>
    add_formula(outcome ~ .)

  fit <- fit(mlp_wflow, data = sim_train)

  c_int <- int_conformal_split(fit, sim_cal)
  expect_identical(
    butcher(c_int)$wflow,
    butcher(c_int$wflow)
  )

  c_int <- int_conformal_full(fit, sim_cal)
  expect_identical(
    butcher(c_int)$wflow,
    butcher(c_int$wflow)
  )

  c_int <- int_conformal_quantile(fit, sim_cal, sim_cal)
  expect_identical(
    butcher(c_int)$wflow,
    butcher(c_int$wflow)
  )

  ctrl <- control_resamples(save_pred = TRUE, extract = I)

  res <- mlp_wflow |>
    fit_resamples(resamples = vfold_cv(sim_train, v = 2), control = ctrl)

  fit <- int_conformal_cv(res)

  c_int <- int_conformal_cv(res)
  expect_identical(
    butcher(c_int)$models,
    lapply(c_int$models, butcher)
  )
})
