test_that("lower_limit bounds for numeric predictions", {
  skip_if_not_installed("modeldata")
  library(dplyr)
  library(rlang)
  data("solubility_test", package = "modeldata")
  tune2 <- function() call("tune", "test")

  # ------------------------------------------------------------------------------

  expect_snapshot(bound_prediction(modeldata::solubility_test, lower_limit = 2), error = TRUE)
  expect_snapshot(
    modeldata::solubility_test |>
      mutate(.pred = format(prediction)) |>
      bound_prediction(lower_limit = 2),
    error = TRUE)

  sol <- modeldata::solubility_test |> set_names(c("solubility", ".pred"))
  
  expect_equal(bound_prediction(sol), sol)
  expect_equal(bound_prediction(sol, lower_limit = NA), sol)

  res_1 <- bound_prediction(sol, lower_limit = -1)
  expect_true(all(res_1$.pred[res_1$.pred < -1] == -1))
  expect_equal(res_1$.pred[sol$.pred >= -1], sol$.pred[sol$.pred >= -1])

  expect_snapshot(bound_prediction(sol, lower_limit = tune2()), error = TRUE)
  expect_snapshot(bound_prediction(as.matrix(sol), lower_limit = 1), error = TRUE)
})

test_that("upper_limit bounds for numeric predictions", {
  skip_if_not_installed("modeldata")
  library(dplyr)
  library(rlang)
  data("solubility_test", package = "modeldata")
  tune2 <- function() call("tune", "test")

  # ------------------------------------------------------------------------------

  expect_snapshot(bound_prediction(modeldata::solubility_test, lower_limit = 2), error = TRUE)
  expect_snapshot(
    modeldata::solubility_test |>
      mutate(.pred = format(prediction)) |>
      bound_prediction(lower_limit = 2),
    error = TRUE)

  sol <- modeldata::solubility_test |> set_names(c("solubility", ".pred"))

  expect_equal(bound_prediction(sol), sol)
  expect_equal(bound_prediction(sol, upper_limit = NA), sol)

  res_1 <- bound_prediction(sol, upper_limit = -1)
  expect_true(all(res_1$.pred[res_1$.pred > -1] == -1))
  expect_equal(res_1$.pred[sol$.pred <= -1], sol$.pred[sol$.pred <= -1])

  expect_snapshot(bound_prediction(sol, upper_limit = tune2()), error = TRUE)
})
