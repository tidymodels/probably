test_that("lower bounds for numeric predictions", {
  skip_if_not_installed("modeldata")
  library(dplyr)
  library(rlang)
  data("solubility_test", package = "modeldata")

  expect_snapshot(bound_prediction(solubility_test, lower = 2), error = TRUE)
  expect_snapshot(
    solubility_test %>%
      mutate(.pred = format(prediction)) %>%
      bound_prediction(lower = 2),
    error = TRUE)

  sol <- solubility_test %>% set_names(c("solubility", ".pred"))

  expect_equal(bound_prediction(sol), sol)
  expect_equal(bound_prediction(sol, lower = NA), sol)

  res_1 <- bound_prediction(sol, lower = -1)
  expect_true(all(res_1$.pred[res_1$.pred < -1] == -1))
  expect_true(all(res_1$.pred[res_1$.pred >= -1] == res_1$.pred[res_1$.pred >= -1]))
})

test_that("upper bounds for numeric predictions", {
  skip_if_not_installed("modeldata")
  library(dplyr)
  library(rlang)
  data("solubility_test", package = "modeldata")

  expect_snapshot(bound_prediction(solubility_test, lower = 2), error = TRUE)
  expect_snapshot(
    solubility_test %>%
      mutate(.pred = format(prediction)) %>%
      bound_prediction(lower = 2),
    error = TRUE)

  sol <- solubility_test %>% set_names(c("solubility", ".pred"))

  expect_equal(bound_prediction(sol), sol)
  expect_equal(bound_prediction(sol, upper = NA), sol)

  res_1 <- bound_prediction(sol, upper = -1)
  expect_true(all(res_1$.pred[res_1$.pred > -1] == -1))
  expect_true(all(res_1$.pred[res_1$.pred <= -1] == res_1$.pred[res_1$.pred <= -1]))
})
