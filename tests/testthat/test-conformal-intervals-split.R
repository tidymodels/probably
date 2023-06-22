test_that("split conformal intervals", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("nnet")

  # ----------------------------------------------------------------------------

  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(dplyr))

  # ----------------------------------------------------------------------------

  set.seed(111)
  sim_data <- sim_regression(500)
  sim_cal <- sim_regression(100)
  sim_new <- sim_regression(2)

  wflow <-
    workflow() %>%
    add_model(parsnip::linear_reg()) %>%
    add_formula(outcome ~ .) %>%
    fit(sim_data)

  # ------------------------------------------------------------------------------

  expect_snapshot_error(
    int_conformal_split(lm(outcome ~ ., sim_data), sim_cal)
  )

  expect_snapshot_error(
    int_conformal_split(wflow, sim_cal[, -1])
  )
  expect_snapshot_error(
    int_conformal_split(wflow, sim_cal[, -2])
  )
  expect_snapshot_error(
    int_conformal_split(wflow, sim_cal, level = .1)
  )

  # ------------------------------------------------------------------------------

  lm_int <- int_conformal_split(wflow, sim_cal)
  expect_snapshot_error(
    predict(lm_int, sim_new, potato = 3)
  )
  expect_snapshot(lm_int)
  expect_true(inherits(lm_int, "int_conformal_split"))

  new_int <- predict(lm_int, sim_new, level = 0.90)
  exp_ptype <-
    dplyr::tibble(
      .pred = numeric(0),
      .pred_lower = numeric(0),
      .pred_upper = numeric(0)
    )

  expect_true(inherits(new_int, "tbl_df"))
  expect_equal(new_int[0,], exp_ptype)
  expect_equal(
    colnames(new_int),
    c(".pred", ".pred_lower", ".pred_upper")
  )
  expect_equal(
    nrow(new_int),
    nrow(sim_new)
  )

})
