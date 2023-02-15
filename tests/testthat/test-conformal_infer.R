test_that("bad inputs to conformal intervals", {
  skip_if_not_installed("modeldata")

  # ----------------------------------------------------------------------------

  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(purrr))

  # ----------------------------------------------------------------------------

  set.seed(111)
  sim_data <- sim_regression(500)
  wflow <-
    workflow() %>%
    add_model(parsnip::linear_reg()) %>%
    add_formula(outcome ~ .) %>%
    fit(sim_data)

  set.seed(182)
  sim_new <- sim_regression(2)

  # ----------------------------------------------------------------------------

  set.seed(121212)
  sim_cls_data <- sim_classification(100)
  wflow_cls <-
    workflow() %>%
    add_model(parsnip::logistic_reg()) %>%
    add_formula(class ~ .) %>%
    fit(sim_cls_data)

  sim_cls_new <- sim_classification(2)

  # ----------------------------------------------------------------------------

  basic_obj <- int_conformal_infer(wflow, train_data = sim_data)
  expect_snapshot(basic_obj)
  expect_s3_class(basic_obj, "int_conformal_infer")

  expect_snapshot(error = TRUE,
    int_conformal_infer(workflow(), sim_new)
  )

  expect_snapshot(error = TRUE,
    int_conformal_infer(wflow %>% extract_fit_parsnip(), sim_new)
  )

  expect_snapshot(error = TRUE,
    int_conformal_infer(wflow_cls, sim_cls_new)
  )

  expect_snapshot(error = TRUE,
    predict(basic_obj, sim_new[, 3:5])
  )

  expect_snapshot(error = TRUE,
    int_conformal_infer(wflow, train_data = sim_cls_data)
  )

  expect_snapshot(error = TRUE,
    int_conformal_infer(wflow, sim_new)
  )

  expect_snapshot(error = TRUE,
    int_conformal_infer(
      wflow,
      sim_data,
      control = control_conformal_infer(required_pkgs = "boop")
    )
  )

  expect_snapshot(
    get_root(try(stop("ope!"), silent = TRUE), control_conformal_infer())
  )
})

test_that("conformal intervals", {
  skip_if_not_installed("modeldata")
  skip_on_cran()

  # ----------------------------------------------------------------------------

  library(workflows)
  library(modeldata)

  # ----------------------------------------------------------------------------

  set.seed(111)
  sim_data <- sim_regression(500)
  sim_small <- sim_data[1:25,]

  wflow <-
    workflow() %>%
    add_model(parsnip::linear_reg()) %>%
    add_formula(outcome ~ .) %>%
    fit(sim_data)

  wflow_small <-
    workflow() %>%
    add_model(parsnip::linear_reg()) %>%
    add_formula(outcome ~ .) %>%
    fit(sim_small)

  set.seed(182)
  sim_new <- sim_regression(2)

  ctrl_grid <- control_conformal_infer(method = "grid", seed = 1)
  basic_obj <- int_conformal_infer(wflow, train_data = sim_data, control = ctrl_grid)

  ctrl_hard <- control_conformal_infer(progress = TRUE, seed = 1,
                                  max_iter = 2, tolerance = 0.000001)
  smol_obj  <- int_conformal_infer(wflow_small, train_data = sim_small, control = ctrl_hard)

  # ----------------------------------------------------------------------------

  expect_snapshot(
    res_small <- predict(smol_obj, sim_new)

  )
  expect_equal(names(res_small), c(".pred_lower", ".pred_upper"))
  expect_equal(nrow(res_small), 2)
  expect_true(mean(complete.cases(res_small)) < 1)

  # ----------------------------------------------------------------------------


  res <- predict(basic_obj, sim_new[1,])
  expect_equal(names(res), c(".pred_lower", ".pred_upper"))
  expect_equal(nrow(res), 1)
  expect_true(mean(complete.cases(res)) == 1)

})

test_that("conformal control", {
  set.seed(1)
  expect_snapshot(dput(control_conformal_infer()))
  expect_snapshot(dput(control_conformal_infer(max_iter = 2)))
  expect_snapshot(error = TRUE, control_conformal_infer(method = "rock-paper-scissors"))
})
