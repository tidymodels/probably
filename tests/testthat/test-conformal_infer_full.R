test_that("bad inputs to conformal intervals", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("nnet")

  # ----------------------------------------------------------------------------

  suppressPackageStartupMessages(library(workflows))
  suppressPackageStartupMessages(library(modeldata))
  suppressPackageStartupMessages(library(purrr))
  suppressPackageStartupMessages(library(rsample))
  suppressPackageStartupMessages(library(tune))
  suppressPackageStartupMessages(library(dplyr))

  # ----------------------------------------------------------------------------

  set.seed(111)
  sim_data <- sim_regression(500)
  wflow <-
    workflow() |>
    add_model(parsnip::linear_reg()) |>
    add_formula(outcome ~ .) |>
    fit(sim_data)

  set.seed(182)
  sim_new <- sim_regression(2)

  ctrl <- control_resamples(save_pred = TRUE, extract = I)

  set.seed(382)
  cv <- vfold_cv(sim_data, v = 2)
  good_res <-
    parsnip::linear_reg() |> fit_resamples(outcome ~ ., cv, control = ctrl)

  set.seed(382)
  cv <- vfold_cv(sim_data, v = 2, repeats = 2)
  rep_res <-
    parsnip::linear_reg() |> fit_resamples(outcome ~ ., cv, control = ctrl)

  set.seed(382)
  bt <- bootstraps(sim_data, times = 2)
  bt_res <-
    parsnip::linear_reg() |> fit_resamples(outcome ~ ., bt, control = ctrl)

  # ----------------------------------------------------------------------------

  set.seed(121212)
  sim_cls_data <- sim_classification(100)
  wflow_cls <-
    workflow() |>
    add_model(parsnip::logistic_reg()) |>
    add_formula(class ~ .) |>
    fit(sim_cls_data)

  sim_cls_new <- sim_classification(2)

  # ----------------------------------------------------------------------------

  # When the gam for variance fails:
  expect_snapshot(
    error = TRUE,
    int_conformal_full(wflow, sim_new)
  )

  expect_snapshot(
    error = TRUE,
    int_conformal_full(
      wflow,
      sim_data,
      control = control_conformal_full(required_pkgs = "boop")
    )
  )

  basic_obj <- int_conformal_full(wflow, train_data = sim_data)
  expect_snapshot(basic_obj)
  expect_s3_class(basic_obj, "int_conformal_full")

  expect_snapshot(
    error = TRUE,
    int_conformal_full(workflow(), sim_new)
  )

  expect_snapshot(
    error = TRUE,
    int_conformal_full(wflow |> extract_fit_parsnip(), sim_new)
  )

  expect_snapshot(
    error = TRUE,
    int_conformal_full(wflow_cls, sim_cls_new)
  )

  expect_snapshot(
    error = TRUE,
    predict(basic_obj, sim_new[, 3:5])
  )

  expect_snapshot(
    error = TRUE,
    int_conformal_full(wflow, train_data = sim_cls_data)
  )

  expect_snapshot(
    probably:::get_root(
      try(stop("I made you stop"), silent = TRUE),
      control_conformal_full()
    )
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
  sim_small <- sim_data[1:25, ]

  wflow <-
    workflow() |>
    add_model(parsnip::linear_reg()) |>
    add_formula(outcome ~ .) |>
    fit(sim_data)

  wflow_small <-
    workflow() |>
    add_model(parsnip::linear_reg()) |>
    add_formula(outcome ~ .) |>
    fit(sim_small)

  set.seed(182)
  sim_new <- sim_regression(2)

  ctrl_grid <- control_conformal_full(method = "grid", seed = 1)
  basic_obj <- int_conformal_full(
    wflow,
    train_data = sim_data,
    control = ctrl_grid
  )

  ctrl_hard <- control_conformal_full(
    progress = TRUE,
    seed = 1,
    max_iter = 2,
    tolerance = 0.000001
  )
  smol_obj <- int_conformal_full(
    wflow_small,
    train_data = sim_small,
    control = ctrl_hard
  )

  ctrl <- control_resamples(save_pred = TRUE, extract = I)
  set.seed(382)
  cv <- vfold_cv(sim_data, v = 2)
  cv_res <-
    parsnip::linear_reg() |> fit_resamples(outcome ~ ., cv, control = ctrl)
  grid_res <-
    parsnip::mlp(penalty = tune()) |>
    parsnip::set_mode("regression") |>
    tune_grid(outcome ~ ., cv, grid = 2, control = ctrl)

  # ----------------------------------------------------------------------------

  expect_snapshot(
    res_small <- predict(smol_obj, sim_new)
  )
  expect_equal(names(res_small), c(".pred_lower", ".pred_upper"))
  expect_equal(nrow(res_small), 2)
  expect_true(mean(complete.cases(res_small)) < 1)

  # ----------------------------------------------------------------------------

  res <- predict(basic_obj, sim_new[1, ])
  expect_equal(names(res), c(".pred_lower", ".pred_upper"))
  expect_equal(nrow(res), 1)
  expect_true(mean(complete.cases(res)) == 1)

  # ----------------------------------------------------------------------------

  expect_identical(
    required_pkgs(smol_obj),
    c(required_pkgs(smol_obj$wflow), "probably")
  )
  expect_identical(
    required_pkgs(smol_obj, infra = FALSE),
    required_pkgs(smol_obj$wflow, infra = FALSE)
  )
})

test_that("conformal control", {
  set.seed(1)
  expect_snapshot(dput(control_conformal_full()))
  expect_snapshot(dput(control_conformal_full(max_iter = 2)))
  expect_snapshot(
    error = TRUE,
    control_conformal_full(method = "rock-paper-scissors")
  )
})
