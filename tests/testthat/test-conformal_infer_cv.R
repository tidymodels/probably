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

  basic_cv_obj <- int_conformal_cv(good_res)

  expect_snapshot_warning(
    int_conformal_cv(rep_res)
  )
  expect_snapshot_warning(
    int_conformal_cv(bt_res)
  )

  # training set < 500 because of 2 bootstraps
  expect_snapshot(basic_cv_obj)
  expect_s3_class(basic_cv_obj, "int_conformal_cv")

  expect_snapshot(
    error = TRUE,
    int_conformal_cv(workflow())
  )
  expect_snapshot(
    error = TRUE,
    int_conformal_cv(good_res |> dplyr::select(-.predictions))
  )
  expect_snapshot(
    error = TRUE,
    int_conformal_cv(good_res |> dplyr::select(-.extracts))
  )

  expect_snapshot(
    error = TRUE,
    predict(basic_cv_obj, sim_new[, 3:5])
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

  cv_int <- int_conformal_cv(cv_res)
  cv_bounds <- predict(cv_int, sim_small)
  cv_bounds_90 <- predict(cv_int, sim_small, level = .9)
  expect_equal(names(cv_bounds), c(".pred_lower", ".pred", ".pred_upper"))
  expect_equal(nrow(cv_bounds), nrow(sim_small))
  expect_true(mean(complete.cases(cv_bounds)) == 1)
  expect_true(
    all(cv_bounds$.pred_lower < cv_bounds_90$.pred_lower)
  )

  # ----------------------------------------------------------------------------

  two_models <- show_best(grid_res, metric = "rmse")[, c("penalty", ".config")]
  expect_snapshot(error = TRUE, int_conformal_cv(grid_res, two_models))
  grid_int <- int_conformal_cv(grid_res, two_models[1, ])
  grid_bounds <- predict(grid_int, sim_small)
  grid_bounds_90 <- predict(grid_int, sim_small, level = .9)
  expect_equal(names(grid_bounds), c(".pred_lower", ".pred", ".pred_upper"))
  expect_equal(nrow(grid_bounds), nrow(sim_small))
  expect_true(mean(complete.cases(grid_bounds)) == 1)
  expect_true(
    all(grid_bounds$.pred_lower < grid_bounds_90$.pred_lower)
  )

  expect_identical(
    required_pkgs(grid_int),
    c(unique(unlist(map(grid_int$models, required_pkgs))), "probably")
  )
  expect_identical(
    required_pkgs(grid_int, infra = FALSE),
    unique(unlist(map(grid_int$models, required_pkgs, infra = FALSE)))
  )
})


test_that("group resampling to conformal CV intervals", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("nnet")

  make_data <- function(n, std_dev = 1 / 5) {
    tibble(x = runif(n, min = -1)) |>
      mutate(
        y = (x^3) + 2 * exp(-6 * (x - 0.3)^2),
        y = y + rnorm(n, sd = std_dev)
      )
  }

  n <- 100
  set.seed(8383)
  train_data <- make_data(n) |>
    mutate(color = sample(c('red', 'blue'), n(), replace = TRUE))

  set.seed(484)
  nnet_wflow <-
    workflow(
      y ~ x,
      parsnip::mlp(hidden_units = 2) |> parsnip::set_mode("regression")
    )

  group_folds <- group_vfold_cv(train_data, group = color)

  ctrl <- control_resamples(save_pred = TRUE, extract = I)

  group_nnet_rs <-
    nnet_wflow |>
    fit_resamples(group_folds, control = ctrl)

  expect_snapshot_warning(int_conformal_cv(group_nnet_rs))
})
