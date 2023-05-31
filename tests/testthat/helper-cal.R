# ---------------------------- Test data sets ----------------------------------
# ------------------------------ >> Binary -------------------------------------
.cal_env <- new.env()

testthat_cal_binary <- function() {
  ret <- .cal_env$tune_results

  if (is.null(ret)) {
    ret_file <- test_path("cal_files/binary_sim.rds")

    if (!file.exists(ret_file)) {
      if (!dir.exists(test_path("cal_files"))) {
        dir.create(test_path("cal_files"))
      }

      set.seed(111)

      sim_data <- modeldata::sim_classification(500)

      rec <- recipes::recipe(class ~ ., data = sim_data) %>%
        recipes::step_ns(linear_01, deg_free = tune::tune("linear_01"))

      ret <- tune::tune_grid(
        object = parsnip::set_engine(parsnip::logistic_reg(), "glm"),
        preprocessor = rec,
        resamples = rsample::vfold_cv(sim_data, v = 2, repeats = 3),
        control = tune::control_resamples(save_pred = TRUE)
      )
      saveRDS(ret, ret_file, version = 2)
    } else {
      ret <- readRDS(ret_file)
    }
    .cal_env$tune_results <- ret
    cp <- tune::collect_predictions(ret, summarize = TRUE)
    .cal_env$tune_results_count <- nrow(cp)
  }

  ret
}

testthat_cal_binary_count <- function() {
  ret <- .cal_env$tune_results_count
  if (is.null(ret)) {
    invisible(testthat_cal_binary())
    ret <- .cal_env$tune_results_count
  }
  ret
}

testthat_cal_sampled <- function() {
  ret <- .cal_env$resampled_data
  if (is.null(ret)) {
    set.seed(100)
    ret <- rsample::vfold_cv(segment_logistic)
    .cal_env$resampled_data <- ret
  }
  ret
}

# -------------------------- >> Multiclass (Tune) ------------------------------

testthat_cal_multiclass <- function() {
  ret <- .cal_env$tune_results_multi

  if (is.null(ret)) {
    ret_file <- test_path("cal_files/multiclass_ames.rds")

    if (!file.exists(ret_file)) {
      if (!dir.exists(test_path("cal_files"))) {
        dir.create(test_path("cal_files"))
      }

      set.seed(111)

      df <- sim_multinom_df(500)

      ranger_recipe <- recipes::recipe(
        formula = class ~ .,
        data = df
      )

      ranger_spec <- parsnip::rand_forest(
        mtry = tune(),
        min_n = tune(),
        trees = 200
      ) %>%
        parsnip::set_mode("classification") %>%
        parsnip::set_engine("ranger")

      ret <- tune::tune_grid(
        object = ranger_spec,
        preprocessor = ranger_recipe,
        resamples = rsample::vfold_cv(df, v = 2, repeats = 3),
        control = tune::control_resamples(save_pred = TRUE)
      )

      saveRDS(ret, ret_file, version = 2)
    } else {
      ret <- readRDS(ret_file)
    }
    .cal_env$tune_results_multi <- ret
    cp <- tune::collect_predictions(ret, summarize = TRUE)
    .cal_env$tune_results_multi_count <- nrow(cp)
  }

  ret
}

testthat_cal_multiclass_count <- function() {
  ret <- .cal_env$tune_results_multi_count
  if (is.null(ret)) {
    invisible(testthat_cal_multiclass())
    ret <- .cal_env$tune_results_multi_count
  }
  ret
}

# -------------------------- >> Multiclass (Sim) -------------------------------

testthat_cal_sim_multi <- function() {
  x <- "sim_multi"
  ret <- .cal_env[[x]]

  if (is.null(ret)) {
    pt <- paste0("cal_files/", x, ".rds")

    ret_file <- test_path(pt)

    if (!file.exists(ret_file)) {
      if (!dir.exists(test_path("cal_files"))) {
        dir.create(test_path("cal_files"))
      }

      set.seed(1)
      train <- sim_multinom_df(200)
      test <- sim_multinom_df()

      model <- randomForest::randomForest(class ~ ., train)

      ret <- model %>%
        predict(test, type = "prob") %>%
        as.data.frame() %>%
        dplyr::rename_all(~ paste0(".pred_", .x)) %>%
        dplyr::mutate(class = test$class)

      saveRDS(ret, ret_file, version = 2)
    } else {
      ret <- readRDS(ret_file)
    }
    .cal_env[[x]] <- ret
  }
  ret
}


testthat_cal_reg <- function() {
  ret <- .cal_env$reg_tune_results

  if(is.null(ret)) {

    ret_file <- test_path("cal_files/reg_sim.rds")

    if(!file.exists(ret_file)) {

      if(!dir.exists(test_path("cal_files"))) {
        dir.create(test_path("cal_files"))
      }

      set.seed(111)

      sim_data <- modeldata::sim_regression(100)[, 1:3]

      rec <- recipes::recipe(outcome ~ ., data = sim_data) %>%
        recipes::step_ns(predictor_01, deg_free = tune::tune("predictor_01"))

      ret <- tune::tune_grid(
        object = parsnip::linear_reg(),
        preprocessor = rec,
        resamples = rsample::bootstraps(sim_data, times = 3),
        control = tune::control_resamples(save_pred = TRUE)
      )
      saveRDS(ret, ret_file, version = 2)
    } else {
      ret <- readRDS(ret_file)
    }
    .cal_env$reg_tune_results <- ret
    cp <- tune::collect_predictions(ret, summarize = TRUE)
    .cal_env$reg_tune_results_count <- nrow(cp)
  }

  ret
}

testthat_cal_reg_count <- function() {
  ret <- .cal_env$reg_tune_results_count
  if(is.null(ret)) {
    invisible(testthat_cal_reg())
    ret <- .cal_env$reg_tune_results_count
  }
  ret
}

testthat_cal_reg_sampled <- function() {
  ret <- .cal_env$resampled_reg_data
  if(is.null(ret)) {
    set.seed(100)
    ret <- rsample::vfold_cv(boosting_predictions_oob)
    .cal_env$resampled_reg_data <- ret
  }
  ret
}

sim_multinom_df <- function(n = 1000) {
  modeldata::sim_multinomial(
    n,
    ~ -0.5 + 0.6 * abs(A),
    ~ ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, -2),
    ~ -0.6 * A + 0.50 * B - A * B
  )
}

testthat_cal_fit_rs <- function() {
  ret <- .cal_env$resample_results

  if(is.null(ret)) {

    ret_file <- test_path("cal_files/fit_rs.rds")

    if(!file.exists(ret_file)) {

      if(!dir.exists(test_path("cal_files"))) {
        dir.create(test_path("cal_files"))
      }
      suppressPackageStartupMessages(library(tune))
      suppressPackageStartupMessages(library(parsnip))
      suppressPackageStartupMessages(library(rsample))

      ctrl <- control_resamples(save_pred = TRUE)

      set.seed(111)
      rs_bin <-
        modeldata::sim_classification(100)[, 1:3] %>%
        dplyr::rename(outcome = class) %>%
        vfold_cv() %>%
        fit_resamples(logistic_reg(), outcome ~ ., resamples = ., control = ctrl)
      set.seed(112)
      rs_mlt <-
        sim_multinom_df(500) %>%
        dplyr::rename(outcome = class) %>%
        vfold_cv() %>%
        fit_resamples(mlp() %>% set_mode("classification"),
                      outcome ~ ., resamples = ., control = ctrl)
      set.seed(113)
      rs_reg <-
        modeldata::sim_regression(100)[, 1:3] %>%
        vfold_cv() %>%
        fit_resamples(linear_reg(), outcome ~ ., resamples = ., control = ctrl)

      ret <- list(binary = rs_bin, multin = rs_mlt, reg = rs_reg)

      saveRDS(ret, file = ret_file, version = 2)
    } else {
      ret <- readRDS(ret_file)
    }
    .cal_env$resample_results <- ret
  }

  ret
}

# --------------------------- Custom Expect Functions --------------------------

expect_cal_type <- function(x, type) {
  expect_equal(x$type, type)
}

expect_cal_method <- function(x, method) {
  expect_equal(x$method, method)
}

expect_cal_estimate <- function(x, class) {
  expect_s3_class(x$estimates[[1]]$estimate, class)
}

expect_cal_rows <- function(x, n = 1010) {
  expect_equal(x$rows, n)
}


# ------------------------------------------------------------------------------

save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code

  path
}

expect_snapshot_plot <- function(name, code) {
  skip_on_os("windows")
  skip_on_os("linux")
  skip_on_os("solaris")

  name <- paste0(name, ".png")

  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path <- save_png(code)
  expect_snapshot_file(path, name)
}

has_facet <- function(x) {
  inherits(x$facet, c("FacetWrap", "FacetGrid"))
}

are_groups_configs <- function(x) {
  fltrs <- purrr::map(x$estimates, ~ .x$filter)

  # Check if anything is in the filter slot
  are_null <- purrr::map_lgl(fltrs, ~ all(is.null(.x)))
  if (all(are_null)) {
    return(FALSE)
  }

  fltr_vars <- purrr::map(fltrs, all.vars)
  are_config <- purrr::map_lgl(fltr_vars, ~ identical(.x, ".config"))
  all(are_config)
}

bin_with_configs <- function() {
  set.seed(1)
  segment_logistic %>%
    dplyr::mutate(.config = sample(letters[1:2], nrow(segment_logistic), replace = TRUE))
}

mnl_with_configs <- function() {
  data("hpc_cv", package = "modeldata")

  set.seed(1)
  hpc_cv %>%
    dplyr::mutate(.config = sample(letters[1:2], nrow(hpc_cv), replace = TRUE))
}

reg_with_configs <- function() {
  data("solubility_test", package = "modeldata")

  set.seed(1)

  solubility_test %>%
    dplyr::mutate(.config = sample(letters[1:2], nrow(solubility_test), replace = TRUE))

}

holdout_length <- function(x) {
  length(as.integer(x, data = "assessment"))
}
