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

      df <- modeldata::ames %>%
        dplyr::sample_frac(0.1)

      ranger_recipe <- recipes::recipe(
        formula = Bldg_Type ~ .,
        data = df
      )

      ranger_spec <- parsnip::rand_forest(
        mtry = tune(),
        min_n = tune(),
        trees = 1000
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
      train <- sim_multinom_df()
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

sim_multinom_df <- function() {
  sim_multinom(
    1000,
    ~ -0.5 + 0.6 * abs(A),
    ~ ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, -2),
    ~ -0.6 * A + 0.50 * B - A * B
  )
}

sim_multinom <- function(num_samples, eqn_1, eqn_2, eqn_3,
                         correlation = 0, keep_truth = FALSE) {
  sigma <- matrix(c(1, correlation, correlation, 1), 2, 2)
  eqn_1 <- rlang::get_expr(eqn_1)
  eqn_2 <- rlang::get_expr(eqn_2)
  eqn_3 <- rlang::get_expr(eqn_3)
  dat <-
    data.frame(MASS::mvrnorm(n = num_samples, c(0, 0), sigma)) %>%
    stats::setNames(LETTERS[1:2]) %>%
    dplyr::mutate(
      .formula_1 = rlang::eval_tidy(eqn_1, data = .),
      .formula_2 = rlang::eval_tidy(eqn_2, data = .),
      .formula_3 = rlang::eval_tidy(eqn_3, data = .),
      across(c(dplyr::starts_with(".formula_")), ~ exp(.x))
    )
  probs <- as.matrix(dplyr::select(dat, dplyr::starts_with(".formula_")))
  probs <- t(apply(probs, 1, function(x) x / sum(x)))
  which_class <- function(x) which.max(rmultinom(1, 1, x))
  index <- apply(probs, 1, which_class)
  lvls <- c("one", "two", "three")
  dat$class <- factor(lvls[index], levels = lvls)
  dat <- dat %>% dplyr::select(-dplyr::starts_with(".formula_"))
  if (keep_truth) {
    colnames(probs) <- paste0(".truth_", lvls)
    probs <- tibble::as_tibble(probs)
    dat <- dplyr::bind_cols(dat, probs)
  }
  tibble::as_tibble(dat)
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
