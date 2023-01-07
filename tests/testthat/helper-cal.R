.cal_env <- new.env()

testthat_cal_tune_results <- function() {
  ret <- .cal_env$tune_results

  if(is.null(ret)) {

    ret_file <- test_path("cal_files/binary_sim.rds")

    if(!file.exists(ret_file)) {

      if(!dir.exists(test_path("cal_files"))) {
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
      saveRDS(ret, ret_file)
    } else {
      ret <- readRDS(ret_file)
    }
    .cal_env$tune_results <- ret
    cp <- tune::collect_predictions(ret, summarize = TRUE)
    .cal_env$tune_results_count <- nrow(cp)
  }

  ret
}

testthat_cal_tune_results_count <- function() {
  ret <- .cal_env$tune_results_count
  if(is.null(ret)) {
    invisible(testthat_cal_tune_results())
    ret <- .cal_env$tune_results_count
  }
  ret
}

testthat_cal_sampled <- function() {
  ret <- .cal_env$resampled_data
  if(is.null(ret)) {
    set.seed(100)
    ret <- rsample::vfold_cv(segment_logistic)
    .cal_env$resampled_data <- ret
  }
  ret
}

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
