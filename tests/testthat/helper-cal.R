.cal_ven <- new.env()

testthat_get_tune_results <- function() {
  ret <- .cal_ven$tune_results

  if(is.null(ret)) {
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

    .cal_ven$tune_results <- ret
  }

  ret
}

