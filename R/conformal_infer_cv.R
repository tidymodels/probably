#' Prediction intervals via conformal inference CV+
#'
#' Nonparametric prediction intervals can be computed for fitted regression
#' workflow objects using the CV+ conformal inference method described by
#' Barber _at al_ (2018).
#'
#' @param object An object from a tidymodels resampling or tuning function such
#' as [tune::fit_resamples()], [tune::tune_grid()], or similar. The object
#' should have been produced in a way that the `.extracts` column contains the
#' fitted workflow for each resample (see the Details below).
#' @param parameters An tibble of tuning parameter values that can be
#' used to filter the predicted values before processing. This tibble should
#' select a single set of hyper-parameter values from the tuning results. This is
#' only required when a tuning object is passed to `object`.
#' @param ... Not currently used.
#' @return An object of class `"int_conformal_cv"` containing the information
#' to create intervals. The `predict()` method is used to produce the intervals.
#' @details
#' This function implements the CV+ method found in Section 3 of Barber _at al_
#' (2018). It uses the resampled model fits and their associated holdout
#' residuals to make prediction intervals for regression models.
#'
#' This function prepares the objects for the computations. The [predict()]
#' method computes the intervals for new data.
#'
#' This method was developed for V-fold cross-validation (no repeats). Interval
#' coverage is unknown for any other resampling methods. The function will not
#' stop the computations for other types of resamples, but we have no way of
#' knowing whether the results are appropriate.
#'
#' @seealso [predict.int_conformal_cv()]
#' @references
#' Rina Foygel Barber, Emmanuel J. Candès, Aaditya Ramdas, Ryan J. Tibshirani
#' "Predictive inference with the jackknife+," _The Annals of Statistics_,
#' 49(1), 486-507, 2021
#' @examplesIf !probably:::is_cran_check() & rlang::is_installed(c("modeldata", "parsnip"))
#' library(workflows)
#' library(dplyr)
#' library(parsnip)
#' library(rsample)
#' library(tune)
#' library(modeldata)
#'
#' set.seed(2)
#' sim_train <- sim_regression(200)
#' sim_new <- sim_regression(5) |> select(-outcome)
#'
#' sim_rs <- vfold_cv(sim_train)
#'
#' # We'll use a neural network model
#' mlp_spec <-
#'   mlp(hidden_units = 5, penalty = 0.01) |>
#'   set_mode("regression")
#'
#' # Use a control function that saves the predictions as well as the models.
#' # Consider using the butcher package in the extracts function to have smaller
#' # object sizes
#'
#' ctrl <- control_resamples(save_pred = TRUE, extract = I)
#'
#' set.seed(3)
#' nnet_res <-
#'   mlp_spec |>
#'   fit_resamples(outcome ~ ., resamples = sim_rs, control = ctrl)
#'
#' nnet_int_obj <- int_conformal_cv(nnet_res)
#' nnet_int_obj
#'
#' predict(nnet_int_obj, sim_new)
#' @export
int_conformal_cv <- function(object, ...) {
  UseMethod("int_conformal_cv")
}


#' @export
#' @rdname int_conformal_cv
int_conformal_cv.default <- function(object, ...) {
  cli::cli_abort("No known {.fn int_conformal_cv} methods for this type of object.")
}

#' @export
#' @rdname int_conformal_cv
int_conformal_cv.resample_results <- function(object, ...) {
  check_resampling(object)
  check_extras(object)

  model_list <- .get_fitted_workflows(object)

  y_name <- tune::.get_tune_outcome_names(object)
  resids <-
    tune::collect_predictions(object, summarize = TRUE) |>
    dplyr::mutate(.abs_resid = abs(.pred - !!rlang::sym(y_name)))

  new_infer_cv(model_list, resids$.abs_resid)
}

#' @export
#' @rdname int_conformal_cv
int_conformal_cv.tune_results <- function(object, parameters, ...) {
  check_resampling(object)
  check_parameters(object, parameters)
  check_extras(object)

  model_list <- .get_fitted_workflows(object, parameters)
  y_name <- tune::.get_tune_outcome_names(object)

  resids <-
    tune::collect_predictions(object, parameters = parameters, summarize = TRUE) |>
    dplyr::mutate(.abs_resid = abs(.pred - !!rlang::sym(y_name)))

  new_infer_cv(model_list, resids$.abs_resid)
}

#' @export
#' @rdname predict.int_conformal_full
predict.int_conformal_cv <- function(object, new_data, level = 0.95, ...) {
  mean_pred <-
    purrr::map_dfr(
      object$models,
      ~ predict(.x, new_data) |> parsnip::add_rowindex()
    ) |>
    dplyr::group_by(.row) |>
    dplyr::summarize(estimate = mean(.pred, na.rm = TRUE), .groups = "drop") |>
    purrr::pluck("estimate")
  lower <-
    purrr::map_dbl(
      as.list(seq_along(mean_pred)),
      ~ .get_lower_cv_bound(mean_pred[.x], object$abs_resid, level = level)
    )
  upper <-
    purrr::map_dbl(
      as.list(seq_along(mean_pred)),
      ~ .get_upper_cv_bound(mean_pred[.x], object$abs_resid, level = level)
    )
  dplyr::tibble(.pred_lower = lower, .pred = mean_pred, .pred_upper = upper)
}

#' @export
print.int_conformal_cv <- function(x, ...) {
  cat("Conformal inference via CV+\n")
  cat("preprocessor:", .get_pre_type(x$models[[1]]), "\n")
  cat("model:", .get_fit_type(x$models[[1]]), "\n")
  cat("number of models:", format(length(x$models), big.mark = ","), "\n")
  cat("training set size:", format(length(x$abs_resid), big.mark = ","), "\n\n")

  cat("Use `predict(object, new_data, level)` to compute prediction intervals\n")
  invisible(x)
}

# ------------------------------------------------------------------------------
# helpers

new_infer_cv <- function(models, resid) {
  if (!is.numeric(resid)) {
    cli::cli_abort("Absolute residuals should be numeric.")
  }
  na_resid <- is.na(resid)
  if (all(na_resid)) {
    cli::cli_abort("All of the absolute residuals are missing.")
  }

  if (!is.list(models)) {
    cli::cli_abort("The model list should be... a list.")
  }
  is_wflow <- purrr::map_lgl(models, workflows::is_trained_workflow)
  if (all(!is_wflow)) {
    cli::cli_abort("The {.arg .extracts} argument does not contain fitted workflows.")
  }
  if (any(!is_wflow)) {
    models <- models[is_wflow]
  }

  res <- list(
    models = models,
    abs_resid = resid[!na_resid]
  )
  class(res) <- c("conformal_reg_cv", "int_conformal_cv")
  res
}

.get_lower_cv_bound <- function(pred, resid, level = 0.95) {
  as.vector(stats::quantile(pred - resid, probs = 1 - level))
}

.get_upper_cv_bound <- function(pred, resid, level = 0.95) {
  as.vector(stats::quantile(pred + resid, probs = level))
}

.get_pre_type <- function(x) {
  cls <- x |>
    workflows::extract_preprocessor() |>
    class()
  cls <- cls[!grepl("butchered", cls)]
  cls[1]
}

.get_fit_type <- function(x) {
  fitted <- x |> workflows::extract_fit_parsnip()
  res <- paste0(class(fitted$spec)[1], " (engine = ", fitted$spec$engine, ")")
  res
}

.get_fitted_workflows <- function(x, prm = NULL) {
  if (is.null(prm)) {
    res <- purrr::map(x$.extracts, ~ .x$.extracts[[1]])
  } else {
    by_vars <- names(prm)
    res <-
      x |>
      dplyr::select(.extracts) |>
      tidyr::unnest(.extracts) |>
      dplyr::inner_join(prm, by = by_vars) |>
      purrr::pluck(".extracts")
  }
  res
}

# ------------------------------------------------------------------------------
# checks

check_resampling <- function(x) {
  rs <- attr(x, "rset_info")
  if (any(rs$att$class != "vfold_cv") | any(grepl("group_", rs$att$class))) {
    cli::cli_warn(
      c(
        "The data were resampled using {rs$label}.",
        "i" = "This method was developed for V-fold cross-validation.",
        "i" = "Interval coverage is unknown for your resampling method."
      )
    )
  } else {
    if (rs$att$repeats > 1) {
      cli::cli_warn(
        c(
          "{rs$att$repeats} repeats were used.",
          "i" = "This method was developed for basic V-fold cross-validation.",
          "i" = "Interval coverage is unknown for multiple repeats."
        )
      )
    }
  }
  invisible(NULL)
}

check_parameters <- function(x, param, call = rlang::caller_env()) {
  prms <- tune::.get_tune_parameter_names(x)
  mtr <- tune::collect_metrics(x) |>
    dplyr::distinct(.config, !!!rlang::syms(prms))
  remain <- dplyr::inner_join(mtr, param, by = names(param))
  if (nrow(remain) > 1) {
    cli::cli_abort(
      "The {.arg parameters} argument selected {nrow(remain)} submodels.
      Only 1 should be selected.",
      call = call
    )
  }
  invisible(NULL)
}

check_extras <- function(x, call = rlang::caller_env()) {
  if (!any(names(x) == ".extracts")) {
    cli::cli_abort(
      "The output must contain a column called {.code .extracts} that contains
       the fitted workflow objects. See the documentation on the {.code extract}
       argument of the control function (e.g., {.fn control_grid} or
       {.fn control_resamples}, etc.).",
      call = call
    )
  }
  if (!any(names(x) == ".predictions")) {
    cli::cli_abort(
      "The output must contain a column called {.code .predictions} that
      contains the holdout predictions. See the documentation on the
      {.code save_pred} argument of the control function (e.g.,
      {.fn control_grid} or {.fn control_resamples}, etc.).",
      call = call
    )
  }
  invisible(NULL)
}
