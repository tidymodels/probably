#--------------------------------- Methods -------------------------------------
#------------------------------- >> Isotonic  ----------------------------------
#' Uses an Isotonic regression model to calibrate model predictions.
#' @inheritParams cal_estimate_logistic
#' @details This function uses [stats::isoreg()] to create obtain the calibration
#' values for binary classification or numeric regression.
#' @template multiclass
#' @references
#' Zadrozny, Bianca and Elkan, Charles. (2002). Transforming Classifier Scores
#' into Accurate Multiclass Probability Estimates. _Proceedings of the ACM SIGKDD
#' International Conference on Knowledge Discovery and Data Mining._
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_validate_isotonic()]
#' @examples
#' # ------------------------------------------------------------------------------
#' # Binary Classification
#'
#' # It will automatically identify the probability columns
#' # if passed a model fitted with tidymodels
#' cal_estimate_isotonic(segment_logistic, Class)
#'
#' # Specify the variable names in a vector of unquoted names
#' cal_estimate_isotonic(segment_logistic, Class, c(.pred_poor, .pred_good))
#'
#' # dplyr selector functions are also supported
#' cal_estimate_isotonic(segment_logistic, Class, dplyr::starts_with(".pred_"))
#'
#' # ------------------------------------------------------------------------------
#' # Regression (numeric outcomes)
#'
#' cal_estimate_isotonic(boosting_predictions_oob, outcome, .pred)
#' @export
cal_estimate_isotonic <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...
) {
  UseMethod("cal_estimate_isotonic")
}

#' @export
#' @rdname cal_estimate_isotonic
cal_estimate_isotonic.data.frame <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...,
  .by = NULL
) {
  stop_null_parameters(parameters)

  info <- get_prediction_data(
    .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    .by = {{ .by }}
  )

  model <- isoreg_fit_over_groups(info, ...)

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "Isotonic regression calibration",
    rows = nrow(info$predictions),
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_isotonic"
  )
}

#' @export
#' @rdname cal_estimate_isotonic
cal_estimate_isotonic.tune_results <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  parameters = NULL,
  ...
) {
  info <- get_tune_data(.data, parameters)

  model <- isoreg_fit_over_groups(info, ...)

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "Isotonic regression calibration",
    rows = nrow(info$predictions),
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_isotonic"
  )
}

#' @export
#' @rdname cal_estimate_isotonic
cal_estimate_isotonic.grouped_df <- function(
  .data,
  truth = NULL,
  estimate = NULL,
  parameters = NULL,
  ...
) {
  abort_if_grouped_df()
}

#------------------ >>  Bootstrapped Isotonic Regression------------------------
#' Uses a bootstrapped Isotonic regression model to calibrate probabilities
#' @param times Number of bootstraps.
#' @inheritParams cal_estimate_logistic
#' @details This function uses [stats::isoreg()] to create obtain the calibration
#' values. It runs [stats::isoreg()] multiple times, and each time with a different
#' seed. The results are saved inside the returned `cal_object`.
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_validate_isotonic_boot()]
#' @template multiclass
#' @examples
#' # It will automatically identify the probability columns
#' # if passed a model fitted with tidymodels
#' cal_estimate_isotonic_boot(segment_logistic, Class)
#' # Specify the variable names in a vector of unquoted names
#' cal_estimate_isotonic_boot(segment_logistic, Class, c(.pred_poor, .pred_good))
#' # dplyr selector functions are also supported
#' cal_estimate_isotonic_boot(segment_logistic, Class, dplyr::starts_with(".pred"))
#' @export
cal_estimate_isotonic_boot <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  times = 10,
  parameters = NULL,
  ...
) {
  UseMethod("cal_estimate_isotonic_boot")
}

# TODO for regression
#  - figure out type of problem by class of `truth`
#  - set default for `estimate` as null and move logic inside with trith info

#' @export
#' @rdname cal_estimate_isotonic_boot
cal_estimate_isotonic_boot.data.frame <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  times = 10,
  parameters = NULL,
  ...,
  .by = NULL
) {
  stop_null_parameters(parameters)

  info <- get_prediction_data(
    .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    .by = {{ .by }}
  )

  model <- isoreg_fit_over_groups(info, times = times, ...)

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "Bootstrapped isotonic regression calibration",
    rows = nrow(info$predictions),
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_isotonic_boot"
  )

}

#' @export
#' @rdname cal_estimate_isotonic_boot
cal_estimate_isotonic_boot.tune_results <- function(
  .data,
  truth = NULL,
  estimate = dplyr::starts_with(".pred"),
  times = 10,
  parameters = NULL,
  ...
) {

  info <- get_tune_data(.data, parameters)

  model <- isoreg_fit_over_groups(info, times = times, ...)

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "Bootstrapped isotonic regression calibration",
    rows = nrow(info$predictions),
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_isotonic_boot"
  )

}

#' @export
#' @rdname cal_estimate_isotonic_boot
cal_estimate_isotonic_boot.grouped_df <- function(
  .data,
  truth = NULL,
  estimate = NULL,
  times = 10,
  parameters = NULL,
  ...
) {
  abort_if_grouped_df()
}

#------------------------------ Implementation ---------------------------------

isoreg_fit_over_groups <- function(info, times = 1, ...) {
  grp_df <- make_group_df(info$predictions, group = info$group)
  nst_df <- vctrs::vec_split(x = info$predictions, by = grp_df)
  fltrs <- make_cal_filters(nst_df$key)

  # ensemble here

  fits <-
    lapply(
      nst_df$val,
      fit_ensemble_isoreg_models,
      truth = info$truth,
      estimate = info$estimate,
      times = times,
      ...
    )

  purrr::map2(fits, fltrs, ~ list(filter = .y, estimates = .x))
}

fit_ensemble_isoreg_models <- function(
    .data,
    truth = NULL,
    estimate = NULL,
    times = 1,
    ...
) {

  is_sampled <- times > 1

  iso_models <- purrr::map(
    sample.int(10000, times),
    ~ withr::with_seed(
      .x,
      {
        fit_all_isoreg_models(
          .data,
          truth = truth,
          estimate = estimate,
          sampled = is_sampled,
          ...
        )
      }
    )
  )
  iso_models
}

fit_all_isoreg_models <- function(
    .data,
    truth = NULL,
    estimate = NULL,
    sampled = FALSE,
    ...
) {
  lvls <- levels(.data[[truth]])
  num_lvls <- length(lvls)

  if (num_lvls == 0 | num_lvls == 2) {
    res <- fit_isoreg_model(
      .data,
      truth = truth,
      estimate = estimate,
      sampled = sampled,
      ...
    )
    res <- list(res)
    names(res) <- estimate[1]
  } else {
    # 1-versus-all, loop over classes and redefine
    res <- fit_over_classes(
      fit_isoreg_model,
      .data = .data,
      truth = truth,
      estimate = estimate,
      sampled = sampled,
      ...
    )
    names(res) <- estimate
  }
  res
}

fit_isoreg_model <- function(
  .data,
  truth,
  estimate,
  sampled = FALSE,
  ...
) {

  estimate <- estimate[1]
  sorted_data <- dplyr::arrange(.data, !!rlang::syms(estimate))

  if (sampled) {
    sorted_data <- dplyr::slice_sample(
      .data = sorted_data,
      prop = 1,
      replace = TRUE
    )
  }

  x <- sorted_data[[ estimate ]]
  y <- sorted_data[[ truth ]]

  if (is.factor(y)) {
    lvls <- levels(y)
    y <- ifelse(y == lvls[1], 1, 0)
  }

  model <- stats::isoreg(x = x, y = y)

  model_stepfun <- as.stepfun(model, ...)

  dplyr::tibble(
    .estimate = environment(model_stepfun)$x,
    .adj_estimate = environment(model_stepfun)$y
  )
}
