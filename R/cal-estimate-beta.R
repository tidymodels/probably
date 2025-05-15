#-------------------------------- Methods --------------------------------------
#' Uses a Beta calibration model to calculate new probabilities
#' @param shape_params Number of shape parameters to use. Accepted values are
#' 1 and 2. Defaults to 2.
#' @param location_params Number of location parameters to use. Accepted values
#' 1 and 0. Defaults to 1.
#' @inheritParams cal_estimate_logistic
#' @details  This function uses the [betacal::beta_calibration()] function, and
#' retains the resulting model.
#' @references Meelis Kull, Telmo M. Silva Filho, Peter Flach "Beyond sigmoids:
#' How to obtain well-calibrated probabilities from binary classifiers with beta
#' calibration," _Electronic Journal of Statistics_ 11(2), 5052-5080, (2017)
#' @template multiclass
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_validate_beta()]
#' @examples
#' if (rlang::is_installed("betacal")) {
#'  # It will automatically identify the probability columns
#'   # if passed a model fitted with tidymodels
#'   cal_estimate_beta(segment_logistic, Class)
#' }
#' @export
cal_estimate_beta <- function(
    .data,
    truth = NULL,
    shape_params = 2,
    location_params = 1,
    estimate = dplyr::starts_with(".pred_"),
    parameters = NULL,
    ...
) {
  UseMethod("cal_estimate_beta")
}

#' @export
#' @rdname cal_estimate_beta
cal_estimate_beta.data.frame <- function(
    .data,
    truth = NULL,
    shape_params = 2,
    location_params = 1,
    estimate = dplyr::starts_with(".pred_"),
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

  model <- beta_fit_over_groups(info, shape_params, location_params, ...)

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "Beta calibration",
    rows = nrow(info$predictions),
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_beta"
  )
}

#' @export
#' @rdname cal_estimate_beta
cal_estimate_beta.tune_results <- function(
    .data,
    truth = NULL,
    shape_params = 2,
    location_params = 1,
    estimate = dplyr::starts_with(".pred_"),
    parameters = NULL,
    ...
) {
  info <- get_tune_data(.data, parameters)

  model <- beta_fit_over_groups(info, shape_params, location_params, ...)

  as_cal_object(
    estimate = model,
    levels = info$map,
    truth = info$truth,
    method = "Beta calibration",
    rows = nrow(info$predictions),
    source_class = cal_class_name(.data),
    additional_classes = "cal_estimate_beta"
  )
}

#' @export
#' @rdname cal_estimate_beta
cal_estimate_beta.grouped_df <- function(
    .data,
    truth = NULL,
    shape_params = 2,
    location_params = 1,
    estimate = NULL,
    parameters = NULL,
    ...
) {
  abort_if_grouped_df()
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_beta <- function(x, ...) {
  c("betacal", "probably")
}

# ----------------------------- Implementation ---------------------------------

beta_fit_over_groups <- function(info, shape_params, location_params, ...) {
  grp_df <- make_group_df(info$predictions, group = info$group)
  nst_df <- vctrs::vec_split(x = info$predictions, by = grp_df)
  fltrs <- make_cal_filters(nst_df$key)

  fits <-
    lapply(
      nst_df$val,
      fit_all_beta_models,
      truth = info$truth,
      shape = shape_params,
      location = location_params,
      estimate = info$estimate,
      ...
    )

  purrr::map2(fits, fltrs, ~ list(filter = .y, estimates = .x))
}


fit_all_beta_models <- function(
    .data,
    truth = NULL,
    shape = 2,
    location = 1,
    estimate = NULL,
    ...
) {
  lvls <- levels(.data[[truth]])
  num_lvls <- length(lvls)

  if (num_lvls == 2) {
    res <- fit_beta_model(
      .data,
      truth = truth,
      shape = shape,
      location = location,
      estimate = estimate,
      ...
    )
    res <- list(res)
    names(res) <- estimate[1]
  } else {
    # 1-versus-all, loop over classes and redefine
    res <- fit_over_classes(
      fit_beta_model,
      .data = .data,
      truth = truth,
      estimate = estimate,
      shape = shape,
      location = location,
      ...
    )
    names(res) <- estimate
  }
  res
}


fit_beta_model <- function(
    .data,
    truth = NULL,
    shape = 2,
    location = 1,
    estimate = NULL,
    ...
) {
  outcome_data <- .data[[truth]]
  lvls <- levels(outcome_data)
  outcome_data <- ifelse(outcome_data == lvls[1], 1, 0)
  prob_data <- .data[[estimate[1]]]

  parameters <- NULL

  if (shape == 1) {
    parameters <- "a"
  }

  if (shape == 2) {
    parameters <- "ab"
  }

  if (location == 1) {
    parameters <- paste0(parameters, "m")
  }

  if (location > 1) {
    cli::cli_abort(
      "Invalid {.arg location_params}, allowed values are 1 and 0."
    )
  }

  if (is.null(parameters)) {
    cli::cli_abort("Invalid {.arg shape_params}, allowed values are 1 and 2.")
  }

  prevent_output <-
    utils::capture.output(
      beta_model <- invisible(betacal::beta_calibration(
        p = prob_data,
        y = outcome_data,
        parameters = parameters
      ))
    )

  beta_model$model <- butcher::butcher(beta_model$model)

  # for easier printing and identification; print method below
  class(beta_model) <- c("betacal", class(beta_model))
  beta_model
}

# TODO cal do we need this?
check_cal_groups <- function(group, .data, call = rlang::env_parent()) {
  group <- enquo(group)
  if (!any(names(.data) == ".config")) {
    return(invisible(NULL))
  }
  num_configs <- length(unique(.data$.config))
  if (num_configs == 1) {
    return(invisible(NULL))
  }
  has_no_groups <- rlang::quo_is_null(group)
  if (has_no_groups) {
    cli::cli_abort(
      c(
        "The data have several values of {.code .config} but no {.code groups}
         argument was passed.",
        "i" = "This will inappropriately pool the data."
      ),
      call = call
    )
  }
  invisible(NULL)
}


#' @export
print.betacal <- function(x, ...) {
  cli::cli_inform("Beta calibration ({x$parameters}) using {x$model$df.null} samples")
  invisible(x)
}
