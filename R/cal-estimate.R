#------------------------------ Estimates --------------------------------------
#----------------------------- >> Logistic --------------------------------------
#' Uses a logistic regression model to calibrate probabilities
#' @param .data A data.frame object containing predictions and probability columns.
#' @param truth The column identifier for the true class results
#' (that is a factor). This should be an unquoted column name.
#' @param estimate A vector of column identifiers, or one of `dplyr` selector
#' functions to choose which variables contains the class probabilities. It
#' defaults to the prefix used by tidymodels (`.pred_`). The order of the
#' identifiers will be considered the same as the order of the levels of the
#' `truth` variable.
#' @param ... Additional arguments passed to the models or routines used to
#' calculate the new probabilities.
#' @param smooth Applies to the logistic models. It switches between logistic
#' spline when `TRUE`, and simple logistic regression when `FALSE`.
#' @examples
#' # It will automatically identify the probability columns
#' # if passed a model fitted with tidymodels
#' cal_estimate_logistic(segment_logistic, Class)
#' # Specify the variable names in a vector of unquoted names
#' cal_estimate_logistic(segment_logistic, Class, c(.pred_poor, .pred_good))
#' # dplyr selector functions are also supported
#' cal_estimate_logistic(segment_logistic, Class, dplyr::starts_with(".pred_"))
#' @export
cal_estimate_logistic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  smooth = TRUE,
                                  ...) {
  UseMethod("cal_estimate_logistic")
}

#' @export
cal_estimate_logistic.data.frame <- function(.data,
                                             truth = NULL,
                                             estimate = dplyr::starts_with(".pred_"),
                                             smooth = TRUE,
                                             ...) {
  if(smooth) {
    model <- "logistic_spline"
    method <- "Logistic Spline"
    additional_class <- "cal_estimate_logistic_spline"
  } else {
    model <- "glm"
    method <- "Logistic"
    additional_class <- "cal_estimate_logistic"
  }


  cal_estimate_logistic_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    model = model,
    method = method,
    additional_class = additional_class
  )
}


#----------------------------- >> Isotonic -------------------------------------
#' Uses an Isotonic regression model to calibrate probabilities
#' @inheritParams cal_estimate_logistic
#' @examples
#' # It will automatically identify the probability columns
#' # if passed a model fitted with tidymodels
#' cal_estimate_isotonic(segment_logistic, Class)
#' # Specify the variable names in a vector of unquoted names
#' cal_estimate_isotonic(segment_logistic, Class, c(.pred_poor, .pred_good))
#' # dplyr selector functions are also supported
#' cal_estimate_isotonic(segment_logistic, Class, dplyr::starts_with(".pred_"))
#' @export
cal_estimate_isotonic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  ...) {
  UseMethod("cal_estimate_isotonic")
}

#' @export
cal_estimate_isotonic.data.frame <- function(.data,
                                             truth = NULL,
                                             estimate = dplyr::starts_with(".pred_"),
                                             ...) {
  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, {{ truth }}, {{ estimate }})

  if (length(levels) == 2) {
    iso_model <- cal_isoreg_dataframe(
      .data = .data,
      truth = !!truth,
      estimate = levels[[1]],
      ...
    )

    res <- as_binary_cal_object(
      estimate = iso_model,
      levels = levels,
      truth = !!truth,
      method = "Isotonic",
      additional_class = "cal_estimate_isotonic"
    )
  } else {
    stop_multiclass()
  }

  res
}

#------------------ >>  Bootstrapped Isotonic Regression------------------------
#' Uses a bootstrapped Isotonic regression model to calibrate probabilities
#' @param times Number of bootstraps.
#' @inheritParams cal_estimate_logistic
#' @examples
#' # It will automatically identify the probability columns
#' # if passed a model fitted with tidymodels
#' cal_estimate_isotonic_boot(segment_logistic, Class)
#' # Specify the variable names in a vector of unquoted names
#' cal_estimate_isotonic_boot(segment_logistic, Class, c(.pred_poor, .pred_good))
#' # dplyr selector functions are also supported
#' cal_estimate_isotonic_boot(segment_logistic, Class, dplyr::starts_with(".pred_"))
#' @export
cal_estimate_isotonic_boot <- function(.data,
                                       truth = NULL,
                                       estimate = dplyr::starts_with(".pred_"),
                                       times = 10,
                                       ...) {
  UseMethod("cal_estimate_isotonic_boot")
}

#' @export
cal_estimate_isotonic_boot.data.frame <- function(.data,
                                                  truth = NULL,
                                                  estimate = dplyr::starts_with(".pred_"),
                                                  times = 10,
                                                  ...) {
  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }})

  if (length(levels) == 2) {
    log_model <- cal_isoreg_boot(
      .data = .data,
      truth = !!truth,
      estimate = levels[[1]],
      times = times,
      ...
    )

    res <- as_binary_cal_object(
      estimate = log_model,
      levels = levels,
      truth = !!truth,
      method = "Bootstrapped Isotonic Regression",
      additional_class = "cal_estimate_isotonic_boot"
    )
  } else {
    stop_multiclass()
  }

  res
}

#------------------------- Estimate implementation -----------------------------
#------------------------------ >> Logistic ------------------------------------
cal_estimate_logistic_impl <- function(.data,
                                       truth = NULL,
                                       estimate = dplyr::starts_with(".pred_"),
                                       type,
                                       model,
                                       method,
                                       additional_class,
                                       ...) {
  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }})

  if (length(levels) == 2) {
    log_model <- cal_model_impl(
      .data = .data,
      truth = !!truth,
      estimate = levels[[1]],
      run_model = model,
      ...
    )

    res <- as_binary_cal_object(
      estimate = log_model,
      levels = levels,
      truth = !!truth,
      method = method,
      additional_class = additional_class
    )
  } else {
    stop_multiclass()
  }

  res
}

cal_model_impl <- function(.data, truth, estimate, run_model, ...) {
  truth <- ensym(truth)

  if (run_model == "logistic_spline") {
    f_model <- expr(!!truth ~ s(!!estimate))
    init_model <- mgcv::gam(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
  }

  if (run_model == "glm") {
    f_model <- expr(!!truth ~ !!estimate)
    init_model <- glm(f_model, data = .data, family = "binomial", ...)
    model <- butcher::butcher(init_model)
  }

  model
}

#------------------------------ >> Isotonic ------------------------------------
cal_isoreg_dataframe <- function(.data,
                                 truth,
                                 estimate,
                                 sampled = FALSE,
                                 ...) {
  sorted_data <- dplyr::arrange(.data, !!estimate)

  if (sampled) {
    sorted_data <- dplyr::slice_sample(
      .data = sorted_data,
      prop = 1,
      replace = TRUE
    )
  }

  x <- dplyr::pull(sorted_data, !!estimate)

  truth <- dplyr::pull(sorted_data, {{ truth }})
  y <- as.integer(as.integer(truth) == 1)

  model <- stats::isoreg(x = x, y = y)

  model_stepfun <- as.stepfun(model, ...)

  dplyr::tibble(
    .estimate = environment(model_stepfun)$x,
    .adj_estimate = environment(model_stepfun)$y
  )
}

# cal_isoreg_boot() runs boot_iso() as many times specified by `times`.
# Each time it runs, it passes a different seed. boot_iso() then runs a
# single Isotonic model with using withr to set a new seed.

cal_isoreg_boot <- function(.data,
                            truth,
                            estimate,
                            times,
                            ...) {
    purrr::map(
      sample.int(10000, times),
      ~ boot_iso(
        .data = .data,
        truth = {{ truth }},
        estimate = {{ estimate }},
        seed = .x
      )
    )
}

boot_iso <- function(.data, truth, estimate, seed) {
  withr::with_seed(
    seed,
    {
      cal_isoreg_dataframe(
        .data = .data,
        truth = {{ truth }},
        estimate = estimate,
        sampled = TRUE
      )
    }
  )
}

#-------------------------- Binary Objects -------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  cli::cli_div(theme = list(
    span.val0 = list(color = "blue"),
    span.val1 = list(color = "yellow")
  ))
  cli::cli_h3("Probability Calibration")
  cli::cli_text("Method: {.val0 {x$method}}")
  cli::cli_text("Type: {.val0 Binary}")
  cli::cli_text("Truth: `{.val0 {x$truth}}`")
  cli::cli_text("Estimates:")
  cli::cli_text("{.val1 `{x$levels[[1]]}`} ==> {.val0 {names(x$levels[1])}}")
  cli::cli_text("{.val1 `{x$levels[[2]]}`} ==> {.val0 {names(x$levels[2])}}")
  cli::cli_end()
}

as_binary_cal_object <- function(estimate,
                                 truth,
                                 levels,
                                 method,
                                 additional_class = NULL) {
  truth_name <- as_name(enquo(truth))

  structure(
    list(
      type = "binary",
      method = method,
      truth = truth_name,
      levels = levels,
      estimates = estimate
    ),
    class = c(additional_class, "cal_binary", "cal_object")
  )
}

# ------------------------------- Utils ----------------------------------------

stop_multiclass <- function() {
  cli::cli_abort("Multiclass not supported...yet")
}

# Centralizes the figuring out of which probability-variable maps to which
# factor level of the "truth" variable. This is where the logic of finding
# and mapping tidymodels explicit column names happen. If there are no .pred_
# named variables, it will map the variables based on the position.
# It returns a named list, wit the variable names as syms, and the assigned
# levels as the name.
truth_estimate_map <- function(.data, truth, estimate) {
  truth_str <- tidyselect_cols(.data, {{ truth }})

  truth_levels <- levels(.data[[truth_str]])

  estimate_str <- tidyselect_cols(.data, {{ estimate }}) %>%
    names()

  if (length(estimate_str) == 0) {
    cli::cli_abort("{.arg estimate} must select at least one column.")
  }

  if (all(substr(estimate_str, 1, 6) == ".pred_")) {
    est_map <- purrr::map(
      truth_levels,
      ~ sym(estimate_str[paste0(".pred_", .x) == estimate_str])
    )
  } else {
    est_map <- purrr::map(
      seq_along(truth_levels),
      ~ sym(estimate_str[[.x]])
    )
  }

  set_names(est_map, truth_levels)
}

# Wraps tidyselect call to avoid code duplication in the function above
tidyselect_cols <- function(.data, x) {
  tidyselect::eval_select(
    expr = enquo(x),
    data = .data[unique(names(.data))],
    allow_rename = FALSE
  )
}
