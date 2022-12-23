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
#' @details
#' This function uses existing modeling functions from other packages to create
#' the calibration:
#' - `stats::glm()` is used when `smooth` is set to `FALSE`
#' - `mgcv::gam()` is used when `smooth` is set to `TRUE`
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
  cal_logistic_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    smooth = smooth,
    ...
  )
}

#' @export
cal_estimate_logistic.tune_results <- function(.data,
                                               truth = NULL,
                                               estimate = dplyr::starts_with(".pred_"),
                                               smooth = TRUE,
                                               ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = NULL,
    event_level = "first",
    ...
  )

  tune_args$predictions %>%
    dplyr::group_by(!!tune_args$group) %>%
    cal_logistic_impl(
      truth = !!tune_args$truth,
      estimate = !!tune_args$estimate,
      smooth = smooth,
      ...
    )
}

#----------------------------- >> Isotonic -------------------------------------
#' Uses an Isotonic regression model to calibrate probabilities
#' @inheritParams cal_estimate_logistic
#' @details This function uses `stats::isoreg()` to create obtain the calibration
#' values.
#' @references
#' Zadrozny, Bianca and Elkan, Charles. (2002). Transforming Classifier Scores
#' into Accurate Multiclass Probability Estimates. _Proceedings of the ACM SIGKDD
#' International Conference on Knowledge Discovery and Data Mining._
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
  cal_isoreg_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    ...
  )
}

#' @export
cal_estimate_isotonic.tune_results <- function(.data,
                                               truth = NULL,
                                               estimate = dplyr::starts_with(".pred_"),
                                               ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = NULL,
    event_level = "first",
    ...
  )

  tune_args$predictions %>%
    dplyr::group_by(!!tune_args$group) %>%
    cal_isoreg_impl(
      truth = !!tune_args$truth,
      estimate = !!tune_args$estimate,
      ...
    )
}

#------------------ >>  Bootstrapped Isotonic Regression------------------------
#' Uses a bootstrapped Isotonic regression model to calibrate probabilities
#' @param times Number of bootstraps.
#' @inheritParams cal_estimate_logistic
#' @details This function uses `stats::isoreg()` to create obtain the calibration
#' values. It runs `isoreg()` multiple times, and each time with a different
#' seed. The results are saved inside the returned `cal_object`.
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
  cal_isoreg_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    times = times,
    ...
  )
}

#' @export
cal_estimate_isotonic_boot.tune_results <- function(.data,
                                                    truth = NULL,
                                                    estimate = dplyr::starts_with(".pred_"),
                                                    times = 10,
                                                    ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = NULL,
    event_level = "first",
    ...
  )

  tune_args$predictions %>%
    dplyr::group_by(!!tune_args$group) %>%
    cal_isoreg_impl(
      truth = !!tune_args$truth,
      estimate = !!tune_args$estimate,
      times = times,
      ...
    )
}

#------------------------------- >> Beta --------------------------------------
#' Uses a Beta calibration model to calculate new probabilities
#' @param shape_params Number of shape parameters to use. Accepted values are
#' 1 and 2. Defaults to 2.
#' @param location_params Number of location parameters to use. Accepted values
#' 1 and 0. Defaults to 1.
#' @inheritParams cal_estimate_logistic
#' @details  This function uses the `betcal::beta_calibration()` function, and
#' retains the resulting model.
#' @references Meelis Kull, Telmo M. Silva Filho, Peter Flach "Beyond sigmoids:
#' How to obtain well-calibrated probabilities from binary classifiers with beta
#' calibration," _Electronic Journal of Statistics_ 11(2), 5052-5080, (2017)
#' @examples
#' # It will automatically identify the probability columns
#' # if passed a model fitted with tidymodels
#' cal_estimate_beta(segment_logistic, Class)
#' @export
cal_estimate_beta <- function(.data,
                              truth = NULL,
                              shape_params = 2,
                              location_params = 1,
                              estimate = dplyr::starts_with(".pred_"),
                              ...) {
  UseMethod("cal_estimate_beta")
}

#' @export
cal_estimate_beta.data.frame <- function(.data,
                                         truth = NULL,
                                         shape_params = 2,
                                         location_params = 1,
                                         estimate = dplyr::starts_with(".pred_"),
                                         ...) {
  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, {{ truth }}, {{ estimate }})

  if (length(levels) == 2) {
    x_factor <- dplyr::pull(.data, !!truth)
    x <- x_factor == names(levels[1])
    y <- dplyr::pull(.data, !!levels[[1]])

    parameters <- NULL

    if (shape_params == 1) {
      parameters <- "a"
    }

    if (shape_params == 2) {
      parameters <- "ab"
    }

    if (location_params == 1) {
      parameters <- paste0(parameters, "m")
    }

    if (location_params > 1) {
      rlang::abort("Invalid `location_params`, allowed values are 1 and 0")
    }

    if (is.null(parameters)) {
      rlang::abort("Invalid `shape_params`, allowed values are 1 and 2")
    }

    prevent_output <- utils::capture.output(
      beta_model <- invisible(betacal::beta_calibration(
        p = y,
        y = x,
        parameters = parameters
      ))
    )

    beta_model$model <- butcher::butcher(beta_model$model)

    res <- as_binary_cal_object(
      estimate = beta_model,
      levels = levels,
      truth = !!truth,
      method = "Beta",
      rows = nrow(.data),
      additional_class = "cal_estimate_beta"
    )
  } else {
    stop_multiclass()
  }

  res
}

#------------------------- Estimate implementation -----------------------------
#------------------------------ >> Logistic ------------------------------------
cal_logistic_impl <- function(.data,
                              truth = NULL,
                              estimate = dplyr::starts_with(".pred_"),
                              type,
                              smooth,
                              ...) {
  if (smooth) {
    model <- "logistic_spline"
    method <- "Logistic Spline"
    additional_class <- "cal_estimate_logistic_spline"
  } else {
    model <- "glm"
    method <- "Logistic"
    additional_class <- "cal_estimate_logistic"
  }

  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }})

  if (length(levels) == 2) {
    log_model <- cal_logistic_impl_grp(
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
      rows = nrow(.data),
      additional_class = additional_class
    )
  } else {
    stop_multiclass()
  }

  res
}

cal_logistic_impl_grp <- function(.data, truth, estimate, run_model, group, ...) {
  .data %>%
    dplyr::group_by({{ group }}, .add = TRUE) %>%
    split_dplyr_groups() %>%
    lapply(
      function(x) {
        estimate <- cal_logistic_impl_single(
          .data = x$data,
          truth = {{ truth }},
          estimate = estimate,
          run_model = run_model,
          ... = ...
        )
        list(
          filter = x$filter,
          estimate = estimate
        )
      }
    )
}

cal_logistic_impl_single <- function(.data, truth, estimate, run_model, ...) {
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
cal_isoreg_impl <- function(.data,
                            truth,
                            estimate,
                            sampled = FALSE,
                            times = 1,
                            ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  levels <- truth_estimate_map(.data, !!truth, !!estimate)

  if (length(levels) == 2) {
    iso_model <- purrr::map(
      sample.int(10000, times),
      ~ withr::with_seed(
        .x,
        {
          cal_isoreg_impl_grp(
            .data = .data,
            truth = !!truth,
            estimate = !!levels[[1]],
            sampled = TRUE
          )
        }
      )
    )

    len_iso <- length(iso_model[[1]])

    iso_flip <- map(
      seq_len(len_iso),
      ~ {
        x <- .x
        map(iso_model, ~ .x[[x]])
      }
    ) %>%
      map(
        ~ {
          x <- .x
          list(
            filter = x[[1]]$filter,
            estimates = map(x, ~ .x[[2]])
          )
        }
      )

    res <- as_binary_cal_object(
      estimate = iso_flip,
      levels = levels,
      truth = !!truth,
      method = "Isotonic",
      rows = nrow(.data),
      additional_class = "cal_estimate_isotonic"
    )
  } else {
    stop_multiclass()
  }

  res
}

cal_isoreg_impl_grp <- function(.data, truth, estimate, sampled, ...) {
  .data %>%
    split_dplyr_groups() %>%
    lapply(
      function(x) {
        iso_model <- cal_isoreg_impl_single(
          .data = x$data,
          truth = {{ truth }},
          estimate = {{ estimate }},
          sampled = sampled,
          ... = ...
        )
        list(
          filter = x$filter,
          estimate = iso_model
        )
      }
    )
}

cal_isoreg_impl_single <- function(.data,
                                   truth,
                                   estimate,
                                   sampled = FALSE,
                                   ...) {
  estimate <- enquo(estimate)

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

#-------------------------- Binary Objects -------------------------------------

#' @export
print.cal_binary <- function(x, ...) {
  print_cal_binary(x, ...)
}

#' @export
print.cal_estimate_isotonic <- function(x, ...) {
  print_cal_binary(x, upv = TRUE, ...)
}

print_cal_binary <- function(x, upv = FALSE, ...) {
  cli::cli_div(theme = list(
    span.val0 = list(color = "blue"),
    span.val1 = list(color = "yellow"),
    span.val2 = list(color = "darkgreen")
  ))
  rows <- prettyNum(x$rows, ",")
  cli::cli_h3("Probability Calibration")
  cli::cli_text("Method: {.val2 {x$method}}")
  cli::cli_text("Type: {.val2 Binary}")
  cli::cli_text("Train set size: {.val2 {rows}}")
  if (upv) {
    upv_no <- prettyNum(nrow(x$estimates[[1]]$estimate), ",")
    cli::cli_text("Unique Probability Values: {.val2 {upv_no}}")
  }
  cli::cli_text("Truth variable: `{.val0 {x$truth}}`")
  cli::cli_text("Estimate variables:")
  cli::cli_text("{.val1 `{x$levels[[1]]}`} ==> {.val0 {names(x$levels[1])}}")
  cli::cli_text("{.val1 `{x$levels[[2]]}`} ==> {.val0 {names(x$levels[2])}}")
  cli::cli_end()
}

as_binary_cal_object <- function(estimate,
                                 truth,
                                 levels,
                                 method,
                                 additional_class = NULL,
                                 rows) {
  truth_name <- as_name(enquo(truth))

  structure(
    list(
      type = "binary",
      method = method,
      truth = truth_name,
      levels = levels,
      rows = rows,
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

# dplyr::group_map() does not pass the parent function's `...`, it overrides it
# and there seems to be no way to change it. This function will split the the
# data set by all the combination of the grouped variables. It will respect
# any tidyeval variable calls made prior to calling the calibration
split_dplyr_groups <- function(.data) {
  if (dplyr::is_grouped_df(.data)) {
    .data %>%
      dplyr::summarise(.groups = "drop") %>%
      purrr::transpose() %>%
      purrr::map(~ {
        purrr::imap(.x, ~ expr(!!parse_expr(.y) == !!.x)) %>%
          purrr::reduce(function(x, y) expr(!!x & !!y))
      }) %>%
      purrr::map(~ {
        list(
          data = .data %>%
            dplyr::filter(, !!.x) %>%
            dplyr::ungroup(),
          filter = .x
        )
      })
  } else {
    list(list(data = .data))
  }
}
