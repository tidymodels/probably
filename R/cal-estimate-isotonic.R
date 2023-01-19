#--------------------------------- Methods -------------------------------------
#------------------------------- >> Isotonic  ----------------------------------
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
                                  parameters = NULL,
                                  ...) {
  UseMethod("cal_estimate_isotonic")
}

#' @export
#' @rdname cal_estimate_isotonic
cal_estimate_isotonic.data.frame <- function(.data,
                                             truth = NULL,
                                             estimate = dplyr::starts_with(".pred_"),
                                             parameters = NULL,
                                             ...) {
  stop_null_parameters(parameters)
  cal_isoreg_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    source_class = cal_class_name(.data),
    ...
  )
}

#' @export
#' @rdname cal_estimate_isotonic
cal_estimate_isotonic.tune_results <- function(.data,
                                               truth = NULL,
                                               estimate = dplyr::starts_with(".pred_"),
                                               parameters = NULL,
                                               ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = NULL,
    event_level = "first",
    parameters = parameters,
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
                                       parameters = NULL,
                                       ...) {
  UseMethod("cal_estimate_isotonic_boot")
}

# TODO for regression
#  - figure out type of problem by class of `truth`
#  - set default for `estimate` as null and move logic inside with trith info


#' @export
#' @rdname cal_estimate_isotonic_boot
cal_estimate_isotonic_boot.data.frame <- function(.data,
                                                  truth = NULL,
                                                  estimate = dplyr::starts_with(".pred_"),
                                                  times = 10,
                                                  parameters = NULL,
                                                  ...) {
  stop_null_parameters(parameters)
  cal_isoreg_impl(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    times = times,
    source_class = cal_class_name(.data),
    ...
  )
}

#' @export
#' @rdname cal_estimate_isotonic_boot
cal_estimate_isotonic_boot.tune_results <- function(.data,
                                                    truth = NULL,
                                                    estimate = dplyr::starts_with(".pred_"),
                                                    times = 10,
                                                    parameters = NULL,
                                                    ...) {
  tune_args <- tune_results_args(
    .data = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    group = NULL,
    event_level = "first", # or null for regression
    parameters = parameters,
    ...
  )

  tune_args$predictions %>%
    dplyr::group_by(!!tune_args$group) %>%
    cal_isoreg_impl(
      truth = !!tune_args$truth,
      estimate = !!tune_args$estimate,
      times = times,
      source_class = cal_class_name(.data),
      ...
    )
}

#------------------------------ Implementation ---------------------------------
cal_isoreg_impl <- function(.data,
                            truth,
                            estimate,
                            sampled = FALSE,
                            times = 1,
                            source_class = NULL,
                            ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  levels <- truth_estimate_map(.data, !!truth, !!estimate)

  if (length(levels) == 2) {
    proc_levels <- levels[1]
  } else {
    proc_levels <- levels
  }

  if (times == 1) {
    iso_model <- cal_isoreg_impl_grp(
      .data = .data,
      truth = !!truth,
      estimate = proc_levels,
      sampled = TRUE
    )
    iso_model <- list(iso_model)
    addl_class <- "cal_estimate_isotonic" # maybe add cls or reg class here
    method <- "Isotonic"
  } else {
    iso_model <- purrr::map(
      sample.int(10000, times),
      ~ withr::with_seed(
        .x,
        {
          cal_isoreg_impl_grp(
            .data = .data,
            truth = !!truth,
            estimate = proc_levels,
            sampled = TRUE
          )
        }
      )
    )
    addl_class <- "cal_estimate_isotonic_boot"
    method <- "Bootstrapped Isotonic Regression"
  }


  iso_flip <- map(
    seq_len(length(iso_model[[1]])),
    ~ {
      x <- .x
      map(iso_model, ~ .x[[x]])
    }
  ) %>%
    purrr:::map(
      ~ {
        x <- .x
        list(
          filter = x[[1]]$filter,
          estimates = map(x, ~ .x[[2]])[[1]]
        )
      }
    )

  as_cal_object(
    estimate = iso_flip,
    levels = levels,
    truth = !!truth,
    method = method,
    rows = nrow(.data),
    source_class = source_class,
    additional_class = addl_class
  )

}

cal_isoreg_impl_grp <- function(.data, truth, estimate, sampled, ...) {
  .data %>%
    split_dplyr_groups() %>%
    lapply(
      function(x) {
        iso_model <- cal_isoreg_impl_estimate(
          .data = x$data,
          truth = {{ truth }},
          estimate = estimate,
          sampled = sampled,
          ... = ...
        ) %>%
          rlang::set_names(as.character(estimate))

        list(
          filter = x$filter,
          estimate = iso_model
        )
      }
    )
}

cal_isoreg_impl_estimate <- function(.data,
                                     truth,
                                     estimate,
                                     sampled = FALSE,
                                     ...) {
  lapply(
    seq_along(estimate),
    function(x) {
      cal_isoreg_impl_single(
        .data = .data,
        truth = {{ truth }},
        estimate = estimate,
        level = x,
        sampled = sampled,
        ...
      )
    }
  )
}


cal_isoreg_impl_single <- function(.data,
                                   truth,
                                   estimate,
                                   level,
                                   sampled = FALSE,
                                   ...) {

  estimate <- estimate[[level]]
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
  y <- as.integer(as.integer(truth) == level)

  model <- stats::isoreg(x = x, y = y)

  model_stepfun <- as.stepfun(model, ...)

  dplyr::tibble(
    .estimate = environment(model_stepfun)$x,
    .adj_estimate = environment(model_stepfun)$y
  )
}
