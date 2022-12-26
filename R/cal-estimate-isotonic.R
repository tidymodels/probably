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



#------------------------------ Implementation ---------------------------------
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
    if (times == 1) {
      iso_model <- cal_isoreg_impl_grp(
        .data = .data,
        truth = !!truth,
        estimate = !!levels[[1]],
        sampled = TRUE
      )
      iso_model <- list(iso_model)
      addl_class <- "cal_estimate_isotonic"
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
              estimate = !!levels[[1]],
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
      method = method,
      rows = nrow(.data),
      additional_class = addl_class
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
