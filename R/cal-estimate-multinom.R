#' Uses a Multinomial calibration model to calculate new probabilities
#' @details It uses the `multinom` function, from the `nnet` package, to
#' create the calibration.
#' @inheritParams cal_estimate_logistic
#' @export
cal_estimate_multinomial <- function(.data,
                                     truth = NULL,
                                     estimate = dplyr::starts_with(".pred_"),
                                     ...) {
  UseMethod("cal_estimate_multinomial")
}

#' @export
#' @rdname cal_estimate_beta
cal_estimate_multinomial <- function(.data,
                                     truth = NULL,
                                     estimate = dplyr::starts_with(".pred_"),
                                     ...) {
  #stop_null_parameters(parameters)
  truth <- enquo(truth)
  cal_multinom_impl(
    .data = .data,
    truth = !!truth,
    estimate = {{ estimate }},
    source_class = cal_class_name(.data),
    ...
  )
}

cal_multinom_impl <- function(.data, truth, estimate, source_class, ...) {
  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }})
  model <- cal_multinom_impl_grp(
    .data = .data,
    truth = !!truth,
    levels = levels,
    ...
  )

  as_multi_cal_object(
    estimate = model,
    levels = levels,
    truth = !!truth,
    method = "multinomial",
    rows = nrow(.data),
    additional_class = "cal_estimate_multinomial",
    source_class = source_class
  )
}


cal_multinom_impl_grp <- function(.data, truth, levels, ...) {
  truth <- enquo(truth)
  .data %>%
    split_dplyr_groups() %>%
    lapply(
      function(x) {
        estimate <- cal_multinom_impl_single(
          .data = x$data,
          truth = !!truth,
          levels = levels,
          ... = ...
        )
        list(
          filter = x$filter,
          estimate = estimate
        )
      }
    )
}

cal_multinom_impl_single <- function(.data,
                                     truth = NULL,
                                     levels = NULL,
                                     ...) {
  truth <- enquo(truth)

  levels_formula <- purrr::reduce(levels, function(x, y) expr(!!x + !!y))

  f_model <- expr(!!ensym(truth) ~ !!levels_formula)

  prevent_output <- utils::capture.output(
    model <- nnet::multinom(formula = f_model, data = .data)
  )

  model
}
