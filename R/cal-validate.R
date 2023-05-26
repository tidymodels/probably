# -------------------------------- Logistic ------------------------------------
#' Measure performance with and without using logistic calibration
#' @description
#' This function uses resampling to measure the effect of calibrating predicted
#' values.
#'
#' @details
#' These functions are designed to calculate performance with and without
#' calibration. They use resampling to measure out-of-sample effectiveness.
#' There are two ways to pass the data in:
#'
#'  - If you have a data frame of predictions, an `rset` object can be created
#'    via \pkg{rsample} functions. See the example below.
#'
#'  - If you have already made a resampling object from the original data and
#'    used it with [tune::fit_resamples()], you can pass that object to the
#'    calibration function and it will use the same resampling scheme. If a
#'    different resampling scheme should be used, run
#'    [tune::collect_predictions()] on the object and use the process in the
#'    previous bullet point.
#'
#' Please note that these functions do not apply to `tune_result` objects. The
#' notion of "validation" implies that the tuning parameter selection has been
#' resolved.
#'
#' @template metrics_cls
#'
#' @param metrics A set of metrics passed created via [yardstick::metric_set()]
#' @param save_pred Indicates whether to a column of post-calibration predictions.
#' @param ... Options to pass to [cal_estimate_logistic()], such as the `smooth`
#' argument.
#' @return The original object with a `.metrics_cal` column and, optionally,
#' an additional `.predictions_cal` column. The class `cal_rset` is also added.
#'
#' @seealso [cal_apply()], [cal_estimate_logistic()]
#' @examples
#'
#' library(dplyr)
#'
#' # ---------------------------------------------------------------------------
#' # classification example
#'
#' segment_logistic %>%
#'   rsample::vfold_cv() %>%
#'   cal_validate_logistic(Class)
#'
#' @inheritParams cal_estimate_logistic
#' @param .data An `rset` object or the results of [tune::fit_resamples()] with
#' a `.predictions` column.
#' @export
cal_validate_logistic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  metrics = NULL,
                                  save_pred = FALSE,
                                  summarize = TRUE,
                                  ...) {
  UseMethod("cal_validate_logistic")
}

#' @export
#' @rdname cal_validate_logistic
cal_validate_logistic.resample_results <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred_"),
           metrics = NULL,
           save_pred = FALSE,
           summarize = TRUE,
           ...) {

    if (!is.null(truth)) {
      rlang::warn("'truth' is automaticaly set when this type of object is used.")
    }
    truth <- tune::.get_tune_outcome_names(.data)
    # Change splits$data to be the predictions instead of the original
    # training data and save as rset
    .data <- convert_resamples(.data)
    cal_validate(
      rset = .data,
      truth = !!truth,
      estimate = {{ estimate }},
      cal_function = "logistic",
      metrics = metrics,
      summarize = summarize,
      save_pred = save_pred,
      ...
    )
  }

#' @export
#' @rdname cal_validate_logistic
cal_validate_logistic.rset <- function(.data,
                                       truth = NULL,
                                       estimate = dplyr::starts_with(".pred_"),
                                       metrics = NULL,
                                       save_pred = FALSE,
                                       summarize = TRUE,
                                       ...) {
  cal_validate(
    rset = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    cal_function = "logistic",
    metrics = metrics,
    summarize = summarize,
    save_pred = save_pred,
    ...
  )
}

#' @export
#' @rdname cal_validate_logistic
cal_validate_logistic.tune_results <- function(.data,
                                               truth = NULL,
                                               estimate = NULL,
                                               metrics = NULL,
                                               save_pred = FALSE,
                                               summarize = TRUE,
                                               ...) {
  abort_if_tune_result()
}

# -------------------------------- Isotonic ------------------------------------
#' Measure performance with and without using isotonic regression calibration
#' @inherit cal_validate_logistic
#' @inheritParams cal_estimate_isotonic
#' @seealso [cal_apply()], [cal_estimate_isotonic()]
#' @template metrics_both
#' @examples
#'
#' library(dplyr)
#'
#' segment_logistic %>%
#'   rsample::vfold_cv() %>%
#'   cal_validate_isotonic(Class)
#'
#' @export
cal_validate_isotonic <- function(.data,
                                  truth = NULL,
                                  estimate = dplyr::starts_with(".pred_"),
                                  metrics = NULL,
                                  save_pred = FALSE,
                                  summarize = TRUE,
                                  ...) {
  UseMethod("cal_validate_isotonic")
}

#' @export
#' @rdname cal_validate_isotonic
cal_validate_isotonic.resample_results <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred_"),
           metrics = NULL,
           save_pred = FALSE,
           summarize = TRUE,
           ...) {

    if (!is.null(truth)) {
      rlang::warn("'truth' is automaticaly set when this type of object is used.")
    }
    truth <- tune::.get_tune_outcome_names(.data)
    # Change splits$data to be the predictions instead of the original
    # training data and save as rset
    .data <- convert_resamples(.data)
    cal_validate(
      rset = .data,
      truth = !!truth,
      estimate = {{ estimate }},
      cal_function = "isotonic",
      metrics = metrics,
      summarize = summarize,
      save_pred = save_pred,
      ...
    )
}

#' @export
#' @rdname cal_validate_isotonic
cal_validate_isotonic.rset <- function(.data,
                                       truth = NULL,
                                       estimate = dplyr::starts_with(".pred_"),
                                       metrics = NULL,
                                       save_pred = FALSE,
                                       summarize = TRUE,
                                       ...) {
  cal_validate(
    rset = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    cal_function = "isotonic",
    metrics = metrics,
    summarize = summarize,
    save_pred = save_pred,
    ...
  )
}

#' @export
#' @rdname cal_validate_isotonic
cal_validate_isotonic.tune_results <- function(.data,
                                               truth = NULL,
                                               estimate = NULL,
                                               metrics = NULL,
                                               save_pred = FALSE,
                                               summarize = TRUE,
                                               ...) {
  abort_if_tune_result()
}

# ----------------------------- Isotonic Boot ----------------------------------
#' Measure performance with and without using bagged isotonic regression calibration
#' @inherit cal_validate_logistic
#' @inheritParams cal_estimate_isotonic_boot
#' @param ... Options to pass to [cal_estimate_isotonic_boot()], such as the
#' `times` argument.
#' @seealso [cal_apply()], [cal_estimate_isotonic_boot()]
#' @template metrics_both
#' @examples
#'
#' library(dplyr)
#'
#' segment_logistic %>%
#'   rsample::vfold_cv() %>%
#'   cal_validate_isotonic_boot(Class)
#'
#' @export
cal_validate_isotonic_boot <- function(.data,
                                       truth = NULL,
                                       estimate = dplyr::starts_with(".pred_"),
                                       metrics = NULL,
                                       save_pred = FALSE,
                                       summarize = TRUE,
                                       ...) {
  UseMethod("cal_validate_isotonic_boot")
}

#' @export
#' @rdname cal_validate_isotonic_boot
cal_validate_isotonic_boot.resample_results <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred_"),
           metrics = NULL,
           save_pred = FALSE,
           summarize = TRUE,
           ...) {

    if (!is.null(truth)) {
      rlang::warn("'truth' is automaticaly set when this type of object is used.")
    }
    truth <- tune::.get_tune_outcome_names(.data)
    # Change splits$data to be the predictions instead of the original
    # training data and save as rset
    .data <- convert_resamples(.data)
    cal_validate(
      rset = .data,
      truth = !!truth,
      estimate = {{ estimate }},
      cal_function = "isotonic_boot",
      metrics = metrics,
      summarize = summarize,
      save_pred = save_pred,
      ...
    )
  }

#' @export
#' @rdname cal_validate_isotonic_boot
cal_validate_isotonic_boot.rset <- function(.data,
                                            truth = NULL,
                                            estimate = dplyr::starts_with(".pred_"),
                                            metrics = NULL,
                                            save_pred = FALSE,
                                            summarize = TRUE,
                                            ...) {
  cal_validate(
    rset = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    cal_function = "isotonic_boot",
    metrics = metrics,
    summarize = summarize,
    save_pred = save_pred,
    ...
  )
}

#' @export
#' @rdname cal_validate_isotonic_boot
cal_validate_isotonic_boot.tune_results <- function(.data,
                                                    truth = NULL,
                                                    estimate = NULL,
                                                    metrics = NULL,
                                                    save_pred = FALSE,
                                                    summarize = TRUE,
                                                    ...) {
  abort_if_tune_result()
}

# ---------------------------------- Beta --------------------------------------
#' Measure performance with and without using Beta calibration
#' @inherit cal_validate_logistic
#' @inheritParams cal_estimate_beta
#' @param ... Options to pass to [cal_estimate_beta()], such as the
#' `shape_params` and `location_params` arguments.
#' @seealso [cal_apply()], [cal_estimate_beta()]
#' @examples
#'
#' library(dplyr)
#'
#' segment_logistic %>%
#'   rsample::vfold_cv() %>%
#'   cal_validate_beta(Class)
#'
#' @export
cal_validate_beta <- function(.data,
                              truth = NULL,
                              estimate = dplyr::starts_with(".pred_"),
                              metrics = NULL,
                              summarize = TRUE,
                              save_pred = FALSE,
                              ...) {
  UseMethod("cal_validate_beta")
}

#' @export
#' @rdname cal_validate_beta
cal_validate_beta.resample_results <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred_"),
           metrics = NULL,
           summarize = TRUE,
           save_pred = FALSE,
           ...) {

    if (!is.null(truth)) {
      rlang::warn("'truth' is automaticaly set when this type of object is used.")
    }
    truth <- tune::.get_tune_outcome_names(.data)
    # Change splits$data to be the predictions instead of the original
    # training data and save as rset
    .data <- convert_resamples(.data)
    cal_validate(
      rset = .data,
      truth = !!truth,
      estimate = {{ estimate }},
      cal_function = "beta",
      metrics = metrics,
      summarize = summarize,
      save_pred = save_pred,
      ...
    )
  }

#' @export
#' @rdname cal_validate_beta
cal_validate_beta.rset <- function(.data,
                                   truth = NULL,
                                   estimate = dplyr::starts_with(".pred_"),
                                   metrics = NULL,
                                   summarize = TRUE,
                                   save_pred = FALSE,
                                   ...) {
  cal_validate(
    rset = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    cal_function = "beta",
    metrics = metrics,
    summarize = summarize,
    save_pred = save_pred,
    ...
  )
}

#' @export
#' @rdname cal_validate_beta
cal_validate_beta.tune_results <- function(.data,
                                           truth = NULL,
                                           estimate = NULL,
                                           metrics = NULL,
                                           summarize = TRUE,
                                           save_pred = FALSE,
                                           ...) {
  abort_if_tune_result()
}

# ------------------------------- Multinomial ----------------------------------
#' Measure performance with and without using multinomial calibration
#' @inherit cal_validate_logistic
#' @inheritParams cal_estimate_multinomial
#' @seealso [cal_apply()], [cal_estimate_multinomial()]
#' @examples
#'
#' library(dplyr)
#'
#' species_probs %>%
#'   rsample::vfold_cv() %>%
#'   cal_validate_multinomial(Species)
#'
#' @export
cal_validate_multinomial <- function(.data,
                                     truth = NULL,
                                     estimate = dplyr::starts_with(".pred_"),
                                     metrics = NULL,
                                     save_pred = FALSE,
                                     summarize = TRUE,
                                     ...) {
  UseMethod("cal_validate_multinomial")
}

#' @export
#' @rdname cal_validate_multinomial
cal_validate_multinomial.resample_results <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred_"),
           metrics = NULL,
           save_pred = FALSE,
           summarize = TRUE,
           ...) {

    if (!is.null(truth)) {
      rlang::warn("'truth' is automaticaly set when this type of object is used.")
    }
    truth <- tune::.get_tune_outcome_names(.data)
    # Change splits$data to be the predictions instead of the original
    # training data and save as rset
    .data <- convert_resamples(.data)
    cal_validate(
      rset = .data,
      truth = !!truth,
      estimate = {{ estimate }},
      cal_function = "multinomial",
      metrics = metrics,
      summarize = summarize,
      save_pred = save_pred,
      ...
    )
  }

#' @export
#' @rdname cal_validate_multinomial
cal_validate_multinomial.rset <- function(.data,
                                          truth = NULL,
                                          estimate = dplyr::starts_with(".pred_"),
                                          metrics = NULL,
                                          save_pred = FALSE,
                                          summarize = TRUE,
                                          ...) {
  cal_validate(
    rset = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    cal_function = "multinomial",
    metrics = metrics,
    summarize = summarize,
    save_pred = save_pred,
    ...
  )
}

#' @export
#' @rdname cal_validate_multinomial
cal_validate_multinomial.tune_results <- function(.data,
                                                  truth = NULL,
                                                  estimate = NULL,
                                                  metrics = NULL,
                                                  save_pred = FALSE,
                                                  summarize = TRUE,
                                                  ...) {
  abort_if_tune_result()
}

# ------------------------------ Implementation --------------------------------

get_problem_type <- function(x) {
  if (is.factor(x)) {
    res <- "classification"
  } else if (is.numeric(x)) {
    res <- "regression"
  } else if (inherits(x, "Surv")) {
    res <- "censored regression"
  } else {
    rlang::abort("Cannot determine the type of calibration problem.")
  }
  res
}

check_validation_metrics <- function(metrics, model_mode) {

  if (is.null(metrics)) {
    if (model_mode == "regression") {
      metrics <- yardstick::metric_set(yardstick::rmse)
    } else if (model_mode == "classification") {
      metrics <- yardstick::metric_set(yardstick::brier_class)
    } else {
      rlang::abort("unknown mode")
    }
  } else {
    metric_info <- dplyr::as_tibble(metrics)
    if (model_mode == "regression") {
      if (any(metric_info$class != "numeric_metric")) {
        rlang::abort("Metric type should be 'numeric_metric'")
      }
    } else if (model_mode == "classification") {
      allowed <- c("prob_metric", "class_metric")
      if (any(!(metric_info$class %in% allowed))) {
        rlang::abort("Metric type should be 'prob_metric' or 'class_metric'")
      }
    } else {
      rlang::abort("unknown mode")
    }
  }
  metrics
}


cal_validate <- function(rset,
                         truth = NULL,
                         estimate = NULL,
                         cal_function = NULL,
                         metrics = NULL,
                         summarize = TRUE,
                         save_pred = FALSE,
                         ...) {
  truth <- enquo(truth)
  estimate <- enquo(estimate)

  if (is.null(cal_function)) {
    rlang::abort("No calibration function provided")
  }

  outcomes <- dplyr::select(rset$splits[[1]]$data, {{ truth }}) %>% purrr::pluck(1)
  model_mode <- get_problem_type(outcomes)
  existing_metrics <- any(names(rset) == ".metrics")

  metrics <- check_validation_metrics(metrics, model_mode)

  predictions_in <- purrr::map(rset$splits, rsample::analysis)
  predictions_out <- purrr::map(rset$splits, rsample::assessment)

  # TODO clean these up
  if (cal_function == "logistic") {
    cals <- purrr::map(
      predictions_in,
      cal_estimate_logistic,
      truth = !!truth,
      estimate = !!estimate,
      ...
    )
  }

  if (cal_function == "isotonic") {
    cals <- purrr::map(
      predictions_in,
      cal_estimate_isotonic,
      truth = !!truth,
      estimate = !!estimate,
      ...
    )
  }

  if (cal_function == "isotonic_boot") {
    cals <- purrr::map(
      predictions_in,
      cal_estimate_isotonic_boot,
      truth = !!truth,
      estimate = !!estimate,
      ...
    )
  }

  if (cal_function == "beta") {
    cals <- purrr::map(
      predictions_in,
      cal_estimate_beta,
      truth = !!truth,
      estimate = !!estimate,
      ...
    )
  }

  if (cal_function == "multinomial") {
    cals <- purrr::map(
      predictions_in,
      cal_estimate_multinomial,
      truth = !!truth,
      estimate = !!estimate,
      ...
    )
  }

  if (cal_function == "linear") {
    cals <- purrr::map(
      predictions_in,
      cal_estimate_linear,
      truth = !!truth,
      estimate = !!estimate,
      ...
    )
  }


  if (model_mode == "classification") {
    if(cals[[1]]$type == "binary") {
      estimate_cols <- cals[[1]]$levels[[1]]
    } else {
      estimate_cols <- cals[[1]]$levels %>%
        purrr::map(as_name) %>%
        purrr::reduce(c)
    }
  } else if (model_mode == "regression") {
    estimate_cols <- rlang::expr_deparse(cals[[1]]$levels$predictions)
  }

  metric_res <-
    purrr::map2_dfr(cals,
                    predictions_out,
                    compute_cal_metrics,
                    metrics = metrics,
                    truth_col = truth,
                    est_cols = estimate_cols,
                    orig = !existing_metrics,
                    pred = save_pred)
  rset <- dplyr::bind_cols(rset, metric_res)

  # TODO add metrics as attribute?
  class(rset) <- c("cal_rset", class(rset))
  rset
}

compute_cal_metrics <- function(calib, preds, metrics, truth_col, est_cols,
                                orig = TRUE, pred = FALSE) {
  metric_info <-
    dplyr::as_tibble(metrics) %>%
    dplyr::select(.metric = metric, direction)

  cal_pred <-
    cal_apply(
      .data = preds,
      object = calib,
      pred_class = !!rlang::parse_expr(".pred_class")
    )
  cal_metrics <-
    metrics(cal_pred, truth = !!truth_col, dplyr::all_of(est_cols)) %>%
    dplyr::full_join(metric_info, by = ".metric")
  res <- tibble(.metrics_cal = list(cal_metrics))
  if (pred) {
    res$.predictions_cal <- list(cal_pred)
  }

  if (orig) {
    uncal_metrics <-
      metrics(preds, truth = !!truth_col, dplyr::all_of(est_cols)) %>%
      dplyr::full_join(metric_info, by = ".metric")
    res$.metrics <- list(uncal_metrics)
    res <- dplyr::relocate(res, .metrics)
  }

  res
}



#' @importFrom pillar type_sum
#' @export
type_sum.cal_object <- function(x, ...) {
  "cal_obj"
}

# -------------------------------- Linear --------------------------------------
#' Measure performance with and without using linear regression calibration
#' @inheritParams cal_estimate_logistic
#'
#' @template metrics_reg
#'
#' @param metrics A set of metrics passed created via [yardstick::metric_set()]
#' @param summarize Indicates to pass tibble with the metrics averaged, or
#' if to return the same sampled object but with new columns containing the
#' calibration y validation list columns.
#' @param save_pred Indicates whether to include the `calibration` and
#' `validation` columns when the `summarize` argument is set to FALSE.
#' @param ... Options to pass to [cal_estimate_linear()], such as the `smooth`
#' argument.
#' @seealso [cal_apply()], [cal_estimate_linear()]
#' @examples
#' library(dplyr)
#' library(yardstick)
#' library(rsample)
#'
#' head(boosting_predictions_test)
#'
#' reg_stats <- metric_set(rmse, ccc)
#'
#' set.seed(828)
#' boosting_predictions_oob %>%
#'   # Resample with 10-fold cross-validation
#'   vfold_cv() %>%
#'   cal_validate_linear(truth = outcome, smooth = FALSE, metrics = reg_stats)
#' @export
cal_validate_linear <- function(.data,
                                truth = NULL,
                                estimate = dplyr::starts_with(".pred"),
                                metrics = NULL,
                                save_pred = FALSE,
                                summarize = TRUE,
                                ...) {
  UseMethod("cal_validate_linear")
}

#' @export
#' @rdname cal_validate_linear
cal_validate_linear.resample_results <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred"),
           metrics = NULL,
           save_pred = FALSE,
           summarize = TRUE,
           ...) {

    if (!is.null(truth)) {
      rlang::warn("'truth' is automaticaly set when this type of object is used.")
    }
    truth <- tune::.get_tune_outcome_names(.data)
    # Change splits$data to be the predictions instead of the original
    # training data and save as rset
    .data <- convert_resamples(.data)
    cal_validate(
      rset = .data,
      truth = !!truth,
      estimate = {{ estimate }},
      cal_function = "linear",
      metrics = metrics,
      summarize = summarize,
      save_pred = save_pred,
      ...
    )
  }

#' @export
#' @rdname cal_validate_linear
cal_validate_linear.rset <- function(.data,
                                     truth = NULL,
                                     estimate = dplyr::starts_with(".pred"),
                                     metrics = NULL,
                                     save_pred = FALSE,
                                     summarize = TRUE,
                                     ...) {
  cal_validate(
    rset = .data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    cal_function = "linear",
    metrics = metrics,
    summarize = summarize,
    save_pred = save_pred,
    ...
  )
}


# ------------------------------------------------------------------------------
# convert a resample_results to an rset that can be used by the validate function

convert_resamples <- function(x) {
  predictions <-
    tune::collect_predictions(x, summarize = TRUE) %>%
    dplyr::arrange(.row) %>%
    dplyr::select(-dplyr::starts_with("id"), -.row, -.config)
  for (i in seq_along(x$splits)) {
    x$splits[[i]]$data <- predictions
  }
  class(x) <- c("rset", "tbl_df", "tbl", "data.frame")
  x
}


