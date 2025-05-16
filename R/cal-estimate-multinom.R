#' Uses a Multinomial calibration model to calculate new probabilities
#' @details
#' When `smooth = FALSE`, [nnet::multinom()] function is used to estimate the
#' model, otherwise [mgcv::gam()] is used.
#' @inheritParams cal_estimate_logistic
#' @seealso
#' \url{https://www.tidymodels.org/learn/models/calibration/},
#' [cal_validate_multinomial()]
#' @examplesIf !probably:::is_cran_check() & rlang::is_installed(c("modeldata", "parsnip", "randomForest"))
#' library(modeldata)
#' library(parsnip)
#' library(dplyr)
#'
#' f <-
#'   list(
#'     ~ -0.5 + 0.6 * abs(A),
#'     ~ ifelse(A > 0 & B > 0, 1.0 + 0.2 * A / B, -2),
#'     ~ -0.6 * A + 0.50 * B - A * B
#'   )
#'
#' set.seed(1)
#' tr_dat <- sim_multinomial(500, eqn_1 = f[[1]], eqn_2 = f[[2]], eqn_3 = f[[3]])
#' cal_dat <- sim_multinomial(500, eqn_1 = f[[1]], eqn_2 = f[[2]], eqn_3 = f[[3]])
#' te_dat <- sim_multinomial(500, eqn_1 = f[[1]], eqn_2 = f[[2]], eqn_3 = f[[3]])
#'
#' set.seed(2)
#' rf_fit <-
#'   rand_forest() |>
#'   set_mode("classification") |>
#'   set_engine("randomForest") |>
#'   fit(class ~ ., data = tr_dat)
#'
#' cal_pred <-
#'   predict(rf_fit, cal_dat, type = "prob") |>
#'   bind_cols(cal_dat)
#' te_pred <-
#'   predict(rf_fit, te_dat, type = "prob") |>
#'   bind_cols(te_dat)
#'
#' cal_plot_windowed(cal_pred, truth = class, window_size = 0.1, step_size = 0.03)
#'
#' smoothed_mn <- cal_estimate_multinomial(cal_pred, truth = class)
#'
#' new_test_pred <- cal_apply(te_pred, smoothed_mn)
#'
#' cal_plot_windowed(new_test_pred, truth = class, window_size = 0.1, step_size = 0.03)
#'
#' @export
cal_estimate_multinomial <- function(.data,
                                     truth = NULL,
                                     estimate = dplyr::starts_with(".pred_"),
                                     smooth = TRUE,
                                     parameters = NULL,
                                     ...) {
  UseMethod("cal_estimate_multinomial")
}

#' @export
#' @rdname cal_estimate_multinomial
cal_estimate_multinomial.data.frame <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred_"),
           smooth = TRUE,
           parameters = NULL,
           ...,
           .by = NULL) {
    stop_null_parameters(parameters)

    info <- get_prediction_data(
      .data,
      truth = {{ truth }},
      estimate = {{ estimate }},
      .by = {{ .by }}
    )

    model <- mtnml_fit_over_groups(info, smooth, ...)

    if (smooth) {
      method <- "Generalized additive model calibration"
      additional_class <- c("cal_estimate_multinomial_spline",
                            "cal_estimate_multinomial")
    } else {
      method <- "Multinomial regression calibration"
      additional_class <- "cal_estimate_multinomial"
    }

    as_cal_object(
      estimate = model,
      levels = info$map,
      truth = info$truth,
      method = method,
      rows = nrow(info$predictions),
      additional_classes = additional_class,
      source_class = cal_class_name(.data),
      type = "multiclass"
    )
  }

#' @export
#' @rdname cal_estimate_multinomial
cal_estimate_multinomial.tune_results <-
  function(.data,
           truth = NULL,
           estimate = dplyr::starts_with(".pred_"),
           smooth = TRUE,
           parameters = NULL,
           ...) {

    info <- get_tune_data(.data, parameters)

    model <- mtnml_fit_over_groups(info, smooth, ...)

    if (!smooth) {
      model_label <- "Multinomial regression calibration"
    } else {
      model_label <- "Generalized additive model calibration"
    }

    as_cal_object(
      estimate = model,
      levels = info$map,
      truth = info$truth,
      method = model_label,
      rows = nrow(info$predictions),
      additional_classes = "cal_estimate_multinomial",
      source_class = cal_class_name(.data),
      type = "multiclass"
    )
  }

#' @export
#' @rdname cal_estimate_multinomial
cal_estimate_multinomial.grouped_df <- function(.data,
                                                truth = NULL,
                                                estimate = NULL,
                                                smooth = TRUE,
                                                parameters = NULL,
                                                ...) {
  abort_if_grouped_df()
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_multinomial_spline <- function(x, ...) {
  check_req_pkgs(x, unsmooth = "nnet")
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_multinomial <- function(x, ...) {
  # for legacy objects that all have this class.
  check_req_pkgs(x, unsmooth = "nnet")
}

#' @rdname required_pkgs.cal_object
#' @keywords internal
#' @export
required_pkgs.cal_estimate_multinomial <- function(x, ...) {
  check_req_pkgs(x, unsmooth = "nnet")
}

clean_env <- function(x) {
  attr(x, ".Environment") <- rlang::base_env()
  x
}

fit_multinomial_model <- function(.data, smooth, estimate, outcome, ...) {
  smooth <- turn_off_smooth_if_too_few_unique(.data, estimate, smooth)

  if (smooth) {
    # multinomial gams in mgcv needs zero-based integers as the outcome
    .data[[outcome]] <- as.numeric(.data[[outcome]]) - 1
    max_int <- max(.data[[outcome]], na.rm = TRUE)

    f <- multinomial_f_from_str(outcome, estimate)
    # TODO check for failures
    model <- mgcv::gam(f, data = .data, family = mgcv::multinom(max_int), ...)
    model$terms <- clean_env(model$terms)
  } else {
    f <- f_from_str(outcome, estimate[-length(estimate)])
    # TODO check for failures
    prevent_output <- utils::capture.output(
      model <- nnet::multinom(formula = f, data = .data, ...)
    )
    model$terms <- clean_env(model$terms)
  }

  model
}


mtnml_fit_over_groups <- function(info, smooth = TRUE, ...) {
  if (length(info$levels) == 2) {
    cli::cli_abort("This function is meant to be used with multi-class outcomes only.")
  }

  grp_df <- make_group_df(info$predictions, group = info$group)
  nst_df <- vctrs::vec_split(x = info$predictions, by = grp_df)
  fltrs <- make_cal_filters(nst_df$key)

  fits <-
    lapply(
      nst_df$val,
      fit_multinomial_model,
      smooth = smooth,
      estimate = info$estimate,
      info$truth,
      ...
    )

  purrr::map2(fits, fltrs, ~ list(filter = .y, estimate = .x))
}
