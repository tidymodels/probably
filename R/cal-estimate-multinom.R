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
#'   rand_forest() %>%
#'   set_mode("classification") %>%
#'   set_engine("randomForest") %>%
#'   fit(class ~ ., data = tr_dat)
#'
#' cal_pred <-
#'   predict(rf_fit, cal_dat, type = "prob") %>%
#'   bind_cols(cal_dat)
#' te_pred <-
#'   predict(rf_fit, te_dat, type = "prob") %>%
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

    group <- get_group_argument({{ .by }}, .data)
    .data <- dplyr::group_by(.data, dplyr::across({{ group }}))

    truth <- enquo(truth)
    cal_multinom_impl(
      .data = .data,
      truth = !!truth,
      estimate = {{ estimate }},
      source_class = cal_class_name(.data),
      smooth = smooth,
      ...
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
    tune_args <- tune_results_args(
      .data = .data,
      truth = {{ truth }},
      estimate = {{ estimate }},
      event_level = "first",
      parameters = parameters,
      ...
    )

    tune_args$predictions %>%
      dplyr::group_by(!!tune_args$group) %>%
      cal_multinom_impl(
        truth = !!tune_args$truth,
        estimate = !!tune_args$estimate,
        source_class = cal_class_name(.data),
        smooth = smooth,
        ...
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
required_pkgs.cal_estimate_multinomial <- function(x, ...) {
  c("nnet", "probably")
}

cal_multinom_impl <- function(.data, truth, estimate, source_class, smooth, ...) {
  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }})

  if (length(levels) == 2) {
    rlang::abort(
      "This function is meant to be used with a multi-class outcomes only"
    )
  }

  model <- cal_multinom_impl_grp(
    .data = .data,
    truth = !!truth,
    levels = levels,
    smooth = smooth,
    ...
  )

  as_cal_object(
    estimate = model,
    levels = levels,
    truth = !!truth,
    method = if (!smooth) "Multinomial regression" else "Generalized additive model",
    rows = nrow(.data),
    additional_classes = "cal_estimate_multinomial",
    source_class = source_class,
    type = "multiclass"
  )
}


cal_multinom_impl_grp <- function(.data, truth, levels, smooth, ...) {
  truth <- enquo(truth)
  .data %>%
    split_dplyr_groups() %>%
    lapply(
      function(x) {
        estimate <- cal_multinom_impl_single(
          .data = x$data,
          truth = !!truth,
          levels = levels,
          smooth = smooth,
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
                                     smooth = TRUE,
                                     ...) {
  truth <- enquo(truth)
  num_lvls <- length(levels)
  levels <- levels[1:(length(levels) - 1)]

  if (smooth) {
    # multinomial gams in mgcv needs zero-based integers as the outcome

    class_col <- deparse(ensym(truth))
    .data[[class_col]] <- as.numeric(.data[[class_col]]) - 1
    max_int <- max(.data[[class_col]], na.rm = TRUE)

    # It also needs a list of formulas, one for each level, and the first one
    # requires a LHS

    smooths <- purrr::map(levels, ~ call2(.fn = "s", expr(!!.x)))
    rhs_f <- purrr::reduce(smooths, function(x, y) expr(!!x + !!y))
    rhs_only <- new_formula(lhs = NULL, rhs = rhs_f)
    both_sides <- new_formula(lhs = ensym(truth), rhs = rhs_f)
    all_f <- purrr::map(seq_along(levels), ~rhs_only)
    all_f[[1]] <- both_sides

    model <- mgcv::gam(all_f, data = .data, family = mgcv::multinom(max_int))

    # Nuke environments saved in formulas
    # # TODO This next line causes a failure for unknown reasons. Look into it more
    # model$formula <- purrr::map(model$formula, clean_env)
    model$terms <- clean_env(model$terms)
  } else {
    levels_formula <- purrr::reduce(
      levels,
      function(x, y) expr(!!x + !!y)
    )

    f_model <- expr(!!ensym(truth) ~ !!levels_formula)

    prevent_output <- utils::capture.output(
      model <- nnet::multinom(formula = f_model, data = .data, ...)
    )
    model$terms <- clean_env(model$terms)
  }


  model
}

clean_env <- function(x) {
  attr(x, ".Environment") <- rlang::base_env()
  x
}
