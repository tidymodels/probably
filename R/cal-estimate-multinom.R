cal_multinom_impl <- function(.data,
                          truth = NULL,
                          estimate = dplyr::starts_with(".pred_"),
                          ...) {
  truth <- enquo(truth)

  levels <- truth_estimate_map(.data, !!truth, {{ estimate }})

  levels_formula <- purrr::reduce(levels, function(x, y) expr(!!x + !!y))

  f_model <- expr(!!ensym(truth) ~ !!levels_formula)

  model <- nnet::multinom(formula = f_model, data = .data)

  res <- list(
    estimate = model,
    levels = levels
  )

  class(res) <- c("cal_multinom", "cal_obj")

  res
}
