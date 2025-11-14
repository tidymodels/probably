# Package index

## Thresholds

- [`threshold_perf()`](https://probably.tidymodels.org/dev/reference/threshold_perf.md)
  : Generate performance metrics across probability thresholds

## Create class predictions

- [`append_class_pred()`](https://probably.tidymodels.org/dev/reference/append_class_pred.md)
  :

  Add a `class_pred` column

- [`make_class_pred()`](https://probably.tidymodels.org/dev/reference/make_class_pred.md)
  [`make_two_class_pred()`](https://probably.tidymodels.org/dev/reference/make_class_pred.md)
  :

  Create a `class_pred` vector from class probabilities

## Class predictions

- [`class_pred()`](https://probably.tidymodels.org/dev/reference/class_pred.md)
  : Create a class prediction object

- [`as_class_pred()`](https://probably.tidymodels.org/dev/reference/as_class_pred.md)
  :

  Coerce to a `class_pred` object

- [`is_class_pred()`](https://probably.tidymodels.org/dev/reference/is_class_pred.md)
  :

  Test if an object inherits from `class_pred`

- [`reportable_rate()`](https://probably.tidymodels.org/dev/reference/reportable_rate.md)
  : Calculate the reportable rate

- [`is_equivocal()`](https://probably.tidymodels.org/dev/reference/locate-equivocal.md)
  [`which_equivocal()`](https://probably.tidymodels.org/dev/reference/locate-equivocal.md)
  [`any_equivocal()`](https://probably.tidymodels.org/dev/reference/locate-equivocal.md)
  : Locate equivocal values

- [`levels(`*`<class_pred>`*`)`](https://probably.tidymodels.org/dev/reference/levels.class_pred.md)
  :

  Extract `class_pred` levels

## Regression predictions

- [`int_conformal_cv()`](https://probably.tidymodels.org/dev/reference/int_conformal_cv.md)
  : Prediction intervals via conformal inference CV+
- [`int_conformal_full()`](https://probably.tidymodels.org/dev/reference/int_conformal_full.md)
  : Prediction intervals via conformal inference
- [`int_conformal_quantile()`](https://probably.tidymodels.org/dev/reference/int_conformal_quantile.md)
  : Prediction intervals via conformal inference and quantile regression
- [`int_conformal_split()`](https://probably.tidymodels.org/dev/reference/int_conformal_split.md)
  : Prediction intervals via split conformal inference
- [`control_conformal_full()`](https://probably.tidymodels.org/dev/reference/control_conformal_full.md)
  : Controlling the numeric details for conformal inference
- [`predict(`*`<int_conformal_cv>`*`)`](https://probably.tidymodels.org/dev/reference/predict.int_conformal_full.md)
  [`predict(`*`<int_conformal_full>`*`)`](https://probably.tidymodels.org/dev/reference/predict.int_conformal_full.md)
  [`predict(`*`<int_conformal_quantile>`*`)`](https://probably.tidymodels.org/dev/reference/predict.int_conformal_full.md)
  [`predict(`*`<int_conformal_split>`*`)`](https://probably.tidymodels.org/dev/reference/predict.int_conformal_full.md)
  : Prediction intervals from conformal methods
- [`bound_prediction()`](https://probably.tidymodels.org/dev/reference/bound_prediction.md)
  : Truncate a numeric prediction column

## Data

- [`segment_naive_bayes`](https://probably.tidymodels.org/dev/reference/segment_naive_bayes.md)
  [`segment_logistic`](https://probably.tidymodels.org/dev/reference/segment_naive_bayes.md)
  : Image segmentation predictions
- [`species_probs`](https://probably.tidymodels.org/dev/reference/species_probs.md)
  : Predictions on animal species
- [`boosting_predictions`](https://probably.tidymodels.org/dev/reference/boosting_predictions.md)
  [`boosting_predictions_oob`](https://probably.tidymodels.org/dev/reference/boosting_predictions.md)
  [`boosting_predictions_test`](https://probably.tidymodels.org/dev/reference/boosting_predictions.md)
  : Boosted regression trees predictions

## Calibration

- [`cal_estimate_beta()`](https://probably.tidymodels.org/dev/reference/cal_estimate_beta.md)
  : Uses a Beta calibration model to calculate new probabilities
- [`cal_estimate_isotonic()`](https://probably.tidymodels.org/dev/reference/cal_estimate_isotonic.md)
  : Uses an Isotonic regression model to calibrate model predictions.
- [`cal_estimate_isotonic_boot()`](https://probably.tidymodels.org/dev/reference/cal_estimate_isotonic_boot.md)
  : Uses a bootstrapped Isotonic regression model to calibrate
  probabilities
- [`cal_estimate_linear()`](https://probably.tidymodels.org/dev/reference/cal_estimate_linear.md)
  : Uses a linear regression model to calibrate numeric predictions
- [`cal_estimate_logistic()`](https://probably.tidymodels.org/dev/reference/cal_estimate_logistic.md)
  : Uses a logistic regression model to calibrate probabilities
- [`cal_estimate_multinomial()`](https://probably.tidymodels.org/dev/reference/cal_estimate_multinomial.md)
  : Uses a Multinomial calibration model to calculate new probabilities
- [`cal_estimate_none()`](https://probably.tidymodels.org/dev/reference/cal_estimate_none.md)
  : Do not calibrate model predictions.
- [`cal_apply()`](https://probably.tidymodels.org/dev/reference/cal_apply.md)
  : Applies a calibration to a set of existing predictions

## Calibration Validation

- [`cal_validate_beta()`](https://probably.tidymodels.org/dev/reference/cal_validate_beta.md)
  : Measure performance with and without using Beta calibration
- [`cal_validate_isotonic()`](https://probably.tidymodels.org/dev/reference/cal_validate_isotonic.md)
  : Measure performance with and without using isotonic regression
  calibration
- [`cal_validate_isotonic_boot()`](https://probably.tidymodels.org/dev/reference/cal_validate_isotonic_boot.md)
  : Measure performance with and without using bagged isotonic
  regression calibration
- [`cal_validate_linear()`](https://probably.tidymodels.org/dev/reference/cal_validate_linear.md)
  : Measure performance with and without using linear regression
  calibration
- [`cal_validate_logistic()`](https://probably.tidymodels.org/dev/reference/cal_validate_logistic.md)
  : Measure performance with and without using logistic calibration
- [`cal_validate_multinomial()`](https://probably.tidymodels.org/dev/reference/cal_validate_multinomial.md)
  : Measure performance with and without using multinomial calibration
- [`cal_validate_none()`](https://probably.tidymodels.org/dev/reference/cal_validate_none.md)
  : Measure performance without using calibration
- [`collect_metrics(`*`<cal_rset>`*`)`](https://probably.tidymodels.org/dev/reference/collect_metrics.cal_rset.md)
  : Obtain and format metrics produced by calibration validation
- [`collect_predictions(`*`<cal_rset>`*`)`](https://probably.tidymodels.org/dev/reference/collect_predictions.cal_rset.md)
  : Obtain and format predictions produced by calibration validation

## Calibration Plots

- [`cal_plot_breaks()`](https://probably.tidymodels.org/dev/reference/cal_plot_breaks.md)
  : Probability calibration plots via binning
- [`cal_plot_logistic()`](https://probably.tidymodels.org/dev/reference/cal_plot_logistic.md)
  : Probability calibration plots via logistic regression
- [`cal_plot_regression()`](https://probably.tidymodels.org/dev/reference/cal_plot_regression.md)
  : Regression calibration plots
- [`cal_plot_windowed()`](https://probably.tidymodels.org/dev/reference/cal_plot_windowed.md)
  : Probability calibration plots via moving windows
