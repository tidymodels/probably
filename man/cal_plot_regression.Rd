% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal-plot-regression.R
\name{cal_plot_regression}
\alias{cal_plot_regression}
\alias{cal_plot_regression.data.frame}
\alias{cal_plot_regression.tune_results}
\alias{cal_plot_regression.grouped_df}
\title{Regression calibration plots}
\usage{
cal_plot_regression(.data, truth = NULL, estimate = NULL, smooth = TRUE, ...)

\method{cal_plot_regression}{data.frame}(
  .data,
  truth = NULL,
  estimate = NULL,
  smooth = TRUE,
  ...,
  .by = NULL
)

\method{cal_plot_regression}{tune_results}(.data, truth = NULL, estimate = NULL, smooth = TRUE, ...)

\method{cal_plot_regression}{grouped_df}(.data, truth = NULL, estimate = NULL, smooth = TRUE, ...)
}
\arguments{
\item{.data}{An ungrouped data frame object containing a prediction
column.}

\item{truth}{The column identifier for the true results
(numeric). This should be an unquoted column name.}

\item{estimate}{The column identifier for the predictions.
This should be an unquoted column name}

\item{smooth}{A logical: should a smoother curve be added.}

\item{...}{Additional arguments passed to \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}.}

\item{.by}{The column identifier for the grouping variable. This should be
a single unquoted column name that selects a qualitative variable for
grouping. Default to \code{NULL}. When \code{.by = NULL} no grouping will take place.}
}
\value{
A ggplot object.
}
\description{
A scatter plot of the observed and predicted values is computed where the
axes are the same. When \code{smooth = TRUE}, a generalized additive model fit
is shown. If the predictions are well calibrated, the fitted curve should align with
the diagonal line.
}
\examples{
cal_plot_regression(boosting_predictions_oob, outcome, .pred)

cal_plot_regression(boosting_predictions_oob, outcome, .pred,
  alpha = 1 / 6, cex = 3, smooth = FALSE
)

cal_plot_regression(boosting_predictions_oob, outcome, .pred,
  .by = id,
  alpha = 1 / 6, cex = 3, smooth = FALSE
)
}
