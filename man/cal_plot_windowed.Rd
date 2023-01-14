% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cal-plot.R
\name{cal_plot_windowed}
\alias{cal_plot_windowed}
\alias{cal_plot_windowed.data.frame}
\alias{cal_plot_windowed.tune_results}
\title{Probability calibration plots via moving windows}
\usage{
cal_plot_windowed(
  .data,
  truth = NULL,
  estimate = NULL,
  group = NULL,
  window_size = 0.1,
  step_size = window_size/2,
  conf_level = 0.9,
  include_ribbon = TRUE,
  include_rug = TRUE,
  include_points = TRUE,
  event_level = c("first", "second"),
  ...
)

\method{cal_plot_windowed}{data.frame}(
  .data,
  truth = NULL,
  estimate = NULL,
  group = NULL,
  window_size = 0.1,
  step_size = window_size/2,
  conf_level = 0.9,
  include_ribbon = TRUE,
  include_rug = TRUE,
  include_points = TRUE,
  event_level = c("first", "second"),
  ...
)

\method{cal_plot_windowed}{tune_results}(
  .data,
  truth = NULL,
  estimate = NULL,
  group = NULL,
  window_size = 0.1,
  step_size = window_size/2,
  conf_level = 0.9,
  include_ribbon = TRUE,
  include_rug = TRUE,
  include_points = TRUE,
  event_level = c("first", "second"),
  ...
)
}
\arguments{
\item{.data}{A data.frame object containing predictions and probability columns.}

\item{truth}{The column identifier for the true class results
(that is a factor). This should be an unquoted column name.}

\item{estimate}{The column identifier for the prediction probabilities.
This should be an unquoted column name}

\item{group}{The column identifier to group the results.}

\item{window_size}{The size of segments. Used for the windowed probability
calculations. It defaults to 10\% of segments.}

\item{step_size}{The gap between segments. Used for the windowed probability
calculations. It defaults to half the size of \code{window_size}}

\item{conf_level}{Confidence level to use in the visualization. It defaults
to 0.9.}

\item{include_ribbon}{Flag that indicates if the ribbon layer is to be
included. It defaults to \code{TRUE}.}

\item{include_rug}{Flag that indicates if the Rug layer is to be included.
It defaults to \code{TRUE}. In the plot, the top side shows the frequency the
event occurring, and the bottom the frequency of the event not occurring.}

\item{include_points}{Flag that indicates if the point layer is to be included.}

\item{event_level}{single string. Either "first" or "second" to specify which
level of truth to consider as the "event".}

\item{...}{Additional arguments passed to the \code{tune_results} object.}
}
\value{
A ggplot object.
}
\description{
A plot is created to assess whether the observed rate of the event is about
the sample as the predicted probability of the event from some model. This
is similar to \code{\link[=cal_plot_breaks]{cal_plot_breaks()}}, except that the bins are overlapping.

A sequence of bins are created from zero to one. For each bin, the data whose
predicted probability falls within the range of the bin is used to calculate
the observed event rate (along with confidence intervals for the event rate).

If the predictions are well calibrated, the fitted curve should align with
the diagonal line.
}
\examples{

library(ggplot2)
library(dplyr)

cal_plot_windowed(
  segment_logistic,
  Class,
  .pred_good
)

# More breaks
cal_plot_windowed(
  segment_logistic,
  Class,
  .pred_good,
  window_size = 0.05
)
}
\seealso{
\code{\link[=cal_plot_breaks]{cal_plot_breaks()}}, \code{\link[=cal_plot_logistic]{cal_plot_logistic()}}
}