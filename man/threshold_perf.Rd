% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threshold_perf.R
\name{threshold_perf}
\alias{threshold_perf}
\alias{threshold_perf.data.frame}
\title{Generate performance metrics across probability thresholds}
\usage{
threshold_perf(.data, ...)

\method{threshold_perf}{data.frame}(
  .data,
  truth,
  estimate,
  thresholds = NULL,
  metrics = NULL,
  na_rm = TRUE,
  event_level = "first",
  ...
)
}
\arguments{
\item{.data}{A tibble, potentially grouped.}

\item{...}{Currently unused.}

\item{truth}{The column identifier for the true two-class results
(that is a factor). This should be an unquoted column name.}

\item{estimate}{The column identifier for the predicted class probabilities
(that is a numeric). This should be an unquoted column name.}

\item{thresholds}{A numeric vector of values for the probability
threshold. If unspecified, a series
of values between 0.5 and 1.0 are used. \strong{Note}: if this
argument is used, it must be named.}

\item{metrics}{Either \code{NULL} or a \code{\link[yardstick:metric_set]{yardstick::metric_set()}} with a list of
performance metrics to calculate. The metrics should all be oriented towards
hard class predictions (e.g. \code{\link[yardstick:sens]{yardstick::sensitivity()}},
\code{\link[yardstick:accuracy]{yardstick::accuracy()}}, \code{\link[yardstick:recall]{yardstick::recall()}}, etc.) and not
class probabilities. A set of default metrics is used when \code{NULL} (see
Details below).}

\item{na_rm}{A single logical: should missing data be removed?}

\item{event_level}{A single string. Either \code{"first"} or \code{"second"} to specify
which level of \code{truth} to consider as the "event".}
}
\value{
A tibble with columns: \code{.threshold}, \code{.estimator}, \code{.metric},
\code{.estimate} and any existing groups.
}
\description{
\code{threshold_perf()} can take a set of class probability predictions
and determine performance characteristics across different values
of the probability threshold and any existing groups.
}
\details{
Note that that the global option \code{yardstick.event_first} will be
used to determine which level is the event of interest. For more details,
see the Relevant level section of \code{\link[yardstick:sens]{yardstick::sens()}}.

The default calculated metrics are:
\itemize{
\item \code{\link[yardstick:j_index]{yardstick::j_index()}}
\item \code{\link[yardstick:sens]{yardstick::sens()}}
\item \code{\link[yardstick:spec]{yardstick::spec()}}
\item \code{distance = (1 - sens) ^ 2 + (1 - spec) ^ 2}
}

If a custom metric is passed that does not compute sensitivity and
specificity, the distance metric is not computed.
}
\examples{
library(dplyr)
data("segment_logistic")

# Set the threshold to 0.6
# > 0.6 = good
# < 0.6 = poor
threshold_perf(segment_logistic, Class, .pred_good, thresholds = 0.6)

# Set the threshold to multiple values
thresholds <- seq(0.5, 0.9, by = 0.1)

segment_logistic |>
  threshold_perf(Class, .pred_good, thresholds)

# ---------------------------------------------------------------------------

# It works with grouped data frames as well
# Let's mock some resampled data
resamples <- 5

mock_resamples <- resamples |>
  replicate(
    expr = sample_n(segment_logistic, 100, replace = TRUE),
    simplify = FALSE
  ) |>
  bind_rows(.id = "resample")

resampled_threshold_perf <- mock_resamples |>
  group_by(resample) |>
  threshold_perf(Class, .pred_good, thresholds)

resampled_threshold_perf

# Average over the resamples
resampled_threshold_perf |>
  group_by(.metric, .threshold) |>
  summarise(.estimate = mean(.estimate))

}
