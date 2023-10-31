#' Predictions on animal species
#'
#' @details These data are holdout predictions from resampling for the animal
#'  scat data of Reid (2015) based on a C5.0 classification model.
#'
#' @name species_probs
#' @aliases species_probs
#' @docType data
#' @return \item{species_probs}{a tibble}
#'
#' @source Reid, R. E. B. (2015). A morphometric modeling approach to
#' distinguishing among bobcat, coyote and gray fox scats. \emph{Wildlife
#' Biology}, 21(5), 254-262
#'
#' @keywords datasets
#' @examples
#' data(species_probs)
#' str(species_probs)
NULL


#' Image segmentation predictions
#'
#' @details These objects contain test set predictions for the cell segmentation
#'  data from Hill, LaPan, Li and Haney (2007). Each data frame are the results
#'  from different models (naive Bayes and logistic regression).
#'
#' @name segment_naive_bayes
#' @aliases segment_naive_bayes segment_logistic
#' @docType data
#' @return \item{segment_naive_bayes,segment_logistic}{a tibble}
#'
#' @source Hill, LaPan, Li and Haney (2007). Impact of image segmentation on
#' high-content screening data quality for SK-BR-3 cells, \emph{BMC
#' Bioinformatics}, Vol. 8, pg. 340,
#' \url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-8-340}.
#'
#' @keywords datasets
#' @examples
#' data(segment_naive_bayes)
#' data(segment_logistic)
NULL


#' Boosted regression trees predictions
#'
#' @details These data have a set of holdout predictions from 10-fold
#' cross-validation and a separate collection of test set predictions from the
#' same boosted tree model. The data were generated using the `sim_regression`
#' function in the \pkg{modeldata} package.
#'
#' @name boosting_predictions
#' @aliases boosting_predictions_oob boosting_predictions_test
#' @docType data
#' @return \item{boosting_predictions_oob,boosting_predictions_test}{tibbles}
#'
#' @keywords datasets
#' @examples
#' data(boosting_predictions_oob)
#' str(boosting_predictions_oob)
#' str(boosting_predictions_test)
NULL
