#' @title Vignette classifier
#'
#' @description An object of class [TEClassifierRegular] trained with the a subset of the Standford Movie Review
#'   Dataset. The purpose of classifier is for illustration in vignettes.
#'
#' @docType data
#' @format R6
#' @keywords internal
"vignette_classifier"

#' @title Test metric for an example classifier
#'
#' @description A matrix of 4 rows and 17 columns containing test metrics for an example classifier. The purpose of the
#'   data is for illustration in vignettes.
#'
#' @docType data
#' @format matrix
#' @keywords internal
"test_metric_mean"

#' @title Sustainability data for an example classifier
#'
#' @description A list of length 5 containing the used energy consumption and co2 emissions of a classifier during
#'   training. The purpose of the data is for illustration in vignettes.
#'
#' @docType data
#' @format list
#' @keywords internal
"test_classifier_sustainability"

#' @title Standford Movie Review Dataset
#'
#' @description A [data.frame] consisting of a subset of 100 negative and 200 positive movie reviews from the
#'   dataset provided by Maas et al. (2011). The [data.frame] consists of three columns. The first column 'text'
#'   stores the movie review. The second stores the labels (0 = negative, 1 = positive). The last column stores the id.
#'   The purpose of the data is for illustration in vignettes.
#'
#' @docType data
#' @format data.frame
#' @keywords internal
#' @references Maas, A. L., Daly, R. E., Pham, P. T., Huang, D., Ng, A. Y., & Potts, C. (2011). Learning Word Vectors
#'   for Sentiment Analysis. In D. Lin, Y. Matsumoto, & R. Mihalcea (Eds.), Proceedings of the 49th Annual Meeting of
#'   the Association for Computational Linguistics: Human Language Technologies (pp. 142–150). Association for
#'   Computational Linguistics. https://aclanthology.org/P11-1015
#'
"imdb_movie_reviews"
