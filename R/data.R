# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Standford Movie Review Dataset
#'
#' @description A [data.frame] consisting of a subset of 100 negative and 200 positive movie reviews from the
#'   dataset provided by Maas et al. (2011). The [data.frame] consists of three columns. The first column 'text'
#'   stores the movie review. The second stores the labels (0 = negative, 1 = positive). The last column stores the id.
#'   The purpose of the data is for illustration in vignettes and for tests.
#'
#' @docType data
#' @format data.frame
#' @family Data Sets
#' @references Maas, A. L., Daly, R. E., Pham, P. T., Huang, D., Ng, A. Y., & Potts, C. (2011). Learning Word Vectors
#'   for Sentiment Analysis. In D. Lin, Y. Matsumoto, & R. Mihalcea (Eds.), Proceedings of the 49th Annual Meeting of
#'   the Association for Computational Linguistics: Human Language Technologies (pp. 142–150). Association for
#'   Computational Linguistics. <https://aclanthology.org/P11-1015>
#'
"imdb_movie_reviews"
