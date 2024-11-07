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

#' Array to matrix
#'
#' Function transforming an array to a matrix.
#'
#' @param text_embedding \code{array} containing the text embedding. The array
#' should be created via an object of class \link{TextEmbeddingModel}.
#' @return Returns a matrix which contains the cases in the rows and the columns
#' represent the features of all sequences. The sequences are concatenated.
#'
#' @family Auxiliary Functions
#'
#' @export
array_to_matrix <- function(text_embedding) {
  # Deprecation Warning
  .Deprecated("array_to_matrix")

  features <- dim(text_embedding)[3]
  times <- dim(text_embedding)[2]
  cases <- dim(text_embedding)[1]

  embedding_matrix <- matrix(
    data = 0,
    nrow = nrow(text_embedding),
    ncol = times * features
  )
  for (i in 1:cases) {
    for (j in 1:times) {
      tmp_interval <- (1:features) + (j - 1) * features
      embedding_matrix[i, tmp_interval] <- text_embedding[i, j, ]
    }
  }
  rownames(embedding_matrix) <- rownames(text_embedding)
  colnames(embedding_matrix) <- colnames(embedding_matrix,
    do.NULL = FALSE,
    prefix = "feat_"
  )
  return(embedding_matrix)
}



#' Check of compatible text embedding models
#'
#' This function checks if different objects are based on the same text
#' embedding model. This is necessary to ensure that classifiers are used
#' only with data generated through compatible embedding models.
#'
#' @param object_list \code{list} of object of class \link{EmbeddedText} or
#' \link{TextEmbeddingClassifierNeuralNet}.
#' @param same_class \code{bool} \code{TRUE} if all object must be from the same class.
#' @return Returns \code{TRUE} if all objects refer to the same text embedding model.
#' \code{FALSE} in all other cases.
#' @family Auxiliary Functions
#' @keywords internal
check_embedding_models <- function(object_list,
                                   same_class = FALSE) {
  # Deprecation Warning
  .Deprecated("check_embedding_models")

  # Check if the class of the object is TextEmbeddingModel, EmbeddedText or
  # TextEmbeddingClassifierNeuralNet
  for (i in 1:length(object_list)) {
    if (!(methods::is(object_list[[i]], "TextEmbeddingModel") ||
      methods::is(object_list[[i]], "EmbeddedText") ||
      methods::is(object_list[[i]], "TextEmbeddingClassifierNeuralNet"))) {
      stop("List contains objects of the wrong class. Objects must be of class
      TextEmbeddingModel, EmbeddedText or TextEmbeddingClassifierNeuralNet")
    }
  }

  # Check if all object are from the same class---------------------------------
  if (same_class == TRUE) {
    tmp_class <- NULL
    for (i in 1:length(object_list)) {
      tmp_class[i] <- list(class(object_list[[i]]))
      if (i > 1) {
        if (tmp_class[[i - 1]][[1]] != tmp_class[[i]][[1]]) {
          return(FALSE)
        }
      }
    }
  }

  # Check if all object have the same model configuration---------------------------------
  # field to check
  # to_check<-c("model_name","model_date","model_method","model_version","model_language",
  #            "param_seq_length","param_chunks","param_overlap","param_aggregation")
  # to_check<-c("model_name","model_method","model_version","model_language",
  #            "param_seq_length","param_chunks","param_overlap","param_aggregation")
  to_check <- c("model_name")

  tmp_model_config <- NULL
  for (i in 1:length(object_list)) {
    if (methods::is(object_list[[i]], "TextEmbeddingModel")) {
      if (object_list[[i]]$get_model_info()$model_method == "bert" ||
        object_list[[i]]$get_model_info()$model_method == "roberta" ||
        object_list[[i]]$get_model_info()$model_method == "longformer") {
        tmp_model_config[i] <- list(object_list[[i]]$get_model_info())
        tmp_model_config[[i]]["model_method"] <- list(object_list[[i]]$get_basic_components()$method)
        tmp_model_config[[i]]["param_seq_length"] <- object_list[[i]]$get_basic_components()$max_length
        tmp_model_config[[i]]["param_chunks"] <- object_list[[i]]$get_transformer_components()$chunks
        tmp_model_config[[i]]["param_overlap"] <- object_list[[i]]$get_transformer_components()$overlap
        tmp_model_config[[i]]["param_aggregation"] <- object_list[[i]]$get_transformer_components()$aggregation
      } else {
        tmp_model_config[i] <- list(object_list[[i]]$get_model_info())
        tmp_model_config[[i]]["model_method"] <- list(object_list[[i]]$get_basic_components()$method)
        tmp_model_config[[i]]["param_seq_length"] <- object_list[[i]]$get_basic_components()$max_length
        tmp_model_config[[i]]["param_chunks"] <- object_list[[i]]$get_bow_components()$chunks
        tmp_model_config[[i]]["param_overlap"] <- object_list[[i]]$get_bow_components()$overlap
        tmp_model_config[[i]]["param_aggregation"] <- object_list[[i]]$get_bow_components()$aggregation
      }
    } else if (methods::is(object_list[[i]], "EmbeddedText")) {
      tmp_model_config[i] <- list(object_list[[i]]$get_model_info())
    } else if (methods::is(object_list[[i]], "TextEmbeddingClassifierNeuralNet")) {
      tmp_model_config[i] <- list(object_list[[i]]$trained_learner$get_text_embedding_model()$model)
    }
  }

  for (i in 1:length(object_list)) {
    if (i > 1) {
      tmp_i_1 <- tmp_model_config[[i - 1]]
      tmp_i <- tmp_model_config[[i]]
      for (check in to_check) {
        #--------------------------------------------------------------------
        if (is.null(tmp_i_1[[check]]) == TRUE) {
          tmp_i_1[[check]] <- "missing"
        }
        if (is.null(tmp_i[[check]]) == TRUE) {
          tmp_i[[check]] <- "missing"
        }

        if (identical(tmp_i_1[[check]], integer(0))) {
          tmp_i_1[[check]] <- "missing"
        }
        if (identical(tmp_i[[check]], integer(0))) {
          tmp_i[[check]] <- "missing"
        }

        if (is.na(tmp_i_1[[check]]) == TRUE) {
          tmp_i_1[[check]] <- "missing"
        }
        if (is.na(tmp_i[[check]]) == TRUE) {
          tmp_i[[check]] <- "missing"
        }


        #----------------------------------------------------------------------
        if (as.character(tmp_i_1[[check]]) != as.character(tmp_i[[check]])) {
          return(FALSE)
        } else {

        }
        #----------------------------------------------------------------------
      }
    }
  }
  return(TRUE)
}









#------------------------------------------------------------------------------
#' Split data into labeled and unlabeled data
#'
#' This functions splits data into labeled and unlabeled data.
#'
#' @param embedding Object of class \link{EmbeddedText}.
#' @param target Named \code{factor} containing all cases with labels and missing
#' labels.
#' @return Returns a \code{list} with the following components
#' \itemize{
#' \item{\code{embeddings_labeled: }Object of class \link{EmbeddedText} containing
#' only the cases which have labels.}
#'
#' \item{\code{embeddings_unlabeled: }Object of class \link{EmbeddedText} containing
#' only the cases which have no labels.}
#'
#' \item{\code{targets_labeled: }Named \code{factor} containing the labels of
#' relevant cases.}
#' }
#' @family Auxiliary Functions
#' @keywords internal
split_labeled_unlabeled <- function(embedding,
                                    target) {
  # Deprecation Warning
  .Deprecated("split_labeled_unlabeled")

  target_labeled <- subset(target, is.na(target) == FALSE)
  embedding_labeled <- embedding$embeddings[names(target_labeled), ]
  embedding_unlabeled <- embedding$embeddings[setdiff(names(target), names(target_labeled)), ]

  result <- list(
    embeddings_labeled = embedding_labeled,
    embeddings_unlabeled = embedding_unlabeled,
    targets_labeled = target_labeled
  )
  return(result)
}
