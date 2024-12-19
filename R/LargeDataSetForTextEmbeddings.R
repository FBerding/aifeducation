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

#' @title Abstract class for large data sets containing text embeddings
#' @description This object stores text embeddings which are usually produced by an object of class
#'   [TextEmbeddingModel]. The data of this objects is not stored in memory directly. By using memory mapping these
#'   objects allow to work with data sets which do not fit into memory/RAM.
#'
#'   [LargeDataSetForTextEmbeddings] are used for storing and managing the text embeddings created with objects of class
#'   [TextEmbeddingModel]. Objects of class [LargeDataSetForTextEmbeddings] serve as input for objects of class
#'   [TEClassifierRegular], [TEClassifierProtoNet], and [TEFeatureExtractor]. The main aim of this class is to provide a
#'   structured link between embedding models and classifiers. Since objects of this class save information on the text
#'   embedding model that created the text embedding it ensures that only embedding generated with same embedding model
#'   are combined. Furthermore, the stored information allows objects to check if embeddings of the correct text
#'   embedding model are used for training and predicting.
#'
#' @return Returns a new object of this class.
#' @export
#' @family Data Management
LargeDataSetForTextEmbeddings <- R6::R6Class(
  classname = "LargeDataSetForTextEmbeddings",
  inherit = LargeDataSetBase,
  private = list(
    # model_name `string` Name of the model that generates this embedding.
    model_name = NA,


    # Label of the model that generates this embedding.
    model_label = NA,


    # Date when the embedding generating model was created.
    model_date = NA,


    # Method of the underlying embedding model
    model_method = NA,


    # Version of the model that generated this embedding.
    model_version = NA,


    # Language of the model that generated this embedding.
    model_language = NA,


    # Maximal number of tokens that processes the generating model for a chunk.
    param_seq_length = NA,


    # Number of tokens that were added at the beginning of the sequence for the next chunk by this model.
    param_overlap = NA,

    # Maximal number of chunks which are supported by the generating model.
    param_chunks = NA,

    # Features of the embeddings
    param_features = NA,

    # Minimal layer to be included in the creation of embeddings.
    param_emb_layer_min = NA,

    # Maximal layer to be included in the creation of embeddings.
    param_emb_layer_max = NA,

    # Type of pooling tokens embeddings within each layer.
    param_emb_pool_type = NA,


    # Aggregation method of the hidden states. Deprecated. Included for backward compatibility.
    param_aggregation = NA,

    # List containing information on the feature extractor if the embeddings are compressed.
    feature_extractor = list(),

    # Variable for checking if the object is successfully configured. Only is this is TRUE the object can be used
    configured = FALSE,

    # Method for setting configured to TRUE
    set_configuration_to_TRUE = function() {
      private$configured <- TRUE
    },

    # Method for checking if the configuration is done successfully
    check_config_for_TRUE = function() {
      if (private$configured == FALSE) {
        stop("The object is not configured. Please call the method configure.")
      }
    }
  ),
  public = list(
    #' @description Creates a new object representing text embeddings.
    #'
    #' @param model_name `string` Name of the model that generates this embedding.
    #' @param model_label `string` Label of the model that generates this embedding.
    #' @param model_date `string` Date when the embedding generating model was created.
    #' @param model_method `string` Method of the underlying embedding model.
    #' @param model_version `string` Version of the model that generated this embedding.
    #' @param model_language `string` Language of the model that generated this embedding.
    #' @param param_seq_length `int` Maximum number of tokens that processes the generating model for a chunk.
    #' @param param_chunks `int` Maximum number of chunks which are supported by the generating model.
    #' @param param_features `int` Number of dimensions of the text embeddings.
    #' @param param_overlap `int` Number of tokens that were added at the beginning of the sequence for the next chunk
    #'   by this model.
    #'
    #' @param param_emb_layer_min `int` or `string` determining the first layer to be included in the creation of
    #'   embeddings.
    #' @param param_emb_layer_max `int` or `string` determining the last layer to be included in the creation of
    #'   embeddings.
    #' @param param_emb_pool_type `string` determining the method for pooling the token embeddings within each layer.
    #'
    #' @param param_aggregation `string` Aggregation method of the hidden states. Deprecated. Only included for backward
    #'   compatibility.
    #' @return The method returns a new object of this class.
    configure = function(model_name = NA,
                         model_label = NA,
                         model_date = NA,
                         model_method = NA,
                         model_version = NA,
                         model_language = NA,
                         param_seq_length = NA,
                         param_chunks = NULL,
                         param_features = NULL,
                         param_overlap = NULL,
                         param_emb_layer_min = NULL,
                         param_emb_layer_max = NULL,
                         param_emb_pool_type = NULL,
                         param_aggregation = NULL) {
      private$model_name <- model_name
      private$model_label <- model_label
      private$model_date <- model_date
      private$model_method <- model_method
      private$model_version <- model_version
      private$model_language <- model_language
      private$param_seq_length <- param_seq_length
      private$param_overlap <- param_overlap

      private$param_features <- param_features
      private$param_chunks <- param_chunks

      private$param_emb_layer_min <- param_emb_layer_min
      private$param_emb_layer_max <- param_emb_layer_max
      private$param_emb_pool_type <- param_emb_pool_type

      private$param_aggregation <- param_aggregation

      # Finalize configuration
      private$set_configuration_to_TRUE()
    },
    #-------------------------------------------------------------------------
    #' @description Method for checking if the model was successfully configured. An object can only be used if this
    #'   value is `TRUE`.
    #' @return `bool` `TRUE` if the model is fully configured. `FALSE` if not.
    is_configured = function() {
      return(private$configured)
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting the name (unique id) of the underlying text embedding model.
    #' @return Returns a `string` describing name of the text embedding model.
    get_text_embedding_model_name = function() {
      return(private$model_name)
    },
    #--------------------------------------------------------------------------
    #' @description Method for retrieving information about the model that generated this embedding.
    #' @return `list` containing all saved information about the underlying text embedding model.
    get_model_info = function() {
      tmp <- list(
        model_name = private$model_name,
        model_label = private$model_label,
        model_date = private$model_date,
        model_method = private$model_method,
        model_version = private$model_version,
        model_language = private$model_language,
        param_seq_length = private$param_seq_length,
        param_chunks = private$param_chunks,
        param_features = private$param_features,
        param_overlap = private$param_overlap,
        param_emb_layer_min = private$param_emb_layer_min,
        param_emb_layer_max = private$param_emb_layer_max,
        param_emb_pool_type = private$param_emb_pool_type,
        param_aggregation = private$param_aggregation
      )
      return(tmp)
    },
    #--------------------------------------------------------------------------
    #' @description loads an object of class [LargeDataSetForTextEmbeddings] from disk and updates the object to the
    #'   current version of the package.
    #' @param dir_path Path where the data set set is stored.
    #' @return Method does not return anything. It loads an object from disk.
    load_from_disk = function(dir_path) {
      if (self$is_configured() == TRUE) {
        stop("The object has already been configured. If you would like to add
             data please create a new object or use one of the following methods:
             'load', 'add_embeddings_from_array', 'add_embeddings_from_EmbeddedText' or
             'add_embeddings_from_LargeDataSetForTextEmbeddings'.")
      }

      # Load R file
      config_file <- load_R_config_state(dir_path)

      # Set configuration
      self$configure(
        model_name = config_file$private$model_name,
        model_label = config_file$private$model_label,
        model_date = config_file$private$model_date,
        model_method = config_file$private$model_method,
        model_version = config_file$private$model_version,
        model_language = config_file$private$model_language,
        param_seq_length = config_file$private$param_seq_length,
        param_chunks = config_file$private$param_chunks,
        param_features = config_file$private$param_features,
        param_overlap = config_file$private$param_overlap,
        param_emb_layer_min = config_file$private$param_emb_layer_min,
        param_emb_layer_max = config_file$private$param_emb_layer_max,
        param_emb_pool_type = config_file$private$param_emb_pool_type,
        param_aggregation = config_file$private$param_aggregation
      )

      # Check for feature extractor and add information
      if (is.null_or_na(config_file$private$feature_extractor$model_name) == FALSE) {
        self$add_feature_extractor_info(
          model_name = config_file$private$feature_extractor$model_name,
          model_label = config_file$private$feature_extractor$model_label,
          features = config_file$private$feature_extractor$features,
          method = config_file$private$feature_extractor$method,
          noise_factor = config_file$private$feature_extractor$noise_factor,
          optimizer = config_file$private$feature_extractor$optimizer
        )
      }

      # Load data set
      self$load(dir_path)
    },
    #--------------------------------------------------------------------------
    #' @description Method for retrieving the label of the model that generated this embedding.
    #' @return `string` Label of the corresponding text embedding model
    get_model_label = function() {
      private$check_config_for_TRUE()
      return(private$transformer_components$ml_framework)
    },

    #--------------------------------------------------------------------------
    #' @description Method setting information on the [TEFeatureExtractor] that was used to reduce the number of
    #'   dimensions of the text embeddings. This information should only be used if a [TEFeatureExtractor] was applied.
    #' @param model_name `string` Name of the underlying [TextEmbeddingModel].
    #' @param model_label `string` Label of the underlying [TextEmbeddingModel].
    #' @param features `int` Number of dimension (features) for the **compressed** text embeddings.
    #' @param method `string` Method that the [TEFeatureExtractor] applies for genereating the compressed text
    #'   embeddings.
    #' @param noise_factor `double` Noise factor of the [TEFeatureExtractor].
    #' @param optimizer `string` Optimizer used during training the [TEFeatureExtractor].
    #' @return Method does nothing return. It sets information on a [TEFeatureExtractor].
    add_feature_extractor_info = function(model_name,
                                          model_label = NA,
                                          features = NA,
                                          method = NA,
                                          noise_factor = NA,
                                          optimizer = NA) {
      private$feature_extractor <- list(
        model_name = model_name,
        model_label = model_label,
        features = features,
        method = method,
        noise_factor = noise_factor,
        optimizer = optimizer
      )
    },

    #--------------------------------------------------------------------------
    #' @description Method for receiving information on the [TEFeatureExtractor] that was used to reduce the number of
    #'   dimensions of the text embeddings.
    #' @return Returns a `list` with information on the [TEFeatureExtractor]. If no [TEFeatureExtractor] was used it
    #'   returns `NULL`.
    get_feature_extractor_info = function() {
      if (is.null_or_na(private$feature_extractor$model_name)) {
        return(NULL)
      } else {
        return(private$feature_extractor)
      }
    },

    #--------------------------------------------------------------------------
    #' @description Checks if the text embedding were reduced by a [TEFeatureExtractor].
    #' @return Returns `TRUE` if the number of dimensions was reduced by a [TEFeatureExtractor]. If not return `FALSE`.
    is_compressed = function() {
      return(!is.null_or_na(private$feature_extractor$model_name))
    },

    #--------------------------------------------------------------------------
    #' @description Number of chunks/times of the text embeddings.
    #' @return Returns an `int` describing the number of chunks/times of the text embeddings.
    get_times = function() {
      return(private$param_chunks)
    },

    #--------------------------------------------------------------------------
    #' @description Number of actual features/dimensions of the text embeddings.In the case a [TEFeatureExtractor] was
    #'   used the number of features is smaller as the original number of features. To receive the original number of
    #'   features (the number of features before applying a [TEFeatureExtractor]) you can use the method
    #'   `get_original_features` of this class.
    #' @return Returns an `int` describing the number of features/dimensions of the text embeddings.
    get_features = function() {
      if (self$is_compressed() == TRUE) {
        return(private$feature_extractor$features)
      } else {
        return(private$param_features)
      }
    },

    #--------------------------------------------------------------------------
    #' @description Number of original features/dimensions of the text embeddings.
    #' @return Returns an `int` describing the number of features/dimensions if no [TEFeatureExtractor]) is used or
    #'   before a [TEFeatureExtractor]) is applied.
    get_original_features = function() {
      return(private$param_features)
    },

    #--------------------------------------------------------------------------
    #' @description Method for adding new data to the data set from an `array`. Please note that the method does not
    #'   check if cases already exist in the data set. To reduce the data set to unique cases call the method
    #'   `reduce_to_unique_ids`.
    #' @param embedding_array `array` containing the text embeddings.
    #' @return The method does not return anything. It adds new data to the data set.
    add_embeddings_from_array = function(embedding_array) {
      private$check_config_for_TRUE()

      if (is.array(embedding_array) == FALSE) {
        stop("Input must be an array.")
      }
      if (self$get_features() != dim(embedding_array)[3]) {
        stop("The number of features does not fit to the underlying
             text embedding model. Please check if you either used compressed
             embedding for a dataset of uncompressed embeddings or uncrompressed
             embeddings for a dataset of compressed embeddings.")
      }
      if (self$get_times() != dim(embedding_array)[2]) {
        stop("Number of times/chunks does not fit to the underlying text embedding model.")
      }

      # Check the number of rows and duplicate if necessary
      n_cases <- dim(embedding_array)[1]
      if (n_cases == 1) {
        embedding_array <- array_form_bind(embedding_array, embedding_array)
      }
      # Transform to a python dict
      new_dataset_dict <- reticulate::dict(
        id = rownames(embedding_array),
        input = prepare_r_array_for_dataset(embedding_array),
        length = get_n_chunks(
          text_embeddings = embedding_array,
          features = self$get_features(),
          times = self$get_times()
        )
      )
      # Create new dataset
      new_dataset <- datasets$Dataset$from_dict(new_dataset_dict)
      # Check the number of rows and remove duplicate if necessary
      if (n_cases == 1) {
        new_dataset <- new_dataset$select(indices = list(as.integer(0)))
      }
      # add dataset
      private$add(new_dataset)
    },

    #--------------------------------------------------------------------------
    #' @description Method for adding new data to the data set from an [EmbeddedText]. Please note that the method does
    #'   not check if cases already exist in the data set. To reduce the data set to unique cases call the method
    #'   `reduce_to_unique_ids`.
    #' @param EmbeddedText Object of class [EmbeddedText].
    #' @return The method does not return anything. It adds new data to the data set.
    add_embeddings_from_EmbeddedText = function(EmbeddedText) {
      private$check_config_for_TRUE()

      if ("EmbeddedText" %in% class(EmbeddedText) == FALSE) {
        stop("Input must be an object of class EmbeddedText.")
      }

      # Select array
      embedding_array <- EmbeddedText$embeddings
      n_cases <- dim(embedding_array)[1]

      if (self$get_features() != dim(embedding_array)[3]) {
        stop("The number of features does not fit to the underlying
             text embedding model. Please check if you either used compressed
             embedding for a dataset of uncompressed embeddings or uncrompressed
             embeddings for a dataset of compressed embeddings.")
      }
      if (self$get_times() != dim(embedding_array)[2]) {
        stop("Number of times/chunks does not fit to the underlying text embedding model.")
      }

      # Check the number of rows and duplicate if necessary
      if (n_cases == 1) {
        embedding_array <- array_form_bind(embedding_array, embedding_array)
      }
      # Transform to a python dict
      new_dataset_dict <- reticulate::dict(
        id = rownames(embedding_array),
        input = prepare_r_array_for_dataset(embedding_array),
        length = get_n_chunks(
          text_embeddings = embedding_array,
          features = self$get_features(),
          times = self$get_times()
        )
      )
      # Create new dataset
      new_dataset <- datasets$Dataset$from_dict(new_dataset_dict)
      # Check the number of rows and remove duplicate if necessary
      if (n_cases == 1) {
        new_dataset <- new_dataset$select(indices = list(as.integer(0)))
      }
      # add dataset
      private$add(new_dataset)
    },
    #--------------------------------------------------------------------------
    #' @description Method for adding new data to the data set from an [LargeDataSetForTextEmbeddings]. Please note that
    #'   the method does not check if cases already exist in the data set. To reduce the data set to unique cases call
    #'   the method `reduce_to_unique_ids`.
    #' @param dataset Object of class [LargeDataSetForTextEmbeddings].
    #' @return The method does not return anything. It adds new data to the data set.
    add_embeddings_from_LargeDataSetForTextEmbeddings = function(dataset) {
      private$check_config_for_TRUE()

      # Argument Checking
      check_class(dataset, c("LargeDataSetForTextEmbeddings", FALSE))

      # Add new data
      if (dataset$get_text_embedding_model_name() == private$model_name) {
        private$add(dataset$get_dataset())
      } else {
        stop("The underlying TextEmbeddingModel of the new dataset is not
             identical to the TextEmbeddinModel underlying this data set.")
      }
    },
    #--------------------------------------------------------------------------
    #' @description Method for converting this object to an object of class [EmbeddedText].
    #'
    #'   **Attention** This object uses memory mapping to allow the usage of data sets
    #'   that do not fit into memory. By calling this method the data set will be loaded and stored into memory/RAM.
    #'   This may lead to an out-of-memory error.
    #' @return LargeDataSetForTextEmbeddings an object of class [EmbeddedText] which is stored in the memory/RAM.
    convert_to_EmbeddedText = function() {
      private$check_config_for_TRUE()

      new_data_set <- EmbeddedText$new()
      new_data_set$configure(
        model_name = private$model_name,
        model_label = private$model_label,
        model_date = private$model_date,
        model_method = private$model_method,
        model_version = private$model_version,
        model_language = private$model_language,
        param_seq_length = private$param_seq_length,
        param_chunks = private$param_chunks,
        param_features = private$param_features,
        param_overlap = private$param_overlap,
        param_emb_layer_min = private$param_emb_layer_min,
        param_emb_layer_max = private$param_emb_layer_max,
        param_emb_pool_type = private$param_emb_pool_type,
        param_aggregation = private$param_aggregation,
        embeddings = py_dataset_to_embeddings(self$get_dataset())
      )

      if (self$is_compressed() == TRUE) {
        new_data_set$add_feature_extractor_info(
          model_name = private$feature_extractor$model_name,
          model_label = private$feature_extractor$model_label,
          features = private$feature_extractor$features,
          method = private$feature_extractor$method,
          noise_factor = private$feature_extractor$noise_factor,
          optimizer = private$feature_extractor$optimizer
        )
      }
      return(new_data_set)
    }
  )
)
