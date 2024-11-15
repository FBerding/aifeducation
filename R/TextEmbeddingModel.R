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

#' @title Text embedding model
#' @description This `R6` class stores a text embedding model which can be used to tokenize, encode, decode, and embed
#'   raw texts. The object provides a unique interface for different text processing methods.
#' @return Objects of class [TextEmbeddingModel] transform raw texts into numerical representations which can be used
#'   for downstream tasks. For this aim objects of this class allow to tokenize raw texts, to encode tokens to sequences
#'   of integers, and to decode sequences of integers back to tokens.
#' @family Text Embedding
#' @export
TextEmbeddingModel <- R6::R6Class(
  classname = "TextEmbeddingModel",
  private = list(
    # Variable for checking if the object is successfully configured. Only is
    # this is TRUE the object can be used
    configured = FALSE,
    r_package_versions = list(
      aifeducation = NA,
      smotefamily = NA,
      reticulate = NA
    ),
    py_package_versions = list(
      tensorflow = NA,
      torch = NA,
      keras = NA,
      numpy = NA
    ),
    supported_transformers = list(
      tensorflow = c(
        "bert",
        "roberta",
        "longformer",
        "funnel",
        "deberta_v2"
      ),
      pytorch = c(
        "bert",
        "roberta",
        "longformer",
        "funnel",
        "deberta_v2",
        "mpnet"
      )
    ),
    basic_components = list(
      method = NULL,
      max_length = NULL
    ),
    transformer_components = list(
      model = NULL,
      model_mlm = NULL,
      tokenizer = NULL,
      emb_layer_min = NULL,
      emb_layer_max = NULL,
      emb_pool_type = NULL,
      chunks = NULL,
      features = NULL,
      overlap = NULL,
      ml_framework = NULL
    ),
    model_info = list(
      model_license = NA,
      model_name_root = NA,
      model_id = NA,
      model_name = NA,
      model_label = NA,
      model_date = NA,
      model_language = NA
    ),
    sustainability = list(
      sustainability_tracked = FALSE,
      track_log = NA
    ),
    publication_info = list(
      developed_by = list(
        authors = NULL,
        citation = NULL,
        url = NULL
      ),
      modified_by = list(
        authors = NULL,
        citation = NULL,
        url = NULL
      )
    ),
    model_description = list(
      eng = NULL,
      native = NULL,
      abstract_eng = NULL,
      abstract_native = NULL,
      keywords_eng = NULL,
      keywords_native = NULL,
      license = NA
    ),
    #---------------------------------------------------------------------------
    # Method for setting configured to TRUE
    set_configuration_to_TRUE = function() {
      private$configured <- TRUE
    },
    #---------------------------------------------------------------------------
    # Method for checking if the configuration is done successfully
    check_config_for_TRUE = function() {
      if (private$configured == FALSE) {
        stop("The object is not configured. Please call the method configure.")
      }
    },
    #--------------------------------------------------------------------------
    # Method for setting the model info
    set_model_info = function(model_name_root, model_id, label, model_date, model_language) {
      private$model_info$model_name_root <- model_name_root
      private$model_info$model_id <- model_id
      private$model_info$model_name <- paste0(model_name_root, "_ID_", model_id)
      private$model_info$model_label <- label
      private$model_info$model_date <- model_date
      private$model_info$model_language <- model_language
    },
    #--------------------------------------------------------------------------
    # Method for setting package versions
    set_package_versions = function() {
      private$r_package_versions$aifeducation <- packageVersion("aifeducation")
      private$r_package_versions$reticulate <- packageVersion("reticulate")

      if (!is.null_or_na(private$ml_framework)) {
        if (private$ml_framework == "pytorch") {
          private$py_package_versions$torch <- torch["__version__"]
          private$py_package_versions$tensorflow <- NULL
          private$py_package_versions$keras <- NULL
        } else {
          private$py_package_versions$torch <- NULL
          private$py_package_versions$tensorflow <- tf$version$VERSION
          private$py_package_versions$keras <- keras["__version__"]
        }
        private$py_package_versions$numpy <- np$version$short_version
      }
    },
    #-------------------------------------------------------------------------
    load_reload_python_scripts = function() {
      if (private$basic_components$method == "mpnet") {
        reticulate::py_run_file(system.file("python/MPNetForMPLM_PT.py",
          package = "aifeducation"
        ))
      }
    },
    #-------------------------------------------------------------------------
    # Method for loading sustainability data
    load_sustainability_data = function(model_dir) {
      sustainability_datalog_path <- paste0(model_dir, "/", "sustainability.csv")
      if (file.exists(sustainability_datalog_path)) {
        tmp_sustainability_data <- read.csv(sustainability_datalog_path)
        private$sustainability$sustainability_tracked <- TRUE
        private$sustainability$track_log <- tmp_sustainability_data
      } else {
        private$sustainability$sustainability_tracked <- FALSE
        private$sustainability$track_log <- NA
      }
    },
    #-------------------------------------------------------------------------
    # Method for saving sustainability data
    save_sustainability_data = function(dir_path, folder_name) {
      save_location <- paste0(dir_path, "/", folder_name)
      create_dir(dir_path, trace = TRUE, msg_fun = FALSE)
      sustain_matrix <- private$sustainability$track_log
      write.csv(
        x = sustain_matrix,
        file = paste0(save_location, "/", "sustainability.csv"),
        row.names = FALSE
      )
    },
    #--------------------------------------------------------------------------
    # Method for loading training history
    load_training_history = function(model_dir) {
      training_datalog_path <- paste0(model_dir, "/", "history.log")
      if (file.exists(training_datalog_path) == TRUE) {
        self$last_training$history <- read.csv2(file = training_datalog_path)
      } else {
        self$last_training$history <- NA
      }
    },
    #--------------------------------------------------------------------------
    # Method for saving training history
    save_training_history = function(dir_path, folder_name) {
      if (is.null_or_na(self$last_training$history) == FALSE) {
        save_location <- paste0(dir_path, "/", folder_name)
        create_dir(dir_path, trace = TRUE, msg_fun = FALSE)
        write.csv2(
          x = self$last_training$history,
          file = paste0(save_location, "/", "history.log"),
          row.names = FALSE,
          quote = FALSE
        )
      }
    },
    #------------------------------------------------------------------------
    # Method for loading tokenizer statistics
    load_tokenizer_statistics=function(model_dir){
      path <- paste0(model_dir, "/", "tokenizer_statistics.csv")
      if (file.exists(path) == TRUE) {
        self$tokenizer_statistics <- read.csv(file = path)
      } else {
        self$tokenizer_statistics <- NA
      }
    },
    #------------------------------------------------------------------------
    # Method for saving tokenizer statistics
    save_tokenizer_statistics = function(dir_path, folder_name) {
      if (is.null_or_na(self$tokenizer_statistics) == FALSE) {
        save_location <- paste0(dir_path, "/", folder_name)
        create_dir(dir_path, trace = TRUE, msg_fun = FALSE)
        write.csv(
          x = self$tokenizer_statistics,
          file = paste0(save_location, "/", "tokenizer_statistics.csv"),
          row.names = FALSE,
          quote = FALSE
        )
      }
    },
    #-------------------------------------------------------------------------
    # Method for checking and setting the embedding configuration
    check_and_set_embedding_layers = function(emb_layer_min,
                                              emb_layer_max) {
      if (private$basic_components$method == "funnel") {
        max_layers_funnel <- sum(private$transformer_components$model$config$block_repeats *
          private$transformer_components$model$config$block_sizes)

        if (emb_layer_min == "first") {
          emb_layer_min <- 1
        } else if (emb_layer_min == "middle") {
          emb_layer_min <- floor(0.5 * max_layers_funnel)
        } else if (emb_layer_min == "2_3_layer") {
          emb_layer_min <- floor(2 / 3 * max_layers_funnel)
        } else if (emb_layer_min == "last") {
          emb_layer_min <- max_layers_funnel
        }

        if (emb_layer_max == "first") {
          emb_layer_max <- 1
        } else if (emb_layer_max == "middle") {
          emb_layer_max <- floor(0.5 * max_layers_funnel)
        } else if (emb_layer_max == "2_3_layer") {
          emb_layer_max <- floor(2 / 3 * max_layers_funnel)
        } else if (emb_layer_max == "last") {
          emb_layer_max <- max_layers_funnel
        }
      } else {
        if (emb_layer_min == "first") {
          emb_layer_min <- 1
        } else if (emb_layer_min == "middle") {
          emb_layer_min <- floor(0.5 * private$transformer_components$model$config$num_hidden_layers)
        } else if (emb_layer_min == "2_3_layer") {
          emb_layer_min <- floor(2 / 3 * private$transformer_components$model$config$num_hidden_layers)
        } else if (emb_layer_min == "last") {
          emb_layer_min <- private$transformer_components$model$config$num_hidden_layers
        }

        if (emb_layer_max == "first") {
          emb_layer_max <- 1
        } else if (emb_layer_max == "middle") {
          emb_layer_max <- floor(0.5 * private$transformer_components$model$config$num_hidden_layers)
        } else if (emb_layer_max == "2_3_layer") {
          emb_layer_max <- floor(2 / 3 * private$transformer_components$model$config$num_hidden_layers)
        } else if (emb_layer_max == "last") {
          emb_layer_max <- private$transformer_components$model$config$num_hidden_layers
        }
      }

      # Check requested configuration
      if (emb_layer_min > emb_layer_max) {
        stop("emb_layer_min layer must be smaller or equal emb_layer_max.")
      }
      if (emb_layer_min < 1) {
        stop("emb_laser_min must be at least 1.")
      }
      if (private$basic_components$method == "funnel") {
        if (emb_layer_max > private$transformer_components$model$config$num_hidden_layers) {
          stop(paste0(
            "emb_layer_max can not exceed the number of layers. The transformer has",
            max_layers_funnel, "layers."
          ))
        }
      } else {
        if (emb_layer_max > private$transformer_components$model$config$num_hidden_layers) {
          stop(paste0(
            "emb_layer_max can not exceed the number of layers. The transformer has",
            private$transformer_components$model$config$num_hidden_layers, "layers."
          ))
        }
      }

      if (is.integer(as.integer(emb_layer_min)) == FALSE | is.integer(as.integer(emb_layer_max)) == FALSE) {
        stop("emb_layer_min and emb_layer_max must be integers or the following string:
               'first','last','middle','2_3_layer'")
      }

      private$transformer_components$emb_layer_min <- emb_layer_min
      private$transformer_components$emb_layer_max <- emb_layer_max
    },
    #-------------------------------------------------------------------------
    # Method for checking and setting pooling type
    check_and_set_pooling_type = function(emb_pool_type) {
      if (emb_pool_type %in% c("cls", "average") == FALSE) {
        stop("emb_pool_type must be 'cls' or 'average'.")
      }
      if (private$basic_components$method == "funnel" & emb_pool_type != "cls") {
        stop("Funnel currently supports only cls as pooling type.")
      }
      private$transformer_components$emb_pool_type <- emb_pool_type
    },
    #-------------------------------------------------------------------------
    # Method for checking and setting max_length
    check_and_set_max_length = function(max_length) {
      if (private$basic_components$method == "longformer" |
        private$basic_components$method == "roberta") {
        if (max_length > (private$transformer_components$model$config$max_position_embeddings)) {
          stop(paste(
            "max_length is", max_length, ". This value is not allowed to exceed",
            private$transformer_components$model$config$max_position_embeddings
          ))
        } else {
          private$basic_components$max_length <- as.integer(max_length)
        }
      } else {
        private$basic_components$max_length <- as.integer(max_length)
      }
    },
    #--------------------------------------------------------------------------
    # Method for loading transformer models and tokenizers
    load_transformer_and_tokenizer = function(model_dir) {
      #------------------------------------------------------------------------
      # Search for the corresponding files and set loading behavior.
      # If the model exists based on another ml framework try to
      # load from the other framework
      if (private$transformer_components$ml_framework == "tensorflow") {
        if (file.exists(paste0(model_dir, "/tf_model.h5"))) {
          from_pt <- FALSE
        } else if (file.exists(paste0(model_dir, "/pytorch_model.bin")) |
          file.exists(paste0(model_dir, "/model.safetensors"))) {
          from_pt <- TRUE
        } else {
          stop("Directory does not contain a tf_model.h5, pytorch_model.bin
                 or a model.saftensors file.")
        }
      } else {
        if (file.exists(paste0(model_dir, "/pytorch_model.bin")) |
          file.exists(paste0(model_dir, "/model.safetensors"))) {
          from_tf <- FALSE
        } else if (file.exists(paste0(model_dir, "/tf_model.h5"))) {
          from_tf <- TRUE
        } else {
          stop("Directory does not contain a tf_model.h5,pytorch_model.bin
                 or a model.saftensors file.")
        }
      }

      #------------------------------------------------------------------------
      # In the case of pytorch
      # Check to load from pt/bin or safetensors
      # Use safetensors as preferred method
      if (private$transformer_components$ml_framework == "pytorch") {
        if ((file.exists(paste0(model_dir, "/model.safetensors")) == FALSE &
          from_tf == FALSE) |
          reticulate::py_module_available("safetensors") == FALSE) {
          load_safe <- FALSE
        } else {
          load_safe <- TRUE
        }
      }

      # Load models and tokenizer-----------------------------------------------

      if (private$basic_components$method == "bert") {
        private$transformer_components$tokenizer <- transformers$AutoTokenizer$from_pretrained(model_dir)
        if (private$transformer_components$ml_framework == "tensorflow") {
          private$transformer_components$model <- transformers$TFBertModel$from_pretrained(model_dir, from_pt = from_pt)
          private$transformer_components$model_mlm <- transformers$TFBertForMaskedLM$from_pretrained(model_dir, from_pt = from_pt)
        } else {
          private$transformer_components$model <- transformers$BertModel$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
          private$transformer_components$model_mlm <- transformers$BertForMaskedLM$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
        }
      } else if (private$basic_components$method == "roberta") {
        private$transformer_components$tokenizer <- transformers$RobertaTokenizerFast$from_pretrained(model_dir)
        if (private$transformer_components$ml_framework == "tensorflow") {
          private$transformer_components$model <- transformers$TFRobertaModel$from_pretrained(model_dir, from_pt = from_pt)
          private$transformer_components$model_mlm <- transformers$TFRobertaForMaskedLM$from_pretrained(model_dir, from_pt = from_pt)
        } else {
          private$transformer_components$model <- transformers$RobertaModel$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
          private$transformer_components$model_mlm <- transformers$RobertaForMaskedLM$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
        }
      } else if (private$basic_components$method == "longformer") {
        private$transformer_components$tokenizer <- transformers$LongformerTokenizerFast$from_pretrained(model_dir)
        if (private$transformer_components$ml_framework == "tensorflow") {
          private$transformer_components$model <- transformers$TFLongformerModel$from_pretrained(model_dir, from_pt = from_pt)
          private$transformer_components$model_mlm <- transformers$TFLongformerForMaskedLM$from_pretrained(model_dir, from_pt = from_pt)
        } else {
          private$transformer_components$model <- transformers$LongformerModel$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
          private$transformer_components$model_mlm <- transformers$LongformerForMaskedLM$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
        }
      } else if (private$basic_components$method == "funnel") {
        private$transformer_components$tokenizer <- transformers$AutoTokenizer$from_pretrained(model_dir)
        if (private$transformer_components$ml_framework == "tensorflow") {
          private$transformer_components$model <- transformers$TFFunnelBaseModel$from_pretrained(model_dir, from_pt = from_pt)
          private$transformer_components$model_mlm <- transformers$TFFunnelForMaskedLM$from_pretrained(model_dir, from_pt = from_pt)
        } else {
          private$transformer_components$model <- transformers$FunnelBaseModel$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
          private$transformer_components$model_mlm <- transformers$FunnelForMaskedLM$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
        }
      } else if (private$basic_components$method == "deberta_v2") {
        private$transformer_components$tokenizer <- transformers$AutoTokenizer$from_pretrained(model_dir)
        if (private$transformer_components$ml_framework == "tensorflow") {
          private$transformer_components$model <- transformers$TFDebertaV2Model$from_pretrained(model_dir, from_pt = from_pt)
          private$transformer_components$model_mlm <- transformers$TFDebertaForMaskedLM$from_pretrained(model_dir, from_pt = from_pt)
        } else {
          private$transformer_components$model <- transformers$DebertaV2Model$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
          private$transformer_components$model_mlm <- transformers$DebertaForMaskedLM$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
        }
      } else if (private$basic_components$method == "mpnet") {
        private$transformer_components$tokenizer <- transformers$AutoTokenizer$from_pretrained(model_dir)
        if (private$transformer_components$ml_framework == "pytorch") {
          private$transformer_components$model <- transformers$MPNetModel$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
          private$transformer_components$model_mlm <- py$MPNetForMPLM_PT$from_pretrained(model_dir,
            from_tf = from_tf,
            use_safetensors = load_safe
          )
        }
      }
    }
  ),
  public = list(

    #' @field last_training ('list()')\cr
    #' List for storing the history and the results of the last training. This
    #' information will be overwritten if a new training is started.
    last_training = list(
      history = NULL
    ),

    #' @field tokenizer_statistics ('matrix()')\cr
    #' Matrix containing the tokenizer statistics for the creation of the tokenizer
    #' and all training runs according to Kaya & Tantuğ (2024).
    #'
    #' Kaya, Y. B., & Tantuğ, A. C. (2024). Effect of tokenization granularity for Turkish
    #' large language models. Intelligent Systems with Applications, 21, 200335.
    #' https://doi.org/10.1016/j.iswa.2024.200335
    tokenizer_statistics = NULL,

    #--------------------------------------------------------------------------
    #' @description Method for creating a new text embedding model
    #' @param model_name `string` containing the name of the new model.
    #' @param model_label `string` containing the label/title of the new model.
    #' @param model_language `string` containing the language which the model
    #' represents (e.g., English).
    #' @param ml_framework `string` Framework to use for the model.
    #' `ml_framework="tensorflow"` for 'tensorflow' and `ml_framework="pytorch"`
    #' for 'pytorch'. Only relevant for transformer models. To request bag-of-words model
    #' set `ml_framework=NULL`.
    #' @param method `string` determining the kind of embedding model. Currently
    #' the following models are supported:
    #' `method="bert"` for Bidirectional Encoder Representations from Transformers (BERT),
    #' `method="roberta"` for A Robustly Optimized BERT Pretraining Approach (RoBERTa),
    #' `method="longformer"` for Long-Document Transformer,
    #' `method="funnel"` for Funnel-Transformer,
    #' `method="deberta_v2"` for Decoding-enhanced BERT with Disentangled Attention (DeBERTa V2),
    #' `method="glove"`` for GlobalVector Clusters, and `method="lda"` for topic modeling. See
    #' details for more information.
    #' @param max_length `int` determining the maximum length of token
    #' sequences used in transformer models. Not relevant for the other methods.
    #' @param chunks `int` Maximum number of chunks. Only relevant for
    #' transformer models.
    #' @param overlap `int` determining the number of tokens which should be added
    #' at the beginning of the next chunk. Only relevant for transformer models.
    #' @param emb_layer_min `int` or `string` determining the first layer to be included
    #' in the creation of embeddings. An integer correspondents to the layer number. The first
    #' layer has the number 1. Instead of an integer the following strings are possible:
    #' `"start"` for the first layer, `"middle"` for the middle layer,
    #' `"2_3_layer"` for the layer two-third layer, and `"last"` for the last layer.
    #' @param emb_layer_max `int` or `string` determining the last layer to be included
    #' in the creation of embeddings. An integer correspondents to the layer number. The first
    #' layer has the number 1. Instead of an integer the following strings are possible:
    #' `"start"` for the first layer, `"middle"` for the middle layer,
    #' `"2_3_layer"` for the layer two-third layer, and `"last"` for the last layer.
    #' @param emb_pool_type `string` determining the method for pooling the token embeddings
    #' within each layer. If `"cls"` only the embedding of the CLS token is used. If
    #' `"average"` the token embedding of all tokens are averaged (excluding padding tokens).
    #' `"cls` is not supported for `method="funnel"`.
    #' @param model_dir `string` path to the directory where the
    #' BERT model is stored.
    #' @param trace `bool` `TRUE` prints information about the progress.
    #' `FALSE` does not.
    #' @return Returns an object of class [TextEmbeddingModel].
    #' @details
    #'
    #' In the case of any transformer (e.g.`method="bert"`,
    #' `method="roberta"`, and `method="longformer"`),
    #' a pretrained transformer model must be supplied via `model_dir`.
    #'
    #' @import reticulate
    #' @import stats
    #' @import reshape2
    configure = function(model_name = NULL,
                         model_label = NULL,
                         model_language = NULL,
                         method = NULL,
                         ml_framework = "pytorch",
                         max_length = 0,
                         chunks = 1,
                         overlap = 0,
                         emb_layer_min = "middle",
                         emb_layer_max = "2_3_layer",
                         emb_pool_type = "average",
                         model_dir = NULL,
                         trace = FALSE) {
      # Parameter check---------------------------------------------------------
      check_type(model_name, "string", FALSE)
      check_type(model_label, "string", FALSE)
      check_type(model_language, "string", FALSE)
      check_type(ml_framework, "string", TRUE)
      if ((ml_framework %in% c("tensorflow", "pytorch")) == FALSE) {
        stop("ml_framework must be 'tensorflow' or 'pytorch'.")
      }
      check_type(method, "string", FALSE)
      if (method %in% c(private$supported_transformers[[ml_framework]]) == FALSE) {
        stop(
          paste(
            "For",
            ml_framework,
            "method must be",
            paste(private$supported_transformers[[ml_framework]], collapse = ", ")
          )
        )
      }


      check_type(max_length, "int", FALSE)
      check_type(chunks, "int", FALSE)
      check_type(overlap, "int", FALSE)
      # emb_layer_min
      # emb_layer_max
      # emb_pool_type
      check_type(model_dir, "string", FALSE)

      # Set model info
      private$set_model_info(
        model_name_root = model_name,
        model_id = generate_id(16),
        label = model_label,
        model_date = date(),
        model_language = model_language
      )

      # Set package versions
      private$set_package_versions()

      # basic_components
      private$basic_components$method <- method

      # Load python scripts
      # Must be called after setting private$basic_components$method
      private$load_reload_python_scripts()

      # transformer_components
      private$transformer_components$ml_framework <- ml_framework
      private$transformer_components$chunks <- chunks
      private$transformer_components$overlap <- overlap

      # Load transformer models and tokenizer
      private$load_transformer_and_tokenizer(model_dir = model_dir)

      # Check max length
      private$check_and_set_max_length(max_length)

      # Load Sustainability Data
      private$load_sustainability_data(model_dir = model_dir)

      # Load Training history
      private$load_training_history(model_dir = model_dir)

      #Load Tokenizer statistics
      private$load_tokenizer_statistics(model_dir = model_dir)

      # Check and Set Embedding Configuration
      private$check_and_set_embedding_layers(
        emb_layer_min = emb_layer_min,
        emb_layer_max = emb_layer_max
      )

      # Check and set pooling type
      private$check_and_set_pooling_type(emb_pool_type)
    },
    #--------------------------------------------------------------------------
    #' @description loads an object from disk
    #' and updates the object to the current version of the package.
    #' @param dir_path Path where the object set is stored.
    #' @return Method does not return anything. It loads an object from disk.
    load_from_disk = function(dir_path) {
      if (self$is_configured() == TRUE) {
        stop("The object has already been configured. Please use the method
             'load' for loading the weights of a model.")
      }

      # Load R file
      config_file <- load_R_config_state(dir_path)

      # Set basic configuration
      private$basic_components <- list(
        method = config_file$private$basic_components$method,
        max_length = config_file$private$basic_components$max_length
      )

      # Load python scripts
      # Must be called after setting private$basic_components$method
      private$load_reload_python_scripts()

      # Set transformer configuration
      private$transformer_components <- list(
        model = NULL,
        model_mlm = NULL,
        tokenizer = NULL,
        emb_layer_min = config_file$private$transformer_components$emb_layer_min,
        emb_layer_max = config_file$private$transformer_components$emb_layer_max,
        emb_pool_type = config_file$private$transformer_components$emb_pool_type,
        chunks = config_file$private$transformer_components$chunks,
        features = config_file$private$transformer_components$features,
        overlap = config_file$private$transformer_components$overlap,
        ml_framework = config_file$private$transformer_components$ml_framework
      )

      # Set model info
      private$set_model_info(
        model_name_root = config_file$private$model_info$model_name_root,
        model_id = config_file$private$model_info$model_id,
        label = config_file$private$model_info$model_label,
        model_date = config_file$private$model_info$model_date,
        model_language = config_file$private$model_info$model_language
      )

      # Set license
      self$set_software_license(config_file$private$model_info$model_license)
      self$set_documentation_license(config_file$private$model_description$license)

      # Set description and documentation
      self$set_model_description(
        eng = config_file$private$model_description$eng,
        native = config_file$private$model_description$native,
        abstract_eng = config_file$private$model_description$abstract_eng,
        abstract_native = config_file$private$model_description$abstract_native,
        keywords_eng = config_file$private$model_description$keywords_eng,
        keywords_native = config_file$private$model_description$keywords_native
      )

      # Set publication info
      self$set_publication_info(
        type = "developer",
        authors = config_file$private$publication_info$developed_by$authors,
        citation = config_file$private$publication_info$developed_by$citation,
        url = config_file$private$publication_info$developed_by$url
      )
      self$set_publication_info(
        type = "modifier",
        authors = config_file$private$publication_info$modified_by$authors,
        citation = config_file$private$publication_info$modified_by$citation,
        url = config_file$private$publication_info$modified_by$modifier$url
      )

      # Get and set original package versions
      private$r_package_versions$aifeducation <- config_file$private$r_package_versions$aifeducation
      private$r_package_versions$reticulate <- config_file$private$r_package_versions$reticulate

      private$py_package_versions$torch <- config_file$private$py_package_versions$torch
      private$py_package_versions$tensorflow <- config_file$private$py_package_versions$tensorflow
      private$py_package_versions$keras <- config_file$private$py_package_versions$keras
      private$py_package_versions$numpy <- config_file$private$py_package_versions$numpy

      # Finalize config
      private$set_configuration_to_TRUE()

      # load AI model
      self$load(dir_path = dir_path)
    },
    #--------------------------------------------------------------------------
    #' @description Method for loading a transformers model into R.
    #' @param dir_path `string` containing the path to the relevant
    #' model directory.
    #' @return Function does not return a value. It is used for loading a saved
    #' transformer model into the R interface.
    #'
    #' @importFrom utils read.csv
    load = function(dir_path) {
      check_type(dir_path, "string", FALSE)

      # Load transformer models and tokenizer
      model_dir_main <- paste0(dir_path, "/", "model_data")
      private$load_transformer_and_tokenizer(model_dir = model_dir_main)

      # Load Sustainability Data
      private$load_sustainability_data(model_dir = dir_path)

      # Load Training history
      private$load_training_history(model_dir = dir_path)

      #Load Tokenizer statistics
      private$load_tokenizer_statistics(model_dir = dir_path)
    },
    #--------------------------------------------------------------------------
    #' @description Method for saving a transformer model on disk.Relevant
    #' only for transformer models.
    #' @param dir_path `string` containing the path to the relevant
    #' model directory.
    #' @param folder_name `string` Name for the folder created within the directory.
    #' This folder contains all model files.
    #' @return Function does not return a value. It is used for saving a transformer model
    #' to disk.
    #'
    #' @importFrom utils write.csv
    save = function(dir_path, folder_name) {
      check_type(dir_path, "string", FALSE)
      check_type(folder_name, "string", FALSE)


      if (private$transformer_components$ml_framework == "tensorflow") {
        save_format <- "h5"
      } else if (private$transformer_components$ml_framework == "pytorch") {
        save_format <- "safetensors"
      }

      save_location <- paste0(dir_path, "/", folder_name)
      create_dir(dir_path, trace = TRUE, msg_fun = FALSE)
      create_dir(save_location, trace = TRUE, msg_fun = FALSE)

      model_dir_data_path <- paste0(save_location, "/", "model_data")

      if (private$transformer_components$ml_framework == "pytorch") {
        if (save_format == "safetensors" & reticulate::py_module_available("safetensors") == TRUE) {
          private$transformer_components$model$save_pretrained(
            save_directory = model_dir_data_path,
            safe_serilization = TRUE
          )
          private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)
        } else if (save_format == "safetensors" & reticulate::py_module_available("safetensors") == FALSE) {
          private$transformer_components$model$save_pretrained(
            save_directory = model_dir_data_path,
            safe_serilization = FALSE
          )
          private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)
          warning("Python library 'safetensors' is not available. Saving model in standard
                  pytorch format.")
        } else if (save_format == "pt") {
          private$transformer_components$model$save_pretrained(
            save_directory = model_dir_data_path,
            safe_serilization = FALSE
          )
          private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)
        }
      } else {
        private$transformer_components$model$save_pretrained(save_directory = model_dir_data_path)
        private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)
      }

      # Save Sustainability Data
      private$save_sustainability_data(
        dir_path = dir_path,
        folder_name = folder_name
      )

      # Save training history
      private$save_training_history(
        dir_path = dir_path,
        folder_name = folder_name
      )

      # Save tokenizer statistics
      private$save_tokenizer_statistics(
        dir_path = dir_path,
        folder_name = folder_name
      )
    },
    #-------------------------------------------------------------------------
    #' @description Method for encoding words of raw texts into integers.
    #' @param raw_text `vector`containing the raw texts.
    #' @param token_encodings_only `bool` If `TRUE`, only the token
    #' encodings are returned. If `FALSE`, the complete encoding is returned
    #' which is important for BERT models.
    #' @param to_int `bool` If `TRUE` the integer ids of the tokens are
    #' returned. If `FALSE` the tokens are returned. Argument only applies
    #' for transformer models and if `token_encodings_only=TRUE`.
    #' @param trace `bool` If `TRUE`, information of the progress
    #' is printed. `FALSE` if not requested.
    #' @return `list` containing the integer or token sequences of the raw texts with
    #' special tokens.
    encode = function(raw_text,
                      token_encodings_only = FALSE,
                      to_int = TRUE,
                      trace = FALSE) {
      # Checking
      check_type(raw_text, "vector", FALSE)
      check_type(token_encodings_only, "bool", FALSE)
      check_type(to_int, "bool", FALSE)
      check_type(trace, "bool", FALSE)

      # Start
      n_units <- length(raw_text)

      chunk_list <- vector(length = n_units)
      encodings <- NULL
      #---------------------------------------------------------------------
      if (token_encodings_only == TRUE) {
        encodings_only <- NULL
        for (i in 1:n_units) {
          tokens_unit <- NULL

          tokens <- private$transformer_components$tokenizer(
            raw_text[i],
            stride = as.integer(private$transformer_components$overlap),
            padding = "max_length",
            truncation = TRUE,
            return_overflowing_tokens = TRUE,
            return_length = FALSE,
            return_offsets_mapping = FALSE,
            return_attention_mask = FALSE,
            max_length = as.integer(private$basic_components$max_length),
            return_tensors = "np"
          )

          seq_len <- nrow(tokens[["input_ids"]])

          chunks <- min(seq_len, private$transformer_components$chunks)

          for (j in 1:chunks) {
            tokens_unit[j] <- list(tokens["input_ids"][j, ])
            if (trace == TRUE) {
              cat(paste(date(), i, "/", n_units, "block", j, "/", chunks, "\n"))
            }
          }
          encodings_only[i] <- list(tokens_unit)
        }
        if (to_int == TRUE) {
          return(encodings_only)
        } else {
          # Convert ids to tokens

          token_seq_list <- NULL
          for (i in 1:length(encodings_only)) {
            tmp_sequence <- encodings_only[[i]]
            tmp_seqeunce_tok <- NULL
            for (j in 1:length(tmp_sequence)) {
              tmp_seqeunce_tok[length(tmp_seqeunce_tok) + 1] <- list(
                private$transformer_components$tokenizer$convert_ids_to_tokens(
                  ids = as.integer(tmp_sequence[[j]]), skip_special_tokens = FALSE
                )
              )
            }
            token_seq_list[length(token_seq_list) + 1] <- list(tmp_seqeunce_tok)
          }
          return(token_seq_list)
        }

        #--------------------------------------------------------------------
      } else {
        # text_chunks<-NULL
        encodings <- NULL
        for (i in 1:n_units) {
          return_token_type_ids <- (private$basic_components$method != AIFETrType$mpnet)

          if (private$transformer_components$ml_framework == "tensorflow") {
            tokens <- private$transformer_components$tokenizer(
              raw_text[i],
              stride = as.integer(private$transformer_components$overlap),
              padding = "max_length",
              truncation = TRUE,
              max_length = as.integer(private$basic_components$max_length),
              return_overflowing_tokens = TRUE,
              return_length = FALSE,
              return_offsets_mapping = FALSE,
              return_attention_mask = TRUE,
              return_token_type_ids = return_token_type_ids,
              return_tensors = "tf"
            )
          } else {
            tokens <- private$transformer_components$tokenizer(
              raw_text[i],
              stride = as.integer(private$transformer_components$overlap),
              padding = "max_length",
              truncation = TRUE,
              max_length = as.integer(private$basic_components$max_length),
              return_overflowing_tokens = TRUE,
              return_length = FALSE,
              return_offsets_mapping = FALSE,
              return_attention_mask = TRUE,
              return_token_type_ids = return_token_type_ids,
              return_tensors = "pt"
            )
          }

          tmp_dataset <- datasets$Dataset$from_dict(tokens)

          seq_len <- tmp_dataset$num_rows
          chunk_list[i] <- min(seq_len, private$transformer_components$chunks)

          if (chunk_list[i] == 1) {
            encodings <- datasets$concatenate_datasets(c(encodings, tmp_dataset))
          } else {
            tmp_dataset <- tmp_dataset$select(as.integer((1:chunk_list[[i]]) - 1))
            encodings <- datasets$concatenate_datasets(c(encodings, tmp_dataset))
          }
        }
        # print(encodings["input_ids"])
        return(encodings_list = list(
          encodings = encodings,
          chunks = chunk_list
        ))
      }
    },
    #--------------------------------------------------------------------------
    #' @description Method for decoding a sequence of integers into tokens
    #' @param int_seqence `list` containing the integer sequences which
    #' should be transformed to tokens or plain text.
    #' @param to_token `bool` If `FALSE` plain text is returned.
    #' If `TRUE` a sequence of tokens is returned. Argument only relevant
    #' if the model is based on a transformer.
    #'
    #' @return `list` of token sequences
    decode = function(int_seqence, to_token = FALSE) {
      # Check
      check_type(int_seqence, "list", FALSE)
      check_type(to_token, "bool", FALSE)

      # Start
      tmp_token_list <- NULL
      for (i in 1:length(int_seqence)) {
        tmp_seq_token_list <- NULL
        for (j in 1:length(int_seqence[[i]])) {
          tmp_vector <- int_seqence[[i]][[j]]
          mode(tmp_vector) <- "integer"
          if (to_token == FALSE) {
            tmp_seq_token_list[j] <- list(private$transformer_components$tokenizer$decode(
              token_ids = tmp_vector,
              skip_special_tokens = TRUE
            ))
          } else {
            tmp_seq_token_list[j] <- list(private$transformer_components$tokenizer$convert_ids_to_tokens(tmp_vector))
          }
        }
        tmp_token_list[i] <- list(tmp_seq_token_list)
      }
      return(tmp_token_list)
    },
    #---------------------------------------------------------------------------
    #' @description Method for receiving the special tokens of the model
    #' @return Returns a `matrix` containing the special tokens in the rows
    #' and their type, token, and id in the columns.
    get_special_tokens = function() {
      special_tokens <- c(
        "bos_token",
        "eos_token",
        "unk_token",
        "sep_token",
        "pad_token",
        "cls_token",
        "mask_token"
      )
      tokens_map <- matrix(
        nrow = length(special_tokens),
        ncol = 3,
        data = NA
      )
      colnames(tokens_map) <- c("type", "token", "id")

      for (i in 1:length(special_tokens)) {
        tokens_map[i, 1] <- special_tokens[i]
        tokens_map[i, 2] <- replace_null_with_na(private$transformer_components$tokenizer[special_tokens[i]])
        tokens_map[i, 3] <- replace_null_with_na(private$transformer_components$tokenizer[paste0(special_tokens[i], "_id")])
      }

      return(tokens_map)
    },
    # Embedding------------------------------------------------------------------
    #' @description Method for creating text embeddings from raw texts.
    #' This method should only be used if a small number of texts should be transformed
    #' into text embeddings. For a large number of texts please use the method `embed_large`.
    #' In the case of using a GPU and running out of memory while using 'tensorflow'  reduce the
    #' batch size or restart R and switch to use cpu only via `set_config_cpu_only`. In general,
    #' not relevant for 'pytorch'.       #'
    #' @param raw_text `vector` containing the raw texts.
    #' @param doc_id `vector` containing the corresponding IDs for every text.
    #' @param batch_size `int` determining the maximal size of every batch.
    #' @param trace `bool` `TRUE`, if information about the progression
    #' should be printed on console.
    #' @param return_large_dataset 'bool' If `TRUE` the retuned object is of class
    #' [LargeDataSetForTextEmbeddings]. If `FALSE` it is of class [EmbeddedText]
    #' @return Method returns an object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings]. This object
    #' contains the embeddings as a [data.frame] and information about the
    #' model creating the embeddings.
    embed = function(raw_text = NULL, doc_id = NULL, batch_size = 8, trace = FALSE, return_large_dataset = FALSE) {
      # check arguments
      check_type(raw_text, "vector", FALSE)
      check_type(doc_id, "vector", FALSE)
      check_type(batch_size, "int", FALSE)
      check_type(trace, "bool", FALSE)
      check_type(return_large_dataset, "bool", FALSE)

      # transformer---------------------------------------------------------------------
      n_units <- length(raw_text)
      n_layer <- private$transformer_components$model$config$num_hidden_layers
      n_layer_size <- private$transformer_components$model$config$hidden_size

      # Batch refers to the number of cases
      n_batches <- ceiling(n_units / batch_size)
      batch_results <- NULL

      if (private$transformer_components$emb_pool_type == "average") {
        if (private$transformer_components$ml_framework == "pytorch") {
          reticulate::py_run_file(system.file("python/pytorch_te_classifier.py",
            package = "aifeducation"
          ))
          pooling <- py$GlobalAveragePooling1D_PT()
        } else if (private$transformer_components$ml_framework == "tensorflow") {
          pooling <- keras$layers$GlobalAveragePooling1D()
        }
      }

      for (b in 1:n_batches) {
        if (private$transformer_components$ml_framework == "pytorch") {
          # Set model to evaluation mode
          private$transformer_components$model$eval()
          if (torch$cuda$is_available()) {
            pytorch_device <- "cuda"
          } else {
            pytorch_device <- "cpu"
          }
          private$transformer_components$model$to(pytorch_device)
          if (private$transformer_components$emb_pool_type == "average") {
            pooling$to(pytorch_device)
          }
        }

        index_min <- 1 + (b - 1) * batch_size
        index_max <- min(b * batch_size, n_units)
        batch <- index_min:index_max

        tokens <- self$encode(
          raw_text = raw_text[batch],
          trace = trace,
          token_encodings_only = FALSE
        )

        text_embedding <- array(
          data = 0,
          dim = c(
            length(batch),
            private$transformer_components$chunks,
            n_layer_size
          )
        )

        # Selecting the relevant layers
        selected_layer <- private$transformer_components$emb_layer_min:private$transformer_components$emb_layer_max
        tmp_selected_layer <- 1 + selected_layer

        if (private$transformer_components$ml_framework == "tensorflow") {
          # Clear session to ensure enough memory
          tf$keras$backend$clear_session()

          # Calculate tensors
          tokens$encodings$set_format(type = "tensorflow")

          tensor_embeddings <- private$transformer_components$model(
            input_ids = tokens$encodings["input_ids"],
            attention_mask = tokens$encodings["attention_mask"],
            token_type_ids = tokens$encodings["token_type_ids"],
            output_hidden_states = TRUE
          )$hidden_states
          if (private$transformer_components$emb_pool_type == "average") {
            # Average Pooling over all tokens
            for (i in tmp_selected_layer) {
              tensor_embeddings[i] <- list(pooling(
                inputs = tensor_embeddings[[as.integer(i)]],
                mask = tokens$encodings["attention_mask"]
              ))
            }
          }
        } else {
          # Clear memory
          if (torch$cuda$is_available()) {
            torch$cuda$empty_cache()
          }

          # Calculate tensors
          tokens$encodings$set_format(type = "torch")


          with(
            data = torch$no_grad(), {
              if (private$basic_components$method == AIFETrType$mpnet) {
                tensor_embeddings <- private$transformer_components$model(
                  input_ids = tokens$encodings["input_ids"]$to(pytorch_device),
                  attention_mask = tokens$encodings["attention_mask"]$to(pytorch_device),
                  output_hidden_states = TRUE
                )$hidden_states
              } else {
                tensor_embeddings <- private$transformer_components$model(
                  input_ids = tokens$encodings["input_ids"]$to(pytorch_device),
                  attention_mask = tokens$encodings["attention_mask"]$to(pytorch_device),
                  token_type_ids = tokens$encodings["token_type_ids"]$to(pytorch_device),
                  output_hidden_states = TRUE
                )$hidden_states
              }
            }
          )

          if (private$transformer_components$emb_pool_type == "average") {
            # Average Pooling over all tokens
            for (i in tmp_selected_layer) {
              tensor_embeddings[i] <- list(pooling(
                x = tensor_embeddings[[as.integer(i)]]$to(pytorch_device),
                mask = tokens$encodings["attention_mask"]$to(pytorch_device)
              ))
            }
          }
        }

        # Sorting the hidden states to the corresponding cases and times
        # If more than one layer is selected the mean is calculated
        index <- 0
        for (i in 1:length(batch)) {
          for (j in 1:tokens$chunks[i]) {
            for (layer in tmp_selected_layer) {
              if (private$transformer_components$ml_framework == "tensorflow") {
                if (private$transformer_components$emb_pool_type == "cls") {
                  # CLS Token is always the first token
                  text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                    tensor_embeddings[[as.integer(layer)]][[as.integer(index)]][[as.integer(0)]]$numpy()
                  )
                } else if (private$transformer_components$emb_pool_type == "average") {
                  text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                    tensor_embeddings[[as.integer(layer)]][[as.integer(index)]]$numpy()
                  )
                }
              } else {
                if (torch$cuda$is_available() == FALSE) {
                  if (private$transformer_components$emb_pool_type == "cls") {
                    # CLS Token is always the first token
                    text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                      tensor_embeddings[[as.integer(layer)]][[as.integer(index)]][[as.integer(0)]]$detach()$numpy()
                    )
                  } else if (private$transformer_components$emb_pool_type == "average") {
                    text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                      tensor_embeddings[[as.integer(layer)]][[as.integer(index)]]$detach()$numpy()
                    )
                  }
                } else {
                  if (private$transformer_components$emb_pool_type == "cls") {
                    # CLS Token is always the first token
                    text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                      tensor_embeddings[[as.integer(layer)]][[as.integer(index)]][[as.integer(0)]]$detach()$cpu()$numpy()
                    )
                  } else if (private$transformer_components$emb_pool_type == "average") {
                    text_embedding[i, j, ] <- text_embedding[i, j, ] + as.vector(
                      tensor_embeddings[[as.integer(layer)]][[as.integer(index)]]$detach()$cpu()$numpy()
                    )
                  }
                }
              }
            }
            text_embedding[i, j, ] <- text_embedding[i, j, ] / length(tmp_selected_layer)
            index <- index + 1
          }
        }
        dimnames(text_embedding)[[3]] <- paste0(
          private$basic_components$method, "_",
          seq(from = 1, to = n_layer_size, by = 1)
        )

        # Add ID of every case
        dimnames(text_embedding)[[1]] <- doc_id[batch]
        batch_results[b] <- list(text_embedding)
        if (trace == TRUE) {
          cat(paste(
            date(),
            "Batch", b, "/", n_batches, "Done", "\n"
          ))
        }
        base::gc(verbose = FALSE, full = TRUE)
      }

      # Summarizing the results over all batches
      text_embedding <- array_form_bind(batch_results)



      embeddings <- EmbeddedText$new()
      embeddings$configure(
        model_name = private$model_info$model_name,
        model_label = private$model_info$model_label,
        model_date = private$model_info$model_date,
        model_method = private$basic_components$method,
        model_language = private$model_info$model_language,
        param_seq_length = private$basic_components$max_length,
        param_features = dim(text_embedding)[3],
        param_chunks = private$transformer_components$chunks,
        param_overlap = private$transformer_components$overlap,
        param_emb_layer_min = private$transformer_components$emb_layer_min,
        param_emb_layer_max = private$transformer_components$emb_layer_max,
        param_emb_pool_type = private$transformer_components$emb_pool_type,
        param_aggregation = NA,
        embeddings = text_embedding
      )

      if (return_large_dataset == FALSE) {
        return(embeddings)
      } else {
        embedded_texts_large <- LargeDataSetForTextEmbeddings$new()
        embedded_texts_large$configure(
          model_name = private$model_info$model_name,
          model_label = private$model_info$model_label,
          model_date = private$model_info$model_date,
          model_method = private$basic_components$method,
          model_language = private$model_info$model_language,
          param_seq_length = private$basic_components$max_length,
          param_features = dim(embeddings$embeddings)[length(dim(embeddings$embeddings))],
          param_chunks = private$transformer_components$chunks,
          param_overlap = private$transformer_components$overlap,
          param_emb_layer_min = private$transformer_components$emb_layer_min,
          param_emb_layer_max = private$transformer_components$emb_layer_max,
          param_emb_pool_type = private$transformer_components$emb_pool_type,
          param_aggregation = NA
        )
        # Add new data
        embedded_texts_large$add_embeddings_from_EmbeddedText(embeddings)
        return(embedded_texts_large)
      }
    },
    #--------------------------------------------------------------------------
    #' @description Method for creating text embeddings from raw texts.
    #' @param large_datas_set Object of class [LargeDataSetForText] containing the
    #' raw texts.
    #' @param batch_size `int` determining the maximal size of every batch.
    #' @param trace `bool` `TRUE`, if information about the progression
    #' should be printed on console.
    #' @param log_file `string` Path to the file where the log should be saved.
    #' If no logging is desired set this argument to `NULL`.
    #' @param log_write_interval `int` Time in seconds determining the interval in which
    #' the logger should try to update the log files. Only relevant if `log_file` is not `NULL`.
    #' @return Method returns an object of class [LargeDataSetForTextEmbeddings].
    embed_large = function(large_datas_set, batch_size = 32, trace = FALSE,
                           log_file = NULL,
                           log_write_interval = 2) {
      # Check arguments
      check_class(large_datas_set, c("LargeDataSetForText", FALSE))
      check_type(batch_size, "int", FALSE)
      check_type(trace, "bool", FALSE)

      # Get total number of batches for the loop
      total_number_of_bachtes <- ceiling(large_datas_set$n_rows() / batch_size)

      # Get indices for every batch
      batches_index <- get_batches_index(
        number_rows = large_datas_set$n_rows(),
        batch_size = batch_size,
        zero_based = TRUE
      )
      # Set up log
      last_log <- NULL

      # Process every batch
      for (i in 1:total_number_of_bachtes) {
        subset <- large_datas_set$select(as.integer(batches_index[[i]]))
        embeddings <- self$embed(
          raw_text = c(subset["text"]),
          doc_id = c(subset["id"]),
          batch_size = batch_size,
          trace = FALSE
        )
        if (i == 1) {
          # Create Large Dataset
          embedded_texts_large <- LargeDataSetForTextEmbeddings$new()
          embedded_texts_large$configure(
            model_name = private$model_info$model_name,
            model_label = private$model_info$model_label,
            model_date = private$model_info$model_date,
            model_method = private$basic_components$method,
            model_language = private$model_info$model_language,
            param_seq_length = private$basic_components$max_length,
            param_features = dim(embeddings$embeddings)[3],
            param_chunks = private$transformer_components$chunks,
            param_overlap = private$transformer_components$overlap,
            param_emb_layer_min = private$transformer_components$emb_layer_min,
            param_emb_layer_max = private$transformer_components$emb_layer_max,
            param_emb_pool_type = private$transformer_components$emb_pool_type,
            param_aggregation = NA
          )
          # Add new data
          embedded_texts_large$add_embeddings_from_EmbeddedText(embeddings)
        } else {
          # Add new data
          embedded_texts_large$add_embeddings_from_EmbeddedText(embeddings)
        }
        if (trace == TRUE) {
          cat(paste(
            date(),
            "Batch", i, "/", total_number_of_bachtes, "done", "\n"
          ))
        }

        # Update log
        last_log <- write_log(
          log_file = log_file,
          last_log = last_log,
          write_interval = log_write_interval,
          value_top = i,
          value_middle = 0,
          value_bottom = 0,
          total_top = total_number_of_bachtes,
          total_middle = 1,
          total_bottom = 1,
          message_top = "Batches",
          message_middle = NA,
          message_bottom = NA
        )
        gc()
      }
      return(embedded_texts_large)
    },
    # Fill Mask------------------------------------------------------------------
    #' @description Method for calculating tokens behind mask tokens.
    #' @param text `string` Text containing mask tokens.
    #' @param n_solutions `int` Number estimated tokens for every mask.
    #' @return Returns a `list` containing a `data.frame` for every
    #' mask. The `data.frame` contains the solutions in the rows and reports
    #' the score, token id, and token string in the columns.
    fill_mask = function(text, n_solutions = 5) {
      # Arugment checking
      check_type(text, "string", FALSE)
      check_type(n_solutions, "int", FALSE)

      if (private$transformer_components$ml_framework == "pytorch") {
        framework <- "pt"
        private$transformer_components$model_mlm$to("cpu")
      } else {
        framework <- "tf"
      }

      return_token_type_ids <- (private$basic_components$method != AIFETrType$mpnet)

      if (private$basic_components$method != "mpnet") {
        run_py_file("FillMaskForMPLM.py")
        fill_mask_pipeline_class = py$FillMaskPipelineForMPLM
      } else {
        fill_mask_pipeline_class = transformers$FillMaskPipeline
      }

      fill_mask_pipeline <- fill_mask_pipeline_class(
        model = private$transformer_components$model_mlm,
        tokenizer = private$transformer_components$tokenizer,
        framework = framework,
        num_workers = 1,
        binary_output = FALSE,
        top_k = as.integer(n_solutions),
        tokenizer_kwargs = reticulate::dict(list(return_token_type_ids = return_token_type_ids))
      )

      special_tokens <- self$get_special_tokens()
      mask_token <- special_tokens[special_tokens[, "type"] == "mask_token", "token"]

      #n_mask_tokens <- ncol(stringr::str_extract_all(text,
      #  stringr::fixed(mask_token),
      #  simplify = TRUE
      #))
      n_mask_tokens <- ncol(stringi::stri_extract_all_fixed(str=text,
                                                     pattern=mask_token,
                                                     simplify = TRUE
      ))

      if (n_mask_tokens == 0) {
        stop("There is no masking token. Please check your input.")
      }

      solutions <- as.list(fill_mask_pipeline(text))

      solutions_list <- NULL

      if (n_mask_tokens == 1) {
        solution_data_frame <- matrix(
          nrow = length(solutions),
          ncol = 3
        )
        colnames(solution_data_frame) <- c(
          "score",
          "token",
          "token_str"
        )
        for (i in 1:length(solutions)) {
          solution_data_frame[i, "score"] <- solutions[[i]]$score
          solution_data_frame[i, "token"] <- solutions[[i]]$token
          solution_data_frame[i, "token_str"] <- solutions[[i]]$token_str
        }
        solution_data_frame <- as.data.frame(solution_data_frame)
        solution_data_frame$score <- as.numeric(solution_data_frame$score)
        solutions_list[length(solutions_list) + 1] <- list(solution_data_frame)
      } else {
        for (j in 1:length(solutions)) {
          solution_data_frame <- matrix(
            nrow = length(solutions[[j]]),
            ncol = 3
          )
          colnames(solution_data_frame) <- c(
            "score",
            "token",
            "token_str"
          )
          for (i in 1:length(solutions[[j]])) {
            solution_data_frame[i, "score"] <- solutions[[j]][[i]]$score
            solution_data_frame[i, "token"] <- solutions[[j]][[i]]$token
            solution_data_frame[i, "token_str"] <- solutions[[j]][[i]]$token_str
          }
          solution_data_frame <- as.data.frame(solution_data_frame)
          solution_data_frame$score <- as.numeric(solution_data_frame$score)
          solutions_list[length(solutions_list) + 1] <- list(solution_data_frame)
        }
      }

      return(solutions_list)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting the bibliographic information of the model.
    #' @param type `string` Type of information which should be changed/added.
    #' `developer`, and `modifier` are possible.
    #' @param authors List of people.
    #' @param citation `string` Citation in free text.
    #' @param url `string` Corresponding URL if applicable.
    #' @return Function does not return a value. It is used to set the private
    #' members for publication information of the model.
    set_publication_info = function(type,
                                    authors,
                                    citation,
                                    url = NULL) {
      if (type == "developer") {
        private$publication_info$developed_by$authors <- authors
        private$publication_info$developed_by$citation <- citation
        private$publication_info$developed_by$url <- url
      } else if (type == "modifier") {
        private$publication_info$modified_by$authors <- authors
        private$publication_info$modified_by$citation <- citation
        private$publication_info$modified_by$url <- url
      }
    },
    #--------------------------------------------------------------------------
    #' @description Method for getting the bibliographic information of the model.
    #' @return `list` of bibliographic information.
    get_publication_info = function() {
      return(private$publication_info)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting the license of the model
    #' @param license `string` containing the abbreviation of the license or
    #' the license text.
    #' @return Function does not return a value. It is used for setting the private
    #' member for the software license of the model.
    set_software_license = function(license = "CC BY") {
      private$model_info$model_license <- license
    },
    #' @description Method for requesting the license of the model
    #' @return `string` License of the model
    get_software_license = function() {
      return(private$model_info$model_license)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting the license of models' documentation.
    #' @param license `string` containing the abbreviation of the license or
    #' the license text.
    #' @return Function does not return a value. It is used to set the private member for the
    #' documentation license of the model.
    set_documentation_license = function(license = "CC BY") {
      private$model_description$license <- license
    },
    #' @description Method for getting the license of the models' documentation.
    #' @param license `string` containing the abbreviation of the license or
    #' the license text.
    get_documentation_license = function() {
      return(private$model_description$license)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting a description of the model
    #' @param eng `string` A text describing the training of the classifier,
    #' its theoretical and empirical background, and the different output labels
    #' in English.
    #' @param native `string` A text describing the training of the classifier,
    #' its theoretical and empirical background, and the different output labels
    #' in the native language of the model.
    #' @param abstract_eng `string` A text providing a summary of the description
    #' in English.
    #' @param abstract_native `string` A text providing a summary of the description
    #' in the native language of the classifier.
    #' @param keywords_eng `vector`of keywords in English.
    #' @param keywords_native `vector`of keywords in the native language of the classifier.
    #' @return Function does not return a value. It is used to set the private members for the
    #' description of the model.
    set_model_description = function(eng = NULL,
                                     native = NULL,
                                     abstract_eng = NULL,
                                     abstract_native = NULL,
                                     keywords_eng = NULL,
                                     keywords_native = NULL) {
      if (!is.null(eng)) {
        private$model_description$eng <- eng
      }
      if (!is.null(native)) {
        private$model_description$native <- native
      }

      if (!is.null(abstract_eng)) {
        private$model_description$abstract_eng <- abstract_eng
      }
      if (!is.null(abstract_native)) {
        private$model_description$abstract_native <- abstract_native
      }

      if (!is.null(keywords_eng)) {
        private$model_description$keywords_eng <- keywords_eng
      }
      if (!is.null(keywords_native)) {
        private$model_description$keywords_native <- keywords_native
      }
    },
    #' @description Method for requesting the model description.
    #' @return `list` with the description of the model in English
    #' and the native language.
    get_model_description = function() {
      return(private$model_description)
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting the model information
    #' @return `list` of all relevant model information
    get_model_info = function() {
      return(list(
        model_license = private$model_info$model_license,
        model_name_root = private$model_info$model_name_root,
        model_id = private$model_info$model_id,
        model_name = private$model_info$model_name,
        model_label = private$model_info$model_label,
        model_date = private$model_info$model_date,
        model_language = private$model_info$model_language
      ))
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting a summary of the R and python packages'
    #' versions used for creating the model.
    #' @return Returns a `list` containing the versions of the relevant
    #' R and python packages.
    get_package_versions = function() {
      return(
        list(
          r_package_versions = private$private$r_package_versions,
          py_package_versions = private$py_package_versions
        )
      )
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting the part of interface's configuration that is
    #' necessary for all models.
    #' @return Returns a `list`.
    get_basic_components = function() {
      return(
        private$basic_components
      )
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting the part of interface's configuration that is
    #' necessary for transformer models.
    #' @return Returns a `list`.
    get_transformer_components = function() {
      return(
        list(
          chunks = private$transformer_components$chunks,
          overlap = private$transformer_components$overlap,
          ml_framework = private$transformer_components$ml_framework,
          emb_layer_min = private$transformer_components$emb_layer_min,
          emb_layer_max = private$transformer_components$emb_layer_max,
          emb_pool_type = private$transformer_components$emb_pool_type,
          ml_framework = private$transformer_components$ml_framework
        )
      )
    },
    #' @description Method for requesting a log of tracked energy consumption
    #' during training and an estimate of the resulting CO2 equivalents in kg.
    #' @return Returns a `matrix` containing the tracked energy consumption,
    #' CO2 equivalents in kg, information on the tracker used, and technical
    #' information on the training infrastructure for every training run.
    get_sustainability_data = function() {
      return(private$sustainability$track_log)
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting the machine learning framework used
    #' for the classifier.
    #' @return Returns a `string` describing the machine learning framework used
    #' for the classifier.
    get_ml_framework = function() {
      return(private$transformer_components$ml_framework)
    },
    #---------------------------------------------------------------------------
    #' @description Method for counting the trainable parameters of a model.
    #' @param with_head `bool` If `TRUE` the number of parameters is returned including
    #' the language modeling head of the model. If `FALSE` only the number of parameters of
    #' the core model is returned.
    #' @return Returns the number of trainable parameters of the model.
    count_parameter = function(with_head = FALSE) {
      if (with_head == FALSE) {
        model <- private$transformer_components$model
      } else {
        model <- private$transformer_components$model_mlm
      }

      if (private$transformer_components$ml_framework == "tensorflow") {
        count <- 0
        for (i in 1:length(model$trainable_weights)) {
          count <- count + tf$keras$backend$count_params(model$trainable_weights[[i]])
        }
      } else if (private$transformer_components$ml_framework == "pytorch") {
        iterator <- reticulate::as_iterator(model$parameters())
        iteration_finished <- FALSE
        count <- 0
        while (iteration_finished == FALSE) {
          iter_results <- reticulate::iter_next(it = iterator)
          if (is.null(iter_results)) {
            iteration_finished <- TRUE
          } else {
            if (iter_results$requires_grad == TRUE) {
              count <- count + iter_results$numel()
            }
          }
        }
      }
      return(count)
    },
    #-------------------------------------------------------------------------
    #' @description Method for checking if the model was successfully configured.
    #' An object can only be used if this value is `TRUE`.
    #' @return `bool` `TRUE` if the model is fully configured. `FALSE` if not.
    is_configured = function() {
      return(private$configured)
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting all private fields and methods. Used
    #' for loading and updating an object.
    #' @return Returns a `list` with all private fields and methods.
    get_private = function() {
      return(private)
    },
    #--------------------------------------------------------------------------
    #' @description Return all fields.
    #' @return Method returns a `list` containing all public and private fields
    #' of the object.
    get_all_fields = function() {
      public_list <- NULL
      private_list <- NULL

      for (entry in names(self)) {
        if (is.function(self[[entry]]) == FALSE & is.environment(self[[entry]]) == FALSE) {
          public_list[entry] <- list(self[[entry]])
        }
      }

      for (entry in names(private)) {
        if (is.function(private[[entry]]) == FALSE & is.environment(private[[entry]]) == FALSE) {
          private_list[entry] <- list(private[[entry]])
        }
      }

      return(
        list(
          public = public_list,
          private = private_list
        )
      )
    }
  )
)
