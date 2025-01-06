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



#' @title Base class for models using neural nets
#' @description Abstract class for all models that do not rely on the python library 'transformers'.
#'
#' @return Objects of this containing fields and methods used in several other classes in 'ai for education'. This class
#'   is **not** designed for a direct application and should only be used by developers.
#' @family Classifiers for developers
#' @export
AIFEBaseModel <- R6::R6Class(
  classname = "AIFEBaseModel",
  public = list(
    #' @field model ('tensorflow_model' or 'pytorch_model')\cr
    #'   Field for storing the 'tensorflow' or 'pytorch' model after loading.
    model = NULL,

    #' @field model_config ('list()')\cr
    #'   List for storing information about the configuration of the model.
    model_config = list(),

    #' @field last_training ('list()')\cr
    #'   List for storing the history, the configuration, and the results of the last
    #'   training. This information will be overwritten if a new training is started.
    #'
    #' * `last_training$start_time`: Time point when training started.
    #' * `last_training$learning_time`: Duration of the training process.
    #' * `last_training$finish_time`: Time when the last training finished.
    #' * `last_training$history`: History of the last training.
    #' * `last_training$data`: Object of class `table` storing the initial frequencies of the passed data.
    #' * `last_training$config`: List storing the configuration used for the last training.
    #'
    last_training = list(
      start_time = NA,
      learning_time = NULL,
      finish_time = NULL,
      history = list(),
      data = NULL,
      config = list()
    ),
    # General Information set and get--------------------------------------------
    #' @description Method for requesting the model information.
    #' @return `list` of all relevant model information.
    get_model_info = function() {
      return(list(
        model_license = private$model_info$model_license,
        model_name = private$model_info$model_name,
        model_id = private$model_info$model_id,
        model_name_root = private$model_info$model_name_root,
        model_label = private$model_info$model_label,
        model_date = private$model_info$model_date
      ))
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting the text embedding model information.
    #' @return `list` of all relevant model information on the text embedding model underlying the model.
    get_text_embedding_model = function() {
      return(private$text_embedding_model)
    },
    #---------------------------------------------------------------------------
    #' @description Method for setting publication information of the model.
    #' @param authors List of authors.
    #' @param citation Free text citation.
    #' @param url URL of a corresponding homepage.
    #' @return Function does not return a value. It is used for setting the private members for publication information.
    set_publication_info = function(authors,
                                    citation,
                                    url = NULL) {
      private$publication_info$developed_by$authors <- authors
      private$publication_info$developed_by$citation <- citation
      private$publication_info$developed_by$url <- url
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting the bibliographic information of the model.
    #' @return `list` with all saved bibliographic information.
    get_publication_info = function() {
      return(private$publication_info)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting the license of the model.
    #' @param license `string` containing the abbreviation of the license or the license text.
    #' @return Function does not return a value. It is used for setting the private member for the software license of
    #'   the model.
    set_model_license = function(license = "CC BY") {
      private$model_info$model_license <- license
    },
    #' @description Method for getting the license of the model.
    #' @param license `string` containing the abbreviation of the license or the license text.
    #' @return `string` representing the license for the model.
    get_model_license = function() {
      return(private$model_info$model_license)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting the license of the model's documentation.
    #' @param license `string` containing the abbreviation of the license or the license text.
    #' @return Function does not return a value. It is used for setting the private member for the documentation license
    #'   of the model.
    set_documentation_license = function(license = "CC BY") {
      private$model_description$license <- license
    },
    #' @description Method for getting the license of the model's documentation.
    #' @param license `string` containing the abbreviation of the license or the license text.
    #' @return Returns the license as a `string`.
    get_documentation_license = function() {
      return(private$model_description$license)
    },
    #--------------------------------------------------------------------------
    #' @description Method for setting a description of the model.
    #' @param eng `string` A text describing the training, its theoretical and empirical background, and output in
    #'   English.
    #' @param native `string` A text describing the training , its theoretical and empirical background, and output in
    #'   the native language of the model.
    #' @param abstract_eng `string` A text providing a summary of the description in English.
    #' @param abstract_native `string` A text providing a summary of the description in the native language of the
    #'   model.
    #' @param keywords_eng `vector` of keyword in English.
    #' @param keywords_native `vector` of keyword in the native language of the model.
    #' @return Function does not return a value. It is used for setting the private members for the description of the
    #'   model.
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
    #' @return `list` with the description of the classifier in English and the native language.
    get_model_description = function() {
      return(private$model_description)
    },
    #-------------------------------------------------------------------------
    #' @description Method for saving a model.
    #' @param dir_path `string` Path of the directory where the model should be saved.
    #' @param folder_name `string` Name of the folder that should be created within the directory.
    #' @return Function does not return a value. It saves the model to disk.
    #' @importFrom utils write.csv
    save = function(dir_path, folder_name) {
      save_location <- paste0(dir_path, "/", folder_name)

      if (private$ml_framework == "tensorflow") {
        save_format <- "keras"
      } else if (private$ml_framework == "pytorch") {
        save_format <- "safetensors"
      }


      if (save_format == "safetensors" & reticulate::py_module_available("safetensors") == FALSE) {
        warning("Python library 'safetensors' is not available. Using
                 standard save format for pytorch.")
        save_format <- "pt"
      }

      if (private$ml_framework == "tensorflow") {
        if (save_format == "keras") {
          extension <- ".keras"
        } else if (save_format == "tf") {
          extension <- ".tf"
        } else {
          extension <- ".h5"
        }
        file_path <- paste0(save_location, "/", "model_data", extension)
        create_dir(save_location, FALSE)
        self$model$save(file_path)
      } else if (private$ml_framework == "pytorch") {
        create_dir(save_location, FALSE)
        self$model$to("cpu", dtype = torch$float)
        if (save_format == "safetensors") {
          file_path <- paste0(save_location, "/", "model_data", ".safetensors")
          safetensors$torch$save_model(model = self$model, filename = file_path)
        } else if (save_format == "pt") {
          file_path <- paste0(save_location, "/", "model_data", ".pt")
          torch$save(self$model$state_dict(), file_path)
        }
      }

      # Saving Sustainability Data
      sustain_matrix <- t(as.matrix(unlist(private$sustainability)))
      write.csv(
        x = sustain_matrix,
        file = paste0(save_location, "/", "sustainability.csv"),
        row.names = FALSE
      )
    },
    #--------------------------------------------------------------------------
    #' @description Method for importing a model.
    #' @param dir_path `string` Path of the directory where the model is saved.
    #' @return Function does not return a value. It is used to load the weights of a model.
    load = function(dir_path) {
      # Load python scripts
      private$load_reload_python_scripts()

      # Load the model---------------------------------------------------------
      if (private$ml_framework == "tensorflow") {
        path <- paste0(dir_path, "/", "model_data", ".keras")
        if (file.exists(paths = path) == TRUE) {
          self$model <- keras$models$load_model(path)
        } else {
          path <- paste0(dir_path, "/", "model_data", ".tf")
          if (dir.exists(paths = path) == TRUE) {
            self$model <- keras$models$load_model(path)
          } else {
            path <- paste0(dir_path, "/", "model_data", ".h5")
            if (file.exists(paths = path) == TRUE) {
              self$model <- keras$models$load_model(paste0(dir_path, "/", "model_data", ".h5"))
            } else {
              stop("There is no compatible model file in the choosen directory.
                   Please check path. Please note that classifiers have to be loaded with
                   the same framework as during creation.")
            }
          }
        }
      } else if (private$ml_framework == "pytorch") {
        path_pt <- paste0(dir_path, "/", "model_data", ".pt")
        path_safe_tensors <- paste0(dir_path, "/", "model_data", ".safetensors")
        private$create_reset_model()
        if (file.exists(path_safe_tensors)) {
          safetensors$torch$load_model(model = self$model, filename = path_safe_tensors)
        } else {
          if (file.exists(paths = path_pt) == TRUE) {
            self$model$load_state_dict(torch$load(path_pt))
          } else {
            stop("There is no compatible model file in the choosen directory.
                     Please check path. Please note that classifiers have to be loaded with
                     the same framework as during creation.")
          }
        }
      }

      # Load sustainability_data
      sustain_path <- paste0(dir_path, "/sustainability.csv")
      if (file.exists(sustain_path)) {
        sustain_data <- read.csv(sustain_path)

        private$sustainability <- list(
          sustainability_tracked = sustain_data$sustainability_tracked,
          date = sustain_data$date,
          sustainability_data = list(
            duration_sec = sustain_data$sustainability_data.duration_sec,
            co2eq_kg = sustain_data$sustainability_data.co2eq_kg,
            cpu_energy_kwh = sustain_data$sustainability_data.cpu_energy_kwh,
            gpu_energy_kwh = sustain_data$sustainability_data.gpu_energy_kwh,
            ram_energy_kwh = sustain_data$sustainability_data.ram_energy_kwh,
            total_energy_kwh = sustain_data$sustainability_data.total_energy_kwh
          )
        )
      }
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting a summary of the R and python packages' versions used for creating the model.
    #' @return Returns a `list` containing the versions of the relevant R and python packages.
    get_package_versions = function() {
      return(
        list(
          r_package_versions = private$r_package_versions,
          py_package_versions = private$py_package_versions
        )
      )
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting a summary of tracked energy consumption during training and an estimate of the
    #'   resulting CO2 equivalents in kg.
    #' @return Returns a `list` containing the tracked energy consumption, CO2 equivalents in kg, information on the
    #'   tracker used, and technical information on the training infrastructure.
    get_sustainability_data = function() {
      return(private$sustainability)
    },
    #---------------------------------------------------------------------------
    #' @description Method for requesting the machine learning framework used for the model.
    #' @return Returns a `string` describing the machine learning framework used for the classifier.
    get_ml_framework = function() {
      return(private$ml_framework)
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting the name (unique id) of the underlying text embedding model.
    #' @return Returns a `string` describing name of the text embedding model.
    get_text_embedding_model_name = function() {
      return(private$text_embedding_model$model$model_name)
    },
    #--------------------------------------------------------------------------
    # Check Embedding Model compatibility of the text embedding
    #' @description Method for checking if the provided text embeddings are created with the same [TextEmbeddingModel]
    #'   as the model.
    #' @param text_embeddings Object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings].
    #' @return `TRUE` if the underlying [TextEmbeddingModel] are the same. `FALSE` if the models differ.
    check_embedding_model = function(text_embeddings) {
      # Check object type
      private$check_embeddings_object_type(text_embeddings, strict = TRUE)

      # Check original text embedding model
      embedding_model_config <- text_embeddings$get_model_info()
      check <- c("model_name")

      if (
        !is.null_or_na(embedding_model_config[[check]]) &
          !is.null_or_na(private$text_embedding_model$model[[check]])
      ) {
        if (embedding_model_config[[check]] != private$text_embedding_model$model[[check]]) {
          stop("The TextEmbeddingModel that generated the data_embeddings is not
               the same as the TextEmbeddingModel when generating the classifier.")
        }
      }
    },
    #---------------------------------------------------------------------------
    #' @description Method for counting the trainable parameters of a model.
    #' @return Returns the number of trainable parameters of the model.
    count_parameter = function() {
      if (private$ml_framework == "tensorflow") {
        count <- 0
        for (i in seq_len(length(self$model$trainable_weights))) {
          count <- count + tf$keras$backend$count_params(self$model$trainable_weights[[i]])
        }
      } else if (private$ml_framework == "pytorch") {
        iterator <- reticulate::as_iterator(self$model$parameters())
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
    #' @description Method for checking if the model was successfully configured. An object can only be used if this
    #'   value is `TRUE`.
    #' @return `bool` `TRUE` if the model is fully configured. `FALSE` if not.
    is_configured = function() {
      return(private$configured)
    },
    #--------------------------------------------------------------------------
    #' @description Method for requesting all private fields and methods. Used for loading and updating an object.
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
  ),
  private = list(
    ml_framework = NA,

    # General Information-------------------------------------------------------
    model_info = list(
      model_license = NA,
      model_name = NA,
      model_name_root = NA,
      model_id = NA,
      name_root = NA,
      model_label = NA,
      model_date = NA
    ),
    text_embedding_model = list(
      model = list(),
      times = NA,
      features = NA
    ),
    publication_info = list(
      developed_by = list(
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
    r_package_versions = list(
      aifeducation = NA,
      reticulate = NA
    ),
    py_package_versions = list(
      tensorflow = NA,
      torch = NA,
      keras = NA,
      numpy = NA
    ),
    sustainability = list(
      sustainability_tracked = FALSE,
      date = NA,
      sustainability_data = list(
        duration_sec = NA,
        co2eq_kg = NA,
        cpu_energy_kwh = NA,
        gpu_energy_kwh = NA,
        ram_energy_kwh = NA,
        total_energy_kwh = NA
      ),
      technical = list(
        tracker = NA,
        py_package_version = NA,
        cpu_count = NA,
        cpu_model = NA,
        gpu_count = NA,
        gpu_model = NA,
        ram_total_size = NA
      ),
      region = list(
        country_name = NA,
        country_iso_code = NA,
        region = NA
      )
    ),
    gui = list(
      shiny_app_active = NA,
      pgr_value = 0,
      pgr_max_value = 0
    ),
    log_config = list(
      log_dir = NULL,
      log_state_file = NULL,
      log_write_intervall = 10
    ),

    # Variable for checking if the object is successfully configured. Only is
    # this is TRUE the object can be used
    configured = FALSE,

    #--------------------------------------------------------------------------
    # Method for setting the model info
    set_model_info = function(model_name_root, model_id, label, model_date) {
      private$model_info$model_name_root <- model_name_root
      private$model_info$model_id <- model_id
      private$model_info$model_name <- paste0(model_name_root, "_ID_", model_id)
      private$model_info$model_label <- label
      private$model_info$model_date <- model_date
    },
    #--------------------------------------------------------------------------
    # Method for summarizing sustainability data for this classifier
    # List for results must correspond to the private fields of the classifier
    summarize_tracked_sustainability = function(sustainability_tracker) {
      results <- list(
        sustainability_tracked = TRUE,
        sustainability_data = list(
          co2eq_kg = sustainability_tracker$final_emissions_data$emissions,
          cpu_energy_kwh = sustainability_tracker$final_emissions_data$cpu_energy,
          gpu_energy_kwh = sustainability_tracker$final_emissions_data$gpu_energy,
          ram_energy_kwh = sustainability_tracker$final_emissions_data$ram_energy,
          total_energy_kwh = sustainability_tracker$final_emissions_data$energy_consumed
        ),
        technical = list(
          tracker = "codecarbon",
          py_package_version = codecarbon$"__version__",
          cpu_count = sustainability_tracker$final_emissions_data$cpu_count,
          cpu_model = sustainability_tracker$final_emissions_data$cpu_model,
          gpu_count = sustainability_tracker$final_emissions_data$gpu_count,
          gpu_model = sustainability_tracker$final_emissions_data$gpu_model,
          ram_total_size = sustainability_tracker$final_emissions_data$ram_total_size
        ),
        region = list(
          country_name = sustainability_tracker$final_emissions_data$country_name,
          country_iso_code = sustainability_tracker$final_emissions_data$country_iso_code,
          region = sustainability_tracker$final_emissions_data$region
        )
      )
      return(results)
    },
    check_embeddings_object_type = function(embeddings, strict = TRUE) {
      if (strict == TRUE) {
        if (
          !("EmbeddedText" %in% class(embeddings)) &
            !("LargeDataSetForTextEmbeddings" %in% class(embeddings))
        ) {
          stop("text_embeddings must be of class EmbeddedText or LargeDataSetForTextEmbeddings.")
        }
      } else {
        if (
          !("EmbeddedText" %in% class(embeddings)) &
            !("LargeDataSetForTextEmbeddings" %in% class(embeddings)) &
            !("array" %in% class(embeddings)) &
            !("datasets.arrow_dataset.Dataset" %in% class(embeddings))
        ) {
          stop("text_embeddings must be of class EmbeddedText, LargeDataSetForTextEmbeddings,
               datasets.arrow_dataset.Dataset or array.")
        }
      }
    },
    #------------------------------------------------------------------------
    detach_tensors = function(tensors) {
      if (torch$cuda$is_available()) {
        return(tensors$detach()$cpu()$numpy())
      } else {
        return(tensors$detach()$numpy())
      }
    },
    #-------------------------------------------------------------------------
    check_single_prediction = function(embeddings) {
      if (
        "EmbeddedText" %in% class(embeddings) |
          "LargeDataSetForTextEmbeddings" %in% class(embeddings)
      ) {
        if (embeddings$n_rows() > 1) {
          single_prediction <- FALSE
        } else {
          single_prediction <- TRUE
        }
      } else if ("array" %in% class(embeddings)) {
        if (nrow(embeddings) > 1) {
          single_prediction <- FALSE
        } else {
          single_prediction <- TRUE
        }
      } else if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        single_prediction <- FALSE
      }
      return(single_prediction)
    },
    #--------------------------------------------------------------------------
    prepare_embeddings_as_dataset = function(embeddings) {
      if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        prepared_dataset <- embeddings
      } else if ("EmbeddedText" %in% class(embeddings)) {
        prepared_dataset <- datasets$Dataset$from_dict(
          reticulate::dict(
            list(
              id = rownames(embeddings$embeddings),
              input = np$squeeze(
                np$split(
                  reticulate::np_array(embeddings$embeddings),
                  as.integer(nrow(embeddings$embeddings)),
                  axis = 0L
                )
              )
            ),
            convert = FALSE
          )
        )
      } else if ("array" %in% class(embeddings)) {
        prepared_dataset <- datasets$Dataset$from_dict(
          reticulate::dict(
            list(
              id = rownames(embeddings),
              input = np$squeeze(np$split(reticulate::np_array(embeddings), as.integer(nrow(embeddings)), axis = 0L))
            ),
            convert = FALSE
          )
        )
      } else if ("LargeDataSetForTextEmbeddings" %in% class(embeddings)) {
        prepared_dataset <- embeddings$get_dataset()
      }
      return(prepared_dataset)
    },
    #-------------------------------------------------------------------------
    prepare_embeddings_as_np_array = function(embeddings) {
      if ("EmbeddedText" %in% class(embeddings)) {
        prepared_dataset <- embeddings$embeddings
        tmp_np_array <- np$array(prepared_dataset)
      } else if ("array" %in% class(embeddings)) {
        prepared_dataset <- embeddings
        tmp_np_array <- np$array(prepared_dataset)
      } else if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        prepared_dataset <- embeddings$set_format("np")
        tmp_np_array <- prepared_dataset["input"]
      } else if ("LargeDataSetForTextEmbeddings" %in% class(embeddings)) {
        prepared_dataset <- embeddings$get_dataset()
        prepared_dataset$set_format("np")
        tmp_np_array <- prepared_dataset["input"]
      }
      tmp_np_array <- reticulate::np_array(tmp_np_array)
      if (numpy_writeable(tmp_np_array) == FALSE) {
        warning("Numpy array is not writable")
      }
      return(tmp_np_array)
    },
    #--------------------------------------------------------------------------
    get_rownames_from_embeddings = function(embeddings) {
      if ("EmbeddedText" %in% class(embeddings)) {
        return(rownames(embeddings$embeddings))
      } else if ("array" %in% class(embeddings)) {
        return(rownames(embeddings))
      } else if ("datasets.arrow_dataset.Dataset" %in% class(embeddings)) {
        return(embeddings["id"])
      } else if ("LargeDataSetForTextEmbeddings" %in% class(embeddings)) {
        embeddings$get_ids()
      }
    },
    #-----------------------------------------------------------------------
    load_reload_python_scripts = function() {
      return(NULL)
    },
    #-------------------------------------------------------------------------
    # Method for setting configured to TRUE
    set_configuration_to_TRUE = function() {
      private$configured <- TRUE
    },
    #-------------------------------------------------------------------------
    # Method for checking if the configuration is done successfully
    check_config_for_TRUE = function() {
      if (private$configured == FALSE) {
        stop("The object is not configured. Please call the method configure.")
      }
    },
    # Method for checking if the configuration is already done
    check_config_for_FALSE = function() {
      if (private$configured == TRUE) {
        stop("The object is configured. Please create a new object if you would like to change the object's
             configuration.")
      }
    },
    #--------------------------------------------------------------------------
    set_text_embedding_model = function(model_info,
                                        feature_extractor_info,
                                        times,
                                        features) {
      private$text_embedding_model["model"] <- list(model_info)
      private$text_embedding_model["feature_extractor"] <- feature_extractor_info
      private$text_embedding_model["times"] <- times
      private$text_embedding_model["features"] <- features
    },
    #--------------------------------------------------------------------------
    set_package_versions = function() {
      private$r_package_versions$aifeducation <- packageVersion("aifeducation")
      private$r_package_versions$reticulate <- packageVersion("reticulate")

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
    },

    #--------------------------------------------------------------------------
    # description Loads configuration and documentation of an object from disk.
    # param dir_path Path where the object set is stored.
    # return Method does not return anything. It loads an object from disk.
    load_config_and_docs = function(dir_path) {
      if (self$is_configured() == TRUE) {
        stop("The object has already been configured. Please use the method
             'load' for loading the weights of a model.")
      }

      # Load R file
      config_file <- load_R_config_state(dir_path)

      # Old public state
      config_public <- config_file$public

      # Old private states
      config_private <- config_file$private

      # Set ML framework
      private$ml_framework <- config_private$ml_framework

      # Set configuration of the core model
      self$model_config <- config_public$model_config

      # Set model info
      private$set_model_info(
        model_name_root = config_private$model_info$model_name_root,
        model_id = config_private$model_info$model_id,
        label = config_private$model_info$model_label,
        model_date = config_private$model_info$model_date
      )

      # Set TextEmbeddingModel
      private$set_text_embedding_model(
        model_info = config_private$text_embedding_model$model,
        feature_extractor_info = config_private$text_embedding_model$feature_extractor,
        times = config_private$text_embedding_model$times,
        features = config_private$text_embedding_model$features
      )

      # Set last training
      self$last_training$config <- config_public$last_training$config
      self$last_training$start_time <- config_public$last_training$start_time
      self$last_training$learning_time <- config_public$last_training$learning_time
      self$last_training$finish_time <- config_public$last_training$finish_time
      self$last_training$history <- config_public$last_training$history
      self$last_training$data <- config_public$last_training$data

      # Set license
      self$set_model_license(config_private$model_info$model_license)
      self$set_documentation_license(config_private$model_description$license)

      # Set description and documentation
      self$set_model_description(
        eng = config_private$model_description$eng,
        native = config_private$model_description$native,
        abstract_eng = config_private$model_description$abstract_eng,
        abstract_native = config_private$model_description$abstract_native,
        keywords_eng = config_private$model_description$keywords_eng,
        keywords_native = config_private$model_description$keywords_native
      )

      # Set publication info
      self$set_publication_info(
        authors = config_private$publication_info$developed_by$authors,
        citation = config_private$publication_info$developed_by$citation,
        url = config_private$publication_info$developed_by$url
      )

      # Get and set original package versions
      private$r_package_versions$aifeducation <- config_private$r_package_versions$aifeducation
      private$r_package_versions$reticulate <- config_private$r_package_versions$reticulate

      private$py_package_versions$torch <- config_private$py_package_versions$torch
      private$py_package_versions$tensorflow <- config_private$py_package_versions$tensorflow
      private$py_package_versions$keras <- config_private$py_package_versions$keras
      private$py_package_versions$numpy <- config_private$py_package_versions$numpy

      # Finalize config
      private$set_configuration_to_TRUE()
    },

    #-------------------------------------------------------------------------
    prepare_history_data = function(history) {
      # Provide rownames for the history
      for (i in seq_len(length(history))) {
        if (!is.null(history[[i]])) {
          if (nrow(history[[i]]) == 2) {
            rownames(history[[i]]) <- c("train", "val")
          } else {
            rownames(history[[i]]) <- c("train", "val", "test")
          }

          # Replace value -100 with the last value
          # Max index for replacements
          index_max <- ncol(history[[i]])
          for (j in seq_len(nrow(history[[i]]))) {
            # Check if -100 occurs in the row
            includes_m_100 <- (history[[i]][j, ] == -100)
            # if at least one -100 occurs
            if (sum(includes_m_100) > 0) {
              # min index for replacements
              index_min <- min(which(includes_m_100))
              # replace
              history[[i]][j, index_min:index_max] <- history[[i]][j, (index_min - 1)]
            }
          }
        }
      }
      return(history)
    }
  )
)
