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
#' Objects of this class containing fields and methods used in several other classes in 'AI for Education'.
#'
#' This class is **not** designed for a direct application and should only be used by developers.
#'
#' @return A new object of this class.
#' @family R6 Classes for Developers
#' @export
AIFEBaseModel <- R6::R6Class(
  classname = "AIFEBaseModel",
  public = list(
    #' @field model ('pytorch_model')\cr
    #'   Field for storing a 'pytorch' model after loading.
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

      save_format <- "safetensors"

      if (save_format == "safetensors" & reticulate::py_module_available("safetensors") == FALSE) {
        warning("Python library 'safetensors' is not available. Using
                 standard save format for pytorch.")
        save_format <- "pt"
      }

      create_dir(save_location, FALSE)
      self$model$to("cpu", dtype = torch$float)
      if (save_format == "safetensors") {
        file_path <- paste0(save_location, "/", "model_data", ".safetensors")
        safetensors$torch$save_model(model = self$model, filename = file_path)
      } else if (save_format == "pt") {
        file_path <- paste0(save_location, "/", "model_data", ".pt")
        torch$save(self$model$state_dict(), file_path)
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
    #---------------------------------------------------------------------------
    #' @description Method for counting the trainable parameters of a model.
    #' @return Returns the number of trainable parameters of the model.
    count_parameter = function() {
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
    #' @description Check if the [TEFeatureExtractor] is trained.
    #' @return Returns `TRUE` if the object is trained and `FALSE` if not.
    is_trained = function() {
      return(private$trained)
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
    sustainability_tracker = NA,
    dir_checkpoint = NULL,
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
    log_config = list(
      log_dir = NULL,
      log_state_file = NULL,
      log_write_intervall = 10
    ),

    # Variable for checking if the object is successfully configured. Only if
    # this is TRUE the object can be used
    configured = FALSE,

    # Variable for checking if the object has been successfully trained.
    trained = FALSE,

    #--------------------------------------------------------------------------
    # Method for setting the model info
    set_model_info = function(model_name, label, model_date) {
      private$model_info$model_name <- model_name
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
    set_package_versions = function() {
      private$r_package_versions$aifeducation <- packageVersion("aifeducation")
      private$r_package_versions$reticulate <- packageVersion("reticulate")

      private$py_package_versions$torch <- torch["__version__"]
      private$py_package_versions$numpy <- np$version$short_version
    },
    #-------------------------------------------------------------------------
    load_base_config_and_docs_general = function(config_public, config_private) {
      # Set ML framework
      private$ml_framework <- config_private$ml_framework

      # Set configuration of the core model
      self$model_config <- config_public$model_config

      # Set model info
      private$set_model_info(
        model_name = config_private$model_info$model_name,
        label = config_private$model_info$model_label,
        model_date = config_private$model_info$model_date
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
      private$py_package_versions$numpy <- config_private$py_package_versions$numpy
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
    },
    #--------------------------------------------------------------------------
    init_and_start_sustainability_tracking = function() {
      if (self$last_training$config$sustain_track == TRUE) {
        if (check_versions(a = get_py_package_version("codecarbon"), operator = ">=", b = "2.8.0")) {
          path_look_file <- codecarbon$lock$LOCKFILE
          if (file.exists(path_look_file)) {
            unlink(path_look_file)
          }
        }

        private$sustainability_tracker <- codecarbon$OfflineEmissionsTracker(
          country_iso_code = self$last_training$config$sustain_iso_code,
          region = self$last_training$config$sustain_region,
          tracking_mode = "machine",
          log_level = "warning",
          measure_power_secs = self$last_training$config$sustain_interval,
          save_to_file = FALSE,
          save_to_api = FALSE,
          allow_multiple_runs = FALSE
        )
        private$sustainability_tracker$start()
      }
    },
    #---------------------------------------------------------------------------
    stop_sustainability_tracking = function() {
      if (self$last_training$config$sustain_track == TRUE) {
        private$sustainability_tracker$stop()
        private$sustainability <- summarize_tracked_sustainability(private$sustainability_tracker)
      } else {
        private$sustainability <- list(
          sustainability_tracked = FALSE,
          date = NA,
          sustainability_data = list(
            duration_sec = NA,
            co2eq_kg = NA,
            cpu_energy_kwh = NA,
            gpu_energy_kwh = NA,
            ram_energy_kwh = NA,
            total_energy_kwh = NA
          )
        )
      }
    },
    #---------------------------------------------------------------------------
    create_checkpoint_directory = function() {
      # Create a directory for the package
      tmp_dir <- create_and_get_tmp_dir()

      # Create a folder for the current task
      private$dir_checkpoint <- paste0(
        tmp_dir, "/",
        generate_id(16)
      )
      create_dir(dir = private$dir_checkpoint, trace = FALSE)
    },
    #--------------------------------------------------------------------------
    clean_checkpoint_directory = function() {
      unlink(
        x = private$dir_checkpoint,
        recursive = TRUE,
        force = FALSE
      )
    }
  )
)
