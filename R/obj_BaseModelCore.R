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

#' @title
#' @description
#' @return
#' @family Base Model
#' @export
BaseModelCore <- R6::R6Class(
  classname = "BaseModelCore",
  inherit = AIFEBaseModel,
  private = list(
    model_type = NULL,
    adjust_max_sequence_length=0,

    model_info = list(),
    sustainability_tracker = NULL,
    sustainability_inference = list(
      sustainability_tracked = FALSE,
      track_log = NA
    ),
    flops_estimates = NULL,

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

    #-------------------------------------------------------------------------
    load_reload_python_scripts = function() {
      load_py_scripts(
        c(
          "py_log.py",
          "datasets_transformer_compute_vocabulary.py",
          "datasets_transformer_prepare_data.py",
          "pytorch_transformer_callbacks.py"
        )
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
    #--------------------------------------------------------------------------
    save_tokenizer = function(dir_path, folder_name) {
      save_location <- paste0(dir_path, "/", folder_name)
      create_dir(dir_path = save_location, trace = FALSE)
      save_to_disk(object = self$Tokenizer,
                   dir_path = save_location,
                   folder_name = "tokenizer")
    },
    #--------------------------------------------------------------------------
    load_tokenizer = function(dir_path) {
      load_location <- paste0(dir_path, "/", "tokenizer")
     self$Tokenizer <- load_from_disk(load_location)
    },
    #--------------------------------------------------------------------------
    load_BaseModel = function(dir_path) {

    },
    #--------------------------------------------------------------------------
    set_up_logger = function(log_dir, log_write_interval) {
      private$log_config$log_dir <- log_dir
      private$log_config$log_write_interval <- log_write_interval

      private$log_config$log_state_file <- paste0(private$log_config$log_dir, "/aifeducation_state.log")
      private$log_config$log_loss_file <- paste0(private$log_config$log_dir, "/aifeducation_loss.log")
    },
    #--------------------------------------------------------------------------
    update_logger = function(message) {
      private$log_state$value_top <- private$log_state$value_top + 1

      private$log_state$last_log <- py$write_log_py(
        log_file = private$log_config$log_state_file,
        value_top = private$log_state$value_top,
        total_top = private$log_state$total_top,
        message_top = message,
        last_log = private$log_state$last_log,
        write_interval = private$log_config$log_write_interval
      )
    },
    #--------------------------------------------------------------------------
    prepare_data_for_training = function(raw_text_dataset) {
      # Update Logger
      private$update_logger("Prepare Data for Training")

      # Trace
      print_message(
        msg = "Prepare Data for Training",
        trace = self$last_training$config$trace
      )

      transformers$RobertaTokenizerFast$from_pretrained("FacebookAI/roberta-base")

      tokenized_texts_raw <- tokenize_dataset(
        dataset = raw_text_dataset,
        tokenizer =self$Tokenizer$get_tokenizer(),
        max_length = self$last_training$config$max_sequence_length-private$adjust_max_sequence_length,
        log_file = private$log_config$log_state_file,
        write_interval = private$log_config$log_write_interval,
        value_top = private$log_state$value_top,
        total_top = private$log_state$total_top,
        message_top = "Tokenize Text"
      )

      length_vector <- tokenized_texts_raw["length"]
      if (self$last_training$config$full_sequences_only) {
        relevant_indices <- which(length_vector == self$last_training$config$max_sequence_length)
      } else {
        relevant_indices <- which(
          length_vector <= self$last_training$config$max_sequence_length &
            length_vector >= self$last_training$config$min_seq_len
        )
      }

      if (length(relevant_indices) > 0) {
        tokenized_texts_raw <- tokenized_texts_raw$select(as.integer(relevant_indices - 1))
      }

      tokenized_texts_raw$set_format(type = "torch")
      tokenized_texts_raw <- tokenized_texts_raw$train_test_split(
        test_size = self$last_training$config$val_size
      )
      return(tokenized_texts_raw)
    },
    #--------------------------------------------------------------------------
    create_data_collator = function() {
      # Update Logger
      private$update_logger("Create Data Collator")

      # Trace
      print_message(
        msg = "Create Data Collator",
        trace = self$last_training$config$trace
      )

      if (self$last_training$config$whole_word) {
        tmp_data_collator <- transformers$DataCollatorForWholeWordMask(
          tokenizer =self$Tokenizer$get_tokenizer(),
          mlm = TRUE,
          mlm_probability = self$last_training$config$p_mask,
          return_tensors = "pt"
        )
      } else {
        tmp_data_collator <- transformers$DataCollatorForLanguageModeling(
          tokenizer =self$Tokenizer$get_tokenizer(),
          mlm = TRUE,
          mlm_probability = self$last_training$config$p_mask,
          return_tensors = "pt"
        )
      }
      return(tmp_data_collator)
    },
    #---------------------------------------------------------------------------
    create_trainer = function(tokenized_dataset, data_collator) {
      # Update Logger
      private$update_logger("Create Trainer")

      # Trace
      print_message(
        msg = "Create Trainer",
        trace = self$last_training$config$trace
      )

      # Trace
      msg <- ifelse(self$last_training$config$whole_word, "Using Whole Word Masking", "Using Token Masking")
      print_message(msg, self$last_training$config$trace)

      create_logger <- py$create_AIFETransformerCSVLogger_PT
      logger_args <- list(
        loss_file = private$log_config$log_loss_file,
        log_file = private$log_config$log_state_file,
        value_top = private$log_state$value_top,
        total_top = private$log_state$total_top,
        message_top = "Training...",
        min_step = 1
      )
      logger <- do.call(create_logger, logger_args)

      if (check_versions(a = get_py_package_version("transformers"), operator = ">=", b = "4.46.0")) {
        training_args <- transformers$TrainingArguments(
          output_dir = private$dir_checkpoint,
          overwrite_output_dir = TRUE,
          eval_strategy = "epoch",
          num_train_epochs = as.integer(self$last_training$config$n_epoch),
          logging_strategy = "epoch",
          save_strategy = "epoch",
          save_total_limit = as.integer(1),
          load_best_model_at_end = TRUE,
          optim = "adamw_torch",
          learning_rate = self$last_training$config$learning_rate,
          per_device_train_batch_size = as.integer(self$last_training$config$batch_size),
          per_device_eval_batch_size = as.integer(self$last_training$config$batch_size),
          save_safetensors = TRUE,
          auto_find_batch_size = FALSE,
          report_to = "none",
          log_level = "error",
          disable_tqdm = !self$last_training$config$pytorch_trace,
          dataloader_pin_memory = torch$cuda$is_available()
        )
      } else {
        training_args <- transformers$TrainingArguments(
          output_dir = private$dir_checkpoint,
          overwrite_output_dir = TRUE,
          evaluation_strategy = "epoch",
          num_train_epochs = as.integer(self$last_training$config$n_epoch),
          logging_strategy = "epoch",
          save_strategy = "epoch",
          save_total_limit = as.integer(1),
          load_best_model_at_end = TRUE,
          optim = "adamw_torch",
          learning_rate = self$last_training$config$learning_rate,
          per_device_train_batch_size = as.integer(self$last_training$configbatch_size),
          per_device_eval_batch_size = as.integer(self$last_training$config$batch_size),
          save_safetensors = TRUE,
          auto_find_batch_size = FALSE,
          report_to = "none",
          log_level = "error",
          disable_tqdm = !self$last_training$config$pytorch_trace
        )
      }

      if (check_versions(a = get_py_package_version("transformers"), operator = ">=", b = "4.46.0")) {
        tmp_trainer <- transformers$Trainer(
          model = private$model,
          train_dataset = tokenized_dataset$train,
          eval_dataset = tokenized_dataset$test,
          args = training_args,
          data_collator = data_collator,
          processing_class =self$Tokenizer$get_tokenizer()
        )
      } else {
        tmp_trainer <- transformers$Trainer(
          model = private$model,
          train_dataset = tokenized_dataset$train,
          eval_dataset = tokenized_dataset$test,
          args = training_args,
          data_collator = data_collator,
          tokenizer =self$Tokenizer$get_tokenizer()
        )
      }

      tmp_trainer$remove_callback(transformers$integrations$CodeCarbonCallback)
      if (!as.logical(self$last_training$config$pytorch_trace)) {
        tmp_trainer$remove_callback(transformers$PrinterCallback)
        tmp_trainer$remove_callback(transformers$ProgressCallback)
      }

      # Add Callbacks
      tmp_trainer$add_callback(logger)

      return(tmp_trainer)
    },
    #----------------------------------------------------------------------------
    start_training = function(trainer) {
      # Update Logger
      private$update_logger("Training")

      # Trace
      print_message(
        msg = "Start Training",
        trace = self$last_training$config$trace
      )

      if (torch$cuda$is_available()) {
        torch$cuda$empty_cache()
      }
      trainer$train()
    },
    #---------------------------------------------------------------------------
    config_dataset_prograss_bar = function() {
      if (self$last_training$config$pytorch_trace == TRUE) {
        datasets$enable_progress_bars()
      } else {
        datasets$disable_progress_bars()
      }
    },
    #---------------------------------------------------------------------------
    do_configuration = function(args, model_type) {
      #Load or reload python scripts
      private$load_reload_python_scripts()

      # Check if the object is not configured
      private$check_config_for_FALSE()

      # Save args
      private$save_all_args(args = args, group = "configure")

      private$model_type <- model_type

      configuration <- private$create_model(args)

     self$Tokenizer <- args$tokenizer$clone(deep = TRUE)

      # Prevent the object from modification
      private$set_configuration_to_TRUE()
    },
    #--------------------------------------------------------------------------
    do_training = function(args) {
      #Check if the model is configured
      private$check_config_for_TRUE()

      #Load or reload python scripts
      private$load_reload_python_scripts()

      # Check all arguments
      check_all_args(args = args)

      # Save args
      private$save_all_args(args = args, group = "training")

      # set up logger
      private$set_up_logger(log_dir = args$log_dir, log_write_interval = args$log_write_interval)
      private$log_state$value_top <- 0
      private$log_state$total_top <- 6

      # Update logger
      private$update_logger("Prepare Process")

      # Configurate datasets progress bar
      private$config_dataset_prograss_bar()

      # Check and create temporary directory for checkpoints
      private$create_checkpoint_directory()

      # Start Sustainability Tracking
      private$init_and_start_sustainability_tracking()

      # Prepare Data for Training
      prepared_data <- private$prepare_data_for_training(raw_text_dataset = args$text_dataset$get_dataset())

      # Create Data Collator
      data_collator <- private$create_data_collator()

      # Create Trainer
      trainer <- private$create_trainer(
        tokenized_dataset = prepared_data,
        data_collator = data_collator
      )

      # Start Training
      private$start_training(trainer)

      # Save training history
      history_log <- pandas$DataFrame(trainer$state$log_history)
      self$last_training$history <- clean_pytorch_log_transformers(history_log)

      # Stop sustainability tracking if requested
      private$stop_sustainability_tracking(task="training")

      # Clean temporary directory
      private$clean_checkpoint_directory()

      # Update logger
      private$update_logger("Finish")

      # Trace
      print_message(
        msg = "Finish",
        trace = self$last_training$config$trace
      )
    }
  ),
  public = list(

    Tokenizer = NULL,

    #Method creates a base model from hugging face
    create_from_hf=function(model_dir=NULL){
      #Load the BaseModel
      tmp_model=transformers$AutoModelForMaskedLM$from_pretrained(model_dir)

      #Check if the model is the correct model type
      detected_model_type=detect_base_model_type(tmp_model)
      if(detected_model_type!=private$model_type){
        stop(paste0("Detected ",detected_model_type," but expected ",private$model_type,"."))
      }

      #Add model to the R6 class
      private$model=tmp_model

      #Set Model Config
      args=rlang::fn_fmls_names(self$configure)
      for(arg in intersection(x=args,y=names(private$model$config))){
        private$model_config[arg]=list(private$model$config[arg])
      }

      #Create and Load the Tokenizer
      tokenizer=HuggingFaceTokenizer$new()
      tokenizer$create_from_hf(model_dir)
      self$Tokenizer=tokenizer

      # Set configured to TRUE to avoid changes in the model
      private$set_configuration_to_TRUE
    },
    #--------------------------------------------------------------------------
    train=function(text_dataset,
                   p_mask = 0.15,
                   whole_word = TRUE,
                   val_size = 0.1,
                   n_epoch = 1,
                   batch_size = 12,
                   max_sequence_length = 250,
                   full_sequences_only = FALSE,
                   min_seq_len = 50,
                   learning_rate = 3e-3,
                   sustain_track = FALSE,
                   sustain_iso_code = NULL,
                   sustain_region = NULL,
                   sustain_interval = 15,
                   trace = TRUE,
                   pytorch_trace = 1,
                   log_dir = NULL,
                   log_write_interval = 2){
      private$do_training(args=get_called_args(n=1))
    },
    #---------------------------------------------------------------------------
    #' @description Method for counting the trainable parameters of a model.
    #' @param with_head `bool` If `TRUE` the number of parameters is returned including
    #' the language modeling head of the model. If `FALSE` only the number of parameters of
    #' the core model is returned.
    #' @return Returns the number of trainable parameters of the model.
    count_parameter = function() {
      iterator <- reticulate::as_iterator(private$model$parameters())
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
    #--------------------------------------------------------------------------
    #' @description Method for requesting a plot of the training history.
    #' This method requires the *R* package 'ggplot2' to work.
    #' @param y_min Minimal value for the y-axis. Set to `NULL` for an automatic adjustment.
    #' @param y_max Maximal value for the y-axis. Set to `NULL` for an automatic adjustment.
    #' @return Returns a plot of class `ggplot` visualizing the training process.
    plot_training_history = function(y_min = NULL, y_max = NULL,text_size=10) {
      requireNamespace("ggplot2")
      plot_data <- self$last_training$history

      if(is.null(y_min)){
        y_min=min(self$last_training$history[,c("loss", "val_loss")])
      }

      if(is.null(y_max)){
        y_max=max(self$last_training$history[,c("loss", "val_loss")])
      }

      colnames <- c("epoch", "val_loss", "loss")
      cols_exist <- sum(colnames %in% colnames(plot_data)) == length(colnames)

      if (cols_exist) {

        val_loss_min <- min(plot_data$val_loss)
        best_model_epoch <- which(x = (plot_data$val_loss) == val_loss_min)

        plot <- ggplot2::ggplot(data = plot_data) +
          ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$loss, color = "train")) +
          ggplot2::geom_line(ggplot2::aes(x = .data$epoch, y = .data$val_loss, color = "validation")) +
          ggplot2::geom_vline(
            xintercept = best_model_epoch,
            linetype = "dashed"
          )

        plot <- plot + ggplot2::theme_classic() +
          ggplot2::ylab("value") +
          ggplot2::coord_cartesian(ylim = c(y_min, y_max)) +
          ggplot2::xlab("epoch") +
          ggplot2::scale_color_manual(values = c(
            "train" = "red",
            "validation" = "blue",
            "test" = "darkgreen"
          )) +
          ggplot2::theme(
            text = ggplot2::element_text(size = text_size),
            legend.position = "bottom"
          )
        return(plot)
      } else {
        warning("Data for the training history is not available.")
        return(NULL)
      }
    },
    get_special_tokens=function(){
      return(self$Tokenizer$get_special_tokens())
    },
    get_tokenizer_statistics=function(){
      return(self$Tokenizer$get_tokenizer_statistics())
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
      check_type(object = text, type = "string", FALSE)
      check_type(object = n_solutions, type = "int", FALSE)


      framework <- "pt"
      private$model$to("cpu")

      return_token_type_ids <- (private$model_type != AIFETrType$mpnet)

      if (private$model_type != "mpnet") {
        run_py_file("FillMaskForMPLM.py")
        fill_mask_pipeline_class <- py$FillMaskPipelineForMPLM
      } else {
        fill_mask_pipeline_class <- transformers$FillMaskPipeline
      }

      fill_mask_pipeline <- fill_mask_pipeline_class(
        model = private$model,
        tokenizer =self$Tokenizer$get_tokenizer(),
        framework = "pt",
        num_workers = 1,
        binary_output = FALSE,
        top_k = as.integer(n_solutions),
        tokenizer_kwargs = reticulate::dict(list(return_token_type_ids = return_token_type_ids))
      )

      special_tokens <- self$Tokenizer$get_special_tokens()
      mask_token <- special_tokens[special_tokens[, "type"] == "mask_token", "token"]

      n_mask_tokens <- ncol(stringi::stri_extract_all_fixed(
        str = text,
        pattern = mask_token,
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
        for (i in seq_len(length(solutions))) {
          solution_data_frame[i, "score"] <- solutions[[i]]$score
          solution_data_frame[i, "token"] <- solutions[[i]]$token
          solution_data_frame[i, "token_str"] <- solutions[[i]]$token_str
        }
        solution_data_frame <- as.data.frame(solution_data_frame)
        solution_data_frame$score <- as.numeric(solution_data_frame$score)
        solutions_list[length(solutions_list) + 1] <- list(solution_data_frame)
      } else {
        for (j in seq_len(length(solutions))) {
          solution_data_frame <- matrix(
            nrow = length(solutions[[j]]),
            ncol = 3
          )
          colnames(solution_data_frame) <- c(
            "score",
            "token",
            "token_str"
          )
          for (i in seq_len(length(solutions[[j]]))) {
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
    save = function(dir_path, folder_name) {
      save_location <- paste0(dir_path, "/", folder_name)
      create_dir(dir_path = save_location, trace = FALSE)

      # Save BaseModel
      private$model$save_pretrained(
        save_directory = save_location,
        safe_serilization = TRUE
      )

      # Save Tokenizer
      private$save_tokenizer(dir_path = dir_path, folder_name = folder_name)

      # Save Sustainability Data
      private$save_sustainability_data(dir_path = dir_path, folder_name =folder_name)

      # Save training history
      private$save_training_history(dir_path = dir_path, folder_name = folder_name)
    },
    #--------------------------------------------------------------------------
    load_from_disk = function(dir_path) {

      #Load private and public config files
      private$load_config_file(dir_path)

      #Load or reload python scripts
      private$load_reload_python_scripts()

      # Load BaseModel
      private$load_BaseModel(dir_path = dir_path)

      # Load Tokenizer
      private$load_tokenizer(dir_path = dir_path)

      # Load Sustainability Data
      private$load_sustainability_data(model_dir = dir_path)

      # Load training history
      private$load_training_history(model_dir = dir_path)

      #Set configured to TRUE
      private$set_configuration_to_TRUE()
    },
    #--------------------------------------------------------------------------
    get_model=function(){
      return(private$model)
    },
    #--------------------------------------------------------------------------
    get_model_type=function(){
      return(private$model_type)
    },
    get_final_size=function(){
      return(private$model$config$hidden_size)
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
    }
  )
)
