testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

# Config transformer library
transformers$utils$logging$set_verbosity_error()
os$environ$setdefault("TOKENIZERS_PARALLELISM", "false")

# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

# Path Management
test_art_path <- testthat::test_path("test_artefacts")
test_art_tmp_path <- testthat::test_path("test_artefacts/base_models")
create_dir(test_art_path, FALSE)
create_dir(test_art_tmp_path, FALSE)

test_tmp_data_path <- testthat::test_path("test_data_tmp")
create_dir(test_tmp_data_path, FALSE)
test_tmp_data_base_model_path=paste0(test_tmp_data_path,"/","TEM")

create_dir(test_tmp_data_base_model_path, FALSE)

# Data Management
example_data <- imdb_movie_reviews
raw_texts <- LargeDataSetForText$new(example_data)

# Test Configuration
object_class_names <- c(
  "BaseModelBert",
  #"BaseModelFunnel",
  #"BaseModelLongformer"#,
  "BaseModelModernBert",
  "BaseModelRoberta",
  "BaseModelMPNet"
  )

max_samples <- 1
max_samples_CI <- 1


samples_config <- check_adjust_n_samples_on_CI(
  n_samples_requested = max_samples,
  n_CI = max_samples_CI
)

for (object_class_name in object_class_names) {
  for (i in 1:samples_config) {

# Prepare Tokenizer for the models
    raw_texts_training <- LargeDataSetForText$new(example_data[1:50, ])

tokenizer <- WordPieceTokenizer$new()
tokenizer$configure(
  vocab_size = 2000,
  vocab_do_lower_case = TRUE
)
tokenizer$train(
  text_dataset = raw_texts,
  statistics_max_tokens_length = 256,
  sustain_track = TRUE,
  sustain_iso_code = "DEU",
  sustain_region = NULL,
  sustain_interval = 15,
  trace = FALSE
)

    config_args <- generate_args_for_tests(
      object_name = object_class_name,
      method = "configure",
      var_objects = list(),
      necessary_objects = list(
        text_dataset = raw_texts_training,
        tokenizer = tokenizer
      ),
      var_override = list(
        sustain_interval = 30,
        sustain_iso_code = "DEU",
        block_sizes = c(4, 4),
        epochs = 2,
        max_position_embeddings = 512,
        hidden_size = 16,
        num_hidden_layers = 3,
        attention_window=4,
        num_attention_heads = 2,
        intermediate_size = 32
      )
    )
    train_args <- generate_args_for_tests(
      object_name = object_class_name,
      method = "train",
      var_objects = list(),
      necessary_objects = list(
        text_dataset = raw_texts_training
      ),
      var_override = list(
        sustain_interval = 30,
        sustain_iso_code = "DEU",
        n_epoch = 2,
        max_sequence_length = 32,
        min_seq_len = 16,
        val_size = 0.25,
        learning_rate = 3e-3
      )
    )

    #Create and train model
    base_model <- create_object(object_class_name)
    do.call(
      what = base_model$configure,
      args = config_args
    )
    do.call(
      what = base_model$train,
      args = train_args
    )

    #Prepare directory
    tmp_dir <- paste0(test_art_tmp_path, "/", object_class_name)
    # Clear directory for next test
    unlink(paste0(tmp_dir,"/",object_class_name), recursive = TRUE)
    create_dir(tmp_dir, trace = FALSE)

    #--------------------------------------------------------------------------
    test_that(paste(
      "Save Model",
      object_class_name,
      get_current_args_for_print(config_args),
      get_current_args_for_print(train_args)
    ), {

      expect_no_error(
        save_to_disk(
          object = base_model,
          dir_path = test_art_tmp_path,
          folder_name = object_class_name
        )
      )
    })

    test_that(paste(
      "Sustainability Tracking",
      object_class_name,
      get_current_args_for_print(config_args),
      get_current_args_for_print(train_args)
    ), {
      if (train_args$sustain_track == TRUE) {
        expect_equal(nrow(base_model$get_sustainability_data()$track_log), 1)
      } else {
        expect_equal(nrow(base_model$get_sustainability_data()$track_log), 0)
      }
    })

    test_that(paste(
      "History Plot",
      object_class_name,
      get_current_args_for_print(config_args),
      get_current_args_for_print(train_args)
    ), {
      history=base_model$last_training$history
      expect_equal(nrow(history), 2)
      expect_equal(ncol(history), 3)
      expect_true("epoch" %in% colnames(history))
      expect_true("loss" %in% colnames(history))
      expect_true("val_loss" %in% colnames(history))

      expect_s3_class(object = base_model$plot_training_history(y_min=NULL,y_max = NULL), class = "ggplot")
      expect_s3_class(object = base_model$plot_training_history(y_min=0,y_max = NULL), class = "ggplot")
      expect_s3_class(object = base_model$plot_training_history(y_min=0,y_max = 10), class = "ggplot")
      expect_s3_class(object = base_model$plot_training_history(y_min=NULL,y_max = 10), class = "ggplot")
    })

    test_that(paste(
      "Fill-Mask",
      object_class_name,
      get_current_args_for_print(config_args),
      get_current_args_for_print(train_args)
    ), {
      tokens <- base_model$get_special_tokens()
      mask_token <- tokens[which(tokens[, 1] == "mask_token"), 2]

      first_solution <- base_model$fill_mask(
        masked_text = paste("This is a", mask_token, "."),
        n_solutions = 5
      )

      expect_equal(length(first_solution), 1)
      expect_true(is.data.frame(first_solution[[1]]))
      expect_equal(nrow(first_solution[[1]]), 5)
      expect_equal(ncol(first_solution[[1]]), 3)

      second_solution <- base_model$fill_mask(
        masked_text = paste("This is a", mask_token, "."),
        n_solutions = 1
      )
      expect_equal(length(second_solution), 1)
      expect_true(is.data.frame(second_solution[[1]]))
      expect_equal(nrow(second_solution[[1]]), 1)
      expect_equal(ncol(second_solution[[1]]), 3)

      third_solution <- base_model$fill_mask(
        masked_text = paste(
          "This is a", mask_token, ".",
          "The weather is", mask_token, "."
        ),
        n_solutions = 5
      )
      expect_equal(length(third_solution), 2)
      for (i in 1:2) {
        expect_true(is.data.frame(third_solution[[i]]))
        expect_equal(nrow(third_solution[[i]]), 5)
        expect_equal(ncol(third_solution[[i]]), 3)
      }
    })

    test_that(paste(
      "Sustainaility Inference Fill-Mask",
      object_class_name,
      get_current_args_for_print(config_args),
      get_current_args_for_print(train_args)
    ), {
      n_repeat=2
      for(j in 1:n_repeat){
        base_model$estimate_sustainability_inference_fill_mask(
          text_dataset=raw_texts_training,
          n=30,
          sustain_iso_code = "DEU",
          sustain_region = NULL,
          sustain_interval = 15,
          trace = train_args$trace
        )
        expect_equal(nrow(base_model$get_sustainability_data("inference")),j)
      }
    })

    test_that(paste(
      "Flops Estimates",
      object_class_name,
      get_current_args_for_print(config_args),
      get_current_args_for_print(train_args)
    ), {
      expect_equal(nrow(base_model$get_flops_estimates()), 1)

    })

    #---------------------------------------------------------------------------
    #Re-Load Base Model and compare with the initial model
    base_model_reloaded <- load_from_disk(
      dir_path = tmp_dir
    )

    test_that(paste(
      "Saving and Loading",
      object_class_name,
      get_current_args_for_print(config_args),
      get_current_args_for_print(train_args)
    ),{


    expect_equal(
      base_model$count_parameter(),
      base_model_reloaded$count_parameter()
    )

      expect_equal(
        base_model$get_sustainability_data(),
        base_model_reloaded$get_sustainability_data()
      )

      expect_equal(
        base_model$Tokenizer$get_tokenizer_statistics(),
        base_model_reloaded$Tokenizer$get_tokenizer_statistics()
      )

      expect_equal(
        base_model$Tokenizer$get_sustainability_data("training"),
        base_model_reloaded$Tokenizer$get_sustainability_data("training")
      )

      expect_equal(
        base_model$Tokenizer$get_sustainability_data("inference"),
        base_model_reloaded$Tokenizer$get_sustainability_data("inference")
      )

      expect_equal(
        base_model$get_flops_estimates(),
        base_model_reloaded$get_flops_estimates()
      )
  })

    if(i==1){
      # Clear directory for next test
      unlink(paste0(test_tmp_data_base_model_path,"/",object_class_name), recursive = TRUE)
      save_to_disk(
        object = base_model,
        dir_path = test_tmp_data_base_model_path,
        folder_name = object_class_name
      )
    }

    # Clear directory for next test
    unlink(paste0(tmp_dir), recursive = TRUE)
  }
}





