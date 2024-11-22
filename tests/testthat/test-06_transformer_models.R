testthat::skip_on_cran()
if(Sys.getenv("CI")=="true"){
  testthat::skip_if_not(
    condition = check_aif_py_modules(trace = FALSE,check = "pytorch"),
    message = "Necessary python modules not available"
  )
} else {
  testthat::skip_if_not(
    condition = check_aif_py_modules(trace = FALSE,check = "all"),
    message = "Necessary python modules not available"
  )

  # SetUp tensorflow
  aifeducation::set_config_gpu_low_memory()
  set_config_tf_logger("ERROR")
  set_config_os_environ_logger("ERROR")
}

#Config transformer library
transformers$utils$logging$set_verbosity_error()
os$environ$setdefault("TOKENIZERS_PARALLELISM", "false")

# Disable tqdm progressbar
transformers$logging$disable_progress_bar()
datasets$disable_progress_bars()

#config trace
trace=FALSE

test_art_path <- testthat::test_path("test_artefacts")
test_art_tmp_path <- testthat::test_path("test_artefacts/base_models")
tmp_full_models_pt_path <- paste0(test_art_tmp_path, "/pytorch")
tmp_full_models_tf_path <- paste0(test_art_tmp_path, "/tensorflow")
create_dir(test_art_path, FALSE)
create_dir(test_art_tmp_path, FALSE)
create_dir(tmp_full_models_pt_path, FALSE)
create_dir(tmp_full_models_tf_path, FALSE)

#Results of this test are used within the test for TextEmbeddingModels
tmp_results_folder_path<-testthat::test_path("test_data_tmp")
create_dir(tmp_results_folder_path, FALSE)
tmp_results_TEM_path<-paste0(tmp_results_folder_path,"/TEM")
create_dir(tmp_results_TEM_path, FALSE)
create_dir(paste0(tmp_results_TEM_path,"/pytorch"), FALSE)
create_dir(paste0(tmp_results_TEM_path,"/tensorflow"), FALSE)

if(Sys.getenv("CI")=="true"){
  ml_frameworks <- c(
    "pytorch"
  )
} else {
  ml_frameworks <- c(
    "tensorflow",
    "pytorch"
  )
}

ai_methods <- unname(unlist(AIFETrType))

ai_framework_matrix <- matrix(
  ncol = length(ml_frameworks),
  nrow = length(ai_methods),
  data = 1,
  dimnames = list(ai_methods, ml_frameworks),
  byrow = TRUE
)

rows_susatainability <- c(
  "bert" = 3,
  "funnel" = 3,
  "roberta" = 2,
  "longformer" = 2,
  "deberta_v2" = 3
)

supported_methods<-list(
  tensorflow=list(
    "bert" ,
    "funnel" ,
    "roberta" ,
    "longformer" ,
    "deberta_v2"
  ),
  pytorch=list(
    "bert" ,
    "funnel" ,
    "roberta" ,
    "longformer" ,
    "deberta_v2",
    "mpnet"
  )
)

example_data <- imdb_movie_reviews

for (framework in ml_frameworks) {
  for (ai_method in supported_methods[[framework]]) {
    base::gc(verbose = FALSE, full = TRUE)

    #create main folder ans sub-folder for every model in creation
    tmp_ai_method_path <- paste0(test_art_tmp_path,"/", framework,"/", ai_method)
    create_dir(tmp_ai_method_path, FALSE)
    tmp_ai_create<-paste0(tmp_ai_method_path,"/create")
    create_dir(tmp_ai_create, FALSE)

    #Create folder and sub-folder for tmp results used in other tests
    tmp_ai_train<- paste0(tmp_results_TEM_path,"/", framework,"/", ai_method)
    create_dir(tmp_ai_train, FALSE)

    if (ai_framework_matrix[ai_method, framework] == 1) {
      # Creation of the Model ----

      test_that(paste0(ai_method, ": creation of the model with ", framework), {
        if (ai_method == AIFETrType$bert) {
          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 50000,
              vocab_do_lower_case = FALSE,
              max_position_embeddings = 512,
              hidden_size = 256,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 256,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )

          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 50000,
              vocab_do_lower_case = TRUE,
              max_position_embeddings = 512,
              hidden_size = 256,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 256,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )

        } else if (ai_method == AIFETrType$roberta) {
          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              add_prefix_space = FALSE,
              max_position_embeddings = 512,
              hidden_size = 16,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )

          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              add_prefix_space = TRUE,
              max_position_embeddings = 512,
              hidden_size = 32,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = FALSE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )
        } else if (ai_method == AIFETrType$deberta_v2) {
          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              vocab_do_lower_case = FALSE,
              max_position_embeddings = 512,
              hidden_size = 32,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )

          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              vocab_do_lower_case = TRUE,
              max_position_embeddings = 512,
              hidden_size = 32,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = FALSE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )
        } else if (ai_method == AIFETrType$funnel) {
          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              max_position_embeddings = 512,
              hidden_size = 32,
              block_sizes = c(2, 2, 2),
              num_decoder_layers = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )

          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              max_position_embeddings = 512,
              hidden_size = 32,
              block_sizes = c(2, 2, 2),
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = FALSE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )
        } else if (ai_method == AIFETrType$longformer) {
          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              add_prefix_space = FALSE,
              max_position_embeddings = 512,
              hidden_size = 32,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              attention_window = 40,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )

          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              add_prefix_space = TRUE,
              max_position_embeddings = 512,
              hidden_size = 32,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              attention_window = 40,
              sustain_track = FALSE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )
        } else if (ai_method == AIFETrType$mpnet) {
          expect_no_error(
            aife_transformer_maker$make(ai_method)$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 50000,
              vocab_do_lower_case = FALSE,
              max_position_embeddings = 512,
              hidden_size = 256,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 256,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          )
        } else {
          cat(paste("Creation of the Model: unknown transformer '", ai_method, "'\n"))
        }
      })

      # Training of the Model ----
      test_that(paste0(ai_method, ": training of the model with ", framework), {
        print("train")
        if (ai_method == AIFETrType$bert) {

          base_model<-aife_transformer_maker$make(ai_method)
          base_model$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 50000,
              vocab_do_lower_case = FALSE,
              max_position_embeddings = 512,
              hidden_size = 256,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 256,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          Sys.sleep(5)
          print("test_a")
          expect_no_error(
            base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.15,
              whole_word = TRUE,
              full_sequences_only = TRUE,
              val_size = 0.25,
              n_epoch = 2,
              batch_size = 2,
              chunk_size = 100,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
          Sys.sleep(5)
          print("test_b")
          expect_no_error(
            base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.30,
              whole_word = FALSE,
              full_sequences_only = TRUE,
              val_size = 0.1,
              n_epoch = 2,
              batch_size = 1,
              chunk_size = 100,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
        } else if (ai_method == AIFETrType$roberta) {
          base_model<-aife_transformer_maker$make(ai_method)
          base_model$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              add_prefix_space = FALSE,
              max_position_embeddings = 512,
              hidden_size = 16,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          Sys.sleep(5)

          expect_no_error(
            base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.30,
              val_size = 0.1,
              n_epoch = 2,
              batch_size = 1,
              chunk_size = 70,
              full_sequences_only = TRUE,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
        } else if (ai_method == AIFETrType$deberta_v2) {
          base_model<-aife_transformer_maker$make(ai_method)
          base_model$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              vocab_do_lower_case = FALSE,
              max_position_embeddings = 512,
              hidden_size = 32,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
          )
          Sys.sleep(3)

          expect_no_error(
            base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.15,
              whole_word = TRUE,
              val_size = 0.1,
              n_epoch = 2,
              batch_size = 2,
              chunk_size = 100,
              full_sequences_only = FALSE,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
          Sys.sleep(2)
          expect_no_error(base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.15,
              whole_word = FALSE,
              val_size = 0.1,
              n_epoch = 2,
              batch_size = 2,
              chunk_size = 100,
              full_sequences_only = FALSE,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
        } else if (ai_method == AIFETrType$funnel) {
          base_model<-aife_transformer_maker$make(ai_method)
          base_model$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              max_position_embeddings = 512,
              hidden_size = 32,
              block_sizes = c(2, 2, 2),
              num_decoder_layers = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          Sys.sleep(3)

          expect_no_error(
            base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.15,
              whole_word = TRUE,
              val_size = 0.1,
              n_epoch = 2,
              batch_size = 2,
              min_seq_len = 50,
              full_sequences_only = TRUE,
              chunk_size = 250,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
          Sys.sleep(2)
          expect_no_error(
            base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.15,
              whole_word = FALSE,
              val_size = 0.1,
              n_epoch = 2,
              batch_size = 2,
              min_seq_len = 50,
              full_sequences_only = TRUE,
              chunk_size = 250,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
        } else if (ai_method == AIFETrType$longformer) {
          base_model<-aife_transformer_maker$make(ai_method)
          base_model$create(
              ml_framework = framework,
              model_dir = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              vocab_size = 10000,
              add_prefix_space = FALSE,
              max_position_embeddings = 512,
              hidden_size = 32,
              num_hidden_layer = 2,
              num_attention_heads = 2,
              intermediate_size = 128,
              hidden_act = "gelu",
              hidden_dropout_prob = 0.1,
              attention_window = 40,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace
            )
          Sys.sleep(3)

          expect_no_error(base_model$train(
              ml_framework = framework,
              output_dir = tmp_ai_train,
              model_dir_path = tmp_ai_create,
              text_dataset = LargeDataSetForText$new(example_data[1:50,]),
              p_mask = 0.30,
              val_size = 0.1,
              n_epoch = 2,
              batch_size = 1,
              chunk_size = 512,
              full_sequences_only = FALSE,
              n_workers = 1,
              multi_process = FALSE,
              sustain_track = TRUE,
              sustain_iso_code = "DEU",
              sustain_region = NULL,
              sustain_interval = 15,
              trace = trace,
              keras_trace = as.numeric(trace),
              pytorch_trace = as.numeric(trace)
            )
          )
        } else if (ai_method == AIFETrType$mpnet) {
          if (framework == "pytorch") {
            base_model<-aife_transformer_maker$make(ai_method)
            base_model$create(
                ml_framework = framework,
                model_dir = tmp_ai_create,
                text_dataset = LargeDataSetForText$new(example_data[1:50,]),
                vocab_size = 50000,
                vocab_do_lower_case = FALSE,
                max_position_embeddings = 512,
                hidden_size = 256,
                num_hidden_layer = 2,
                num_attention_heads = 2,
                intermediate_size = 256,
                hidden_act = "gelu",
                hidden_dropout_prob = 0.1,
                sustain_track = TRUE,
                sustain_iso_code = "DEU",
                sustain_region = NULL,
                sustain_interval = 15,
                trace = trace
              )
            Sys.sleep(3)

            expect_no_error(
              base_model$train(
                ml_framework = framework,
                output_dir = tmp_ai_train,
                model_dir_path = tmp_ai_create,
                text_dataset = LargeDataSetForText$new(example_data[1:50,]),
                p_mask = 0.15,
                p_perm = 0.15,
                whole_word = TRUE,
                full_sequences_only = TRUE,
                val_size = 0.25,
                n_epoch = 2,
                batch_size = 20,
                chunk_size = 100,
                n_workers = 1,
                multi_process = FALSE,
                sustain_track = TRUE,
                sustain_iso_code = "DEU",
                sustain_region = NULL,
                sustain_interval = 15,
                trace = trace,
                keras_trace = as.numeric(trace),
                pytorch_trace = as.numeric(trace)
              )
            )
          }
        } else {
          cat(paste("Training of the Model: unknown transformer '", ai_method, "'\n"))
        }
      })
    } else {
      cat(paste(framework, "not supported for", ai_method, "\n"))
    }

    #Clean Directory
    if(dir.exists(tmp_ai_create)){
      unlink(
        x=tmp_ai_create,
        recursive = TRUE
      )
    }
    #if(dir.exists(tmp_ai_train)){
    #  unlink(
    #    x=tmp_ai_create,
    #    recursive = TRUE
    #  )
    #}
  }
}

#Clean Directory
if(dir.exists(test_art_tmp_path)){
  unlink(
    x=test_art_tmp_path,
    recursive = TRUE
  )
}
