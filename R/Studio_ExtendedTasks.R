# LargeDataSetForText ------------------------------------------------------------
long_add_texts_to_dataset <- function(source_path,
                                      destination_path,
                                      destination_folder,
                                      log_path,
                                      include_txt,
                                      include_pdf,
                                      include_xlsx,
                                      excel_id_column,
                                      excel_text_column,
                                      excel_license_column,
                                      excel_bib_entry_column,
                                      log_write_interval = 2) {
  promises::future_promise({
    # Set up top level progress monitoring
    top_total <- include_txt + include_pdf + include_xlsx
    top_value <- 0
    total_message <- "File types"

    # Create new data set
    new_dataset <- LargeDataSetForText$new()

    # Start processing different file types
    if (include_txt) {
      new_dataset$add_from_files_txt(
        dir_path = source_path,
        batch_size = 2,
        log_file = log_path,
        log_top_value = top_value,
        log_top_total = top_total,
        log_top_message = total_message,
        log_write_interval = log_write_interval,
        trace = FALSE
      )
      top_value <- top_value + 1
    }

    if (include_pdf) {
      new_dataset$add_from_files_pdf(
        dir_path = source_path,
        batch_size = 2,
        log_file = log_path,
        log_top_value = top_value,
        log_top_total = top_total,
        log_top_message = total_message,
        log_write_interval = log_write_interval,
        trace = FALSE
      )
      top_value <- top_value + 1
    }

    if (include_xlsx) {
      new_dataset$add_from_files_xlsx(
        dir_path = source_path,
        trace = FALSE,
        id_column = excel_id_column,
        text_column = excel_text_column,
        license_column = excel_license_column,
        bib_entry_column = excel_bib_entry_column,
        log_file = log_path,
        log_top_value = top_value,
        log_top_total = top_total,
        log_top_message = total_message,
        log_write_interval = log_write_interval,
      )
      top_value <- top_value + 1
    }

    # Save
    save_to_disk(
      object = new_dataset,
      dir_path = destination_path,
      folder_name = destination_folder
    )

    # Returns number of documents added to the data set
    return(new_dataset$n_rows())
  })
}

# TextEmbeddingModel --------------------------------------------------------------
long_transform_text_to_embeddings <- function(source_path,
                                              destination_path,
                                              destination_folder,
                                              log_path,
                                              batch_size,
                                              model_path,
                                              log_write_interval = 2) {
  promises::future_promise({
    # Read the large data set for raw texts
    raw_texts <- load_from_disk(source_path)

    # Set up top level progress monitoring
    # TODO (Yuliia): remove? Variables are not used
    top_total <- raw_texts$n_rows()
    top_value <- 0
    total_message <- "Documents"

    # Load the model
    model <- load_from_disk(model_path)

    # Start embedding
    embeddings <- model$embed_large(
      large_datas_set = raw_texts,
      batch_size = batch_size,
      trace = FALSE,
      log_file = log_path,
      log_write_interval = log_write_interval
    )

    # Save
    save_to_disk(
      object = embeddings,
      dir_path = destination_path,
      folder_name = destination_folder
    )

    # Returns number of documents that are embedded
    return(embeddings$n_rows())
  })
}

# Classifiers =====================================================================

long_classifier <- function(classifier_type,
                            destination_path,
                            folder_name,
                            path_to_embeddings,
                            path_to_target_data,
                            target_levels,
                            path_to_feature_extractor,
                            target_data_column,
                            name,
                            label,
                            data_folds,
                            data_val_size,
                            balance_class_weights,
                            balance_sequence_length,
                            use_sc,
                            sc_method,
                            sc_min_k,
                            sc_max_k,
                            use_pl,
                            pl_max_steps,
                            pl_max,
                            pl_anchor,
                            pl_min,
                            sustain_iso_code,
                            epochs,
                            batch_size,
                            log_dir,
                            dense_layers,
                            dense_size,
                            rec_layers ,
                            rec_size,
                            rec_type,
                            rec_bidirectional,
                            self_attention_heads,
                            intermediate_size,
                            attention_type,
                            add_pos_embedding,
                            rec_dropout,
                            repeat_encoder,
                            dense_dropout,
                            recurrent_dropout,
                            encoder_dropout,
                            optimizer,
                            log_write_interval = 2,
                            embedding_dim,
                            Ns,
                            Nq,
                            loss_alpha,
                            loss_margin,
                            sampling_separate,
                            sampling_shuffle) {
  promises::future_promise({
    # Load data
    embeddings <- load_from_disk(path_to_embeddings)
    target_data <- long_load_target_data(
      file_path = path_to_target_data,
      selectet_column = target_data_column
    )

    # Load feature extractor if provided
    if (!is.null(path_to_feature_extractor)) {
      feature_extractor <- load_from_disk(path_to_feature_extractor)
    } else {
      feature_extractor <- NULL
    }

    # Check for valid arguments
    if (is.null(self_attention_heads)) {
      self_attention_heads <- 0
    }

    # Create dir for checkpints
    dir_destination <- paste0(
      destination_path, "/",
      folder_name
    )
    dir_checkpoints <- paste0(
      dir_destination, "/",
      "checkpoints"
    )
    create_dir(dir_destination, FALSE)
    create_dir(dir_checkpoints, FALSE)

    if (classifier_type == "regular") {
      # Create Classifier
      classifier <- TEClassifierRegular$new()
      classifier$configure(
        ml_framework = "pytorch",
        name = name,
        label = label,
        text_embeddings = embeddings,
        feature_extractor = feature_extractor,
        target_levels = target_levels,
        dense_layers = dense_layers,
        dense_size=dense_size,
        rec_layers = rec_layers,
        rec_size=rec_size,
        rec_type = rec_type,
        rec_bidirectional = rec_bidirectional,
        self_attention_heads = self_attention_heads,
        intermediate_size = intermediate_size,
        attention_type = attention_type,
        add_pos_embedding = add_pos_embedding,
        rec_dropout = rec_dropout,
        repeat_encoder = repeat_encoder,
        dense_dropout = dense_dropout,
        recurrent_dropout = recurrent_dropout,
        encoder_dropout = encoder_dropout,
        optimizer = optimizer
      )

      # Train classifier
      classifier$train(
        data_embeddings = embeddings,
        data_targets = target_data,
        data_folds = data_folds,
        data_val_size = data_val_size,
        balance_class_weights = balance_class_weights,
        balance_sequence_length = balance_sequence_length,
        use_sc = use_sc,
        sc_method = sc_method,
        sc_min_k = sc_min_k,
        sc_max_k = sc_max_k,
        use_pl = use_pl,
        pl_max_steps = pl_max_steps,
        pl_max = pl_max,
        pl_anchor = pl_anchor,
        pl_min = pl_min,
        sustain_track = TRUE,
        sustain_iso_code = sustain_iso_code,
        sustain_region = NULL,
        sustain_interval = 15,
        epochs = epochs,
        batch_size = batch_size,
        dir_checkpoint = dir_checkpoints,
        trace = FALSE,
        ml_trace = 0,
        log_dir = log_dir,
        log_write_interval = log_write_interval
      )
    } else if (classifier_type == "protonet") {
      # Create
      classifier <- TEClassifierProtoNet$new()
      classifier$configure(
        ml_framework = "pytorch",
        embedding_dim = embedding_dim,
        name = name,
        label = label,
        text_embeddings = embeddings,
        feature_extractor = feature_extractor,
        target_levels = target_levels,
        dense_layers = dense_layers,
        dense_size=dense_size,
        rec_layers = rec_layers,
        rec_size=rec_size,
        rec_type = rec_type,
        rec_bidirectional = rec_bidirectional,
        self_attention_heads = self_attention_heads,
        intermediate_size = intermediate_size,
        attention_type = attention_type,
        add_pos_embedding = add_pos_embedding,
        rec_dropout = rec_dropout,
        repeat_encoder = repeat_encoder,
        dense_dropout = dense_dropout,
        recurrent_dropout = recurrent_dropout,
        encoder_dropout = encoder_dropout,
        optimizer = optimizer
      )

      # Train
      classifier$train(
        data_embeddings = embeddings,
        data_targets = target_data,
        data_folds = data_folds,
        data_val_size = data_val_size,
        use_sc = use_sc,
        sc_method = sc_method,
        sc_min_k = sc_min_k,
        sc_max_k = sc_max_k,
        use_pl = use_pl,
        pl_max_steps = pl_max_steps,
        pl_max = pl_max,
        pl_anchor = pl_anchor,
        pl_min = pl_min,
        sustain_track = TRUE,
        sustain_iso_code = sustain_iso_code,
        sustain_region = NULL,
        sustain_interval = 15,
        epochs = epochs,
        batch_size = batch_size,
        dir_checkpoint = dir_checkpoints,
        trace = FALSE,
        ml_trace = 0,
        log_dir = log_dir,
        log_write_interval = log_write_interval,
        Ns = Ns,
        Nq = Nq,
        loss_alpha = loss_alpha,
        loss_margin = loss_margin,
        sampling_separate=sampling_separate,
        sampling_shuffle=sampling_shuffle
      )
    }
    # Save
    save_to_disk(
      object = classifier,
      dir_path = destination_path,
      folder_name = folder_name
    )

    # Returns message
    return("Classifier trained.")
  })
}

get_arguments_extended_task_TEClassifierRegular <- function() {
  return(c(
    "destination_path",
    "folder_name",
    "path_to_embeddings",
    "path_to_target_data",
    "path_to_feature_extractor",
    "target_data_column",
    "name",
    "label",
    "data_folds",
    "data_val_size",
    "balance_class_weights",
    "balance_sequence_length",
    "use_sc",
    "sc_method",
    "sc_min_k",
    "sc_max_k",
    "use_pl",
    "pl_max_steps",
    "pl_max",
    "pl_anchor",
    "pl_min",
    "sustain_iso_code",
    "epochs",
    "batch_size",
    "log_dir",
    "dense_layers" ,
    "dense_size",
    "rec_layers" ,
    "rec_size",
    "rec_type",
    "rec_bidirectional",
    "self_attention_heads",
    "intermediate_size",
    "attention_type",
    "add_pos_embedding",
    "rec_dropout",
    "repeat_encoder",
    "dense_dropout",
    "recurrent_dropout",
    "encoder_dropout",
    "optimizer",
    "log_write_interval"
  ))
}

# Feature extractor --------------------------------------------------------------
long_feature_extractor <- function(name,
                                   label,
                                   features,
                                   method,
                                   noise_factor,
                                   optimizer,
                                   data_val_size,
                                   epochs,
                                   batch_size,
                                   sustain_iso_code,
                                   log_dir,
                                   log_write_interval,
                                   path_to_embeddings,
                                   destination_path,
                                   folder_name) {
  promises::future_promise({
    # Load data
    embeddings <- load_from_disk(path_to_embeddings)

    # Create dir for checkpints
    dir_destination <- paste0(
      destination_path, "/",
      folder_name
    )
    dir_checkpoints <- paste0(
      dir_destination, "/",
      "checkpoints"
    )

    create_dir(dir_destination, FALSE)
    create_dir(dir_checkpoints, FALSE)

    # Create
    feature_extractor <- TEFeatureExtractor$new()
    feature_extractor$configure(
      ml_framework = "pytorch",
      name = name,
      label = label,
      text_embeddings = embeddings,
      features = features,
      method = method,
      noise_factor = noise_factor,
      optimizer = optimizer
    )

    # Train
    feature_extractor$train(
      data_embeddings = embeddings,
      data_val_size = data_val_size,
      sustain_track = TRUE,
      sustain_iso_code = sustain_iso_code,
      sustain_region = NULL,
      sustain_interval = 15,
      epochs = epochs,
      batch_size = batch_size,
      dir_checkpoint = dir_checkpoints,
      trace = FALSE,
      ml_trace = 0,
      log_dir = log_dir,
      log_write_interval = log_write_interval
    )

    # Save
    save_to_disk(
      object = feature_extractor,
      dir_path = destination_path,
      folder_name = folder_name
    )

    # Returns message
    return("TEFeatureExtractor trained.")
  })
}

# Transformers =====================================================================

long_create_transformer <- function(transformer_type, params) {
  promises::future_promise({
    do.call(aife_transformer_maker$make(transformer_type)$create, params)

    # Returns message
    return("Transformer created.")
  })
}

long_train_transformer <- function(transformer_type) {
  promises::future_promise({})
}
