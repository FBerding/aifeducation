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
                                      excel_url_license_column,
                                      excel_text_license_column,
                                      excel_url_source_column,
                                      log_write_interval = 2,
                                      current_conda_env) {
  promises::future_promise({
    # Set up conda env
    reticulate::use_condaenv(condaenv = current_conda_env)

    # Set up top level progress monitoring
    top_total <- include_txt + include_pdf + include_xlsx
    top_value <- 0
    total_message <- "File types"

    # Create new data set
    new_dataset <- LargeDataSetForText$new()

    # Start processing different file types
    if (include_txt) {
      top_value <- top_value + 1

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
    }

    if (include_pdf) {
      top_value <- top_value + 1

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
    }

    if (include_xlsx) {
      top_value <- top_value + 1

      new_dataset$add_from_files_xlsx(
        dir_path = source_path,
        trace = FALSE,
        id_column = excel_id_column,
        text_column = excel_text_column,
        license_column = excel_license_column,
        bib_entry_column = excel_bib_entry_column,
        url_license_column = excel_url_license_column,
        text_license_column = excel_text_license_column,
        url_source_column = excel_url_source_column,
        log_file = log_path,
        log_top_value = top_value,
        log_top_total = top_total,
        log_top_message = total_message,
        log_write_interval = log_write_interval
      )
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
                                              log_write_interval = 2,
                                              current_conda_env) {
  promises::future_promise({
    # Set up conda env
    reticulate::use_condaenv(condaenv = current_conda_env)

    # Read the large data set for raw texts
    raw_texts <- load_from_disk(source_path)

    # Set up top level progress monitoring
    # top_total <- raw_texts$n_rows()
    # top_value <- 0
    # total_message <- "Documents"

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

# Classifiers ==================================================================

long_models <- function(args) {
  promises::future_promise({

    # Set up py env
    prepare_python(
      env_type=args$configure$meta_args$py_environment_type,
      envname=args$configure$meta_args$py_env_name
    )

    #Create object
    object=create_object(args$configure$meta_args$object_class)

    requested_methods=names(args)
    for(method in requested_methods){
      # add missing objects to arguments by loading them
      args[[method]]$args=add_missing_args(
        args=args[[method]]$args,
        path_args=args[[method]]$path_args,
        meta_args=args[[method]]$meta_args
      )

      #Call Method
      do.call(what = object[[method]],args=args[[method]]$args)
    }

    # Create dir for saving the object
    dir_destination <- paste0(
      args$configure$path_args$destination_path, "/",
      args$configure$path_args$folder_name
    )
    create_dir(dir_destination, FALSE)

    # Save
    save_to_disk(
      object = object,
      dir_path = args[[1]]$path_args$destination_path,
      folder_name = args[[1]]$path_args$folder_name
    )

    # Returns message
    return("Model trained.")
  })
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
                                   folder_name,
                                   current_conda_env) {
  promises::future_promise({
    # Set up conda env
    reticulate::use_condaenv(condaenv = current_conda_env)

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

long_create_transformer <- function(transformer_type, dataset_dir_path, params,
                                    current_conda_env) {
  promises::future_promise({
    # Set up conda env
    reticulate::use_condaenv(condaenv = current_conda_env)

    text_dataset <- LargeDataSetForText$new()
    text_dataset$load_from_disk(dataset_dir_path)

    params[["text_dataset"]] <- text_dataset
    do.call(aife_transformer_maker$make(transformer_type)$create, params)

    # Returns message
    return("Transformer created.")
  })
}

long_train_transformer <- function(transformer_type, dataset_dir_path, params,
                                   current_conda_env) {
  promises::future_promise({
    # Set up conda env
    reticulate::use_condaenv(condaenv = current_conda_env)

    text_dataset <- LargeDataSetForText$new()
    text_dataset$load_from_disk(dataset_dir_path)

    params[["text_dataset"]] <- text_dataset
    do.call(aife_transformer_maker$make(transformer_type)$train, params)

    # Returns message
    return("Transformer trained.")
  })
}
