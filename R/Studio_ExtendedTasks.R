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
                                      log_write_interval=2) {
  promises::future_promise({
    #Set up top level progress monitoring
    top_total=include_txt+include_txt+include_xlsx
    top_value=0
    total_message="File types"


    # Create new data set
    new_dataset <- LargeDataSetForText$new()

    #Start processing different file types
    if (include_txt == TRUE) {
      new_dataset$add_from_files_txt(
        dir_path = source_path,
        batch_size = 2,
        log_file = log_path,
        log_top_value=top_value,
        log_top_total=top_total,
        log_top_message=total_message,
        log_write_interval=log_write_interval,
        trace = FALSE
      )
      top_value=top_value+1
    }

    if (include_pdf == TRUE) {
      new_dataset$add_from_files_pdf(
        dir_path = source_path,
        batch_size = 2,
        log_file = log_path,
        top_value=top_value,
        top_total=top_total,
        top_message=total_message,
        log_write_interval=log_write_interval,
        trace = FALSE
      )
      top_value=top_value+1
    }

    if (include_xlsx == TRUE) {
      new_dataset$add_from_files_xlsx(
        dir_path = source_path,
        trace = FALSE,
        id_column = excel_id_column,
        text_column = excel_text_column,
        license_column = excel_license_column,
        bib_entry_column = excel_bib_entry_column,
        log_file = log_path,
        top_value=top_value,
        top_total=top_total,
        top_message=total_message,
        log_write_interval=log_write_interval,
      )
      top_value=top_value+1
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

long_transform_text_to_embeddings <- function(source_path,
                                      destination_path,
                                      destination_folder,
                                      log_path,
                                      batch_size,
                                      model_path,
                                      log_write_interval=2) {

  promises::future_promise({
    #Read the large data set for raw texts
    raw_texts=load_from_disk(source_path)

    #Set up top level progress monitoring
    top_total=raw_texts$n_rows()
    top_value=0
    total_message="Documents"

    #Load the model
    model=load_from_disk(model_path)

    #Start embedding
    embeddings=model$embed_large(
      large_datas_set=raw_texts,
      batch_size=batch_size,
      trace=FALSE,
      log_file = log_path,
      log_write_interval = log_write_interval)

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
