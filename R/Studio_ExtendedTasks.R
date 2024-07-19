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
                                      excel_bib_entry_column) {
  promises::future_promise({
    # Create new data set
    new_dataset <- LargeDataSetForText$new()

    if (include_txt == TRUE) {
      new_dataset$add_from_files_txt(
        dir_path = source_path,
        batch_size = 2,
        log_file = log_path,
        trace = FALSE
      )
    }

    if (include_pdf == TRUE) {
      new_dataset$add_from_files_pdf(
        dir_path = source_path,
        batch_size = 2,
        trace = FALSE
      )
    }

    if (include_xlsx == TRUE) {
      new_dataset$add_from_files_xlsx(
        dir_path = source_path,
        trace = FALSE,
        id_column = excel_id_column,
        text_column = excel_text_column,
        license_column = excel_license_column,
        bib_entry_column = excel_bib_entry_column
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
