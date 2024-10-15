#' @title Graphical user interface for encode, decode and tokenize raw texts.
#' @description Functions generates the tab within a page for using an object of class [TextEmbeddingModel] for encode,
#'   decode and tokenize a raw text.
#'
#' @param id `string` determining the id for the namespace.
#' @return This function does nothing return. It is used to build a page for a shiny app.
#'
#' @family studio_gui_text_embedding_model_tokenize_encode_decode
#' @keywords internal
#'
Tokenize_Encode_Decode_UI <- function(id) {
  bslib::page(
    bslib::card(
      bslib::card_header(
        "Encode"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          shiny::uiOutput(outputId = shiny::NS(id, "token_table")),
          shiny::tableOutput(outputId = shiny::NS(id, "tokenizer_statistics"))
        ),
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header(
              "Raw Text"
            ),
            bslib::card_body(
              shiny::textAreaInput(
                inputId = shiny::NS(id, "text_for_encode"),
                label = NULL,
                rows = 5,
                width = "100%"
              ),
              shiny::actionButton(
                inputId = shiny::NS(id, "encode_start"),
                label = "Encode",
                width = "100%",
                icon = shiny::icon("paper-plane")
              ),
              shiny::actionButton(
                inputId = shiny::NS(id, "encode_clear"),
                label = "Clear",
                width = "100%",
                icon = shiny::icon("trash")
              )
            )
          ),
          bslib::card(
            bslib::card_header(
              "Token Sequence"
            ),
            bslib::card_body(
              shiny::uiOutput(outputId = shiny::NS(id, "txt_to_tokens"))
            )
          ),
          bslib::card(
            bslib::card_header(
              "ID Sequence"
            ),
            bslib::card_body(
              shiny::uiOutput(outputId = shiny::NS(id, "txt_to_int"))
            )
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header(
        "Decode"
      ),
      bslib::card_body(
        bslib::layout_column_wrap(
          bslib::card(
            bslib::card_header(
              "ID Sequence"
            ),
            bslib::card_body(
              shiny::textAreaInput(
                inputId = shiny::NS(id, "ids_for_decode"),
                label = NULL,
                rows = 5,
                width = "100%"
              ),
              shiny::actionButton(
                inputId = shiny::NS(id, "decode_start"),
                label = "Decode",
                width = "100%",
                icon = shiny::icon("paper-plane")
              ),
              shiny::actionButton(
                inputId = shiny::NS(id, "decode_clear"),
                label = "Clear",
                width = "100%",
                icon = shiny::icon("trash")
              )
            )
          ),
          bslib::card(
            bslib::card_header(
              "Token Sequence"
            ),
            bslib::card_body(
              shiny::uiOutput(outputId = shiny::NS(id, "ids_to_tokens"))
            )
          ),
          bslib::card(
            bslib::card_header(
              "Raw Text"
            ),
            bslib::card_body(
              shiny::uiOutput(outputId = shiny::NS(id, "ids_to_txt"))
            )
          )
        )
      )
    )
  )
}

#' @title Server function for: graphical user interface for encode, decode and tokenize raw texts.
#' @description Functions generates the functionality of a page on the server.
#'
#' @param id `string` determining the id for the namespace.
#' @param model Model used for inference.
#' @return This function does nothing return. It is used to create the functionality of a page for a shiny app.
#'
#' @family studio_gui_text_embedding_model_tokenize_encode_decode
#' @keywords internal
#'
Tokenize_Encode_Decode_Server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    # global variables-----------------------------------------------------------
    # TODO (Yuliia): Remove? Variable is not used
    ns <- session$ns

    # Render Token table--------------------------------------------------------
    output$token_table <- shiny::renderTable({
      shiny::req(model)
      model()$get_special_tokens()
    })

    #Render Tokenizer Statistics-----------------------------------------------
    output$tokenizer_statistics<-shiny::renderTable({
      shiny::req(model)
       return(model()$tokenizer_statistics)
    })

    # Encode-------------------------------------------------------------------
    # Calculate encodings
    encodings <- shiny::eventReactive(input$encode_start, {
      shiny::req(model)

      integer_sequence <- model()$encode(
        raw_text = input$text_for_encode,
        token_encodings_only = TRUE,
        to_int = TRUE,
        trace = FALSE
      )[[1]]

      integer_output <- NULL
      for (i in 1:length(integer_sequence)) {
        tmp_sequence <- paste(integer_sequence[[i]], collapse = " ")
        integer_output[length(integer_output) + 1] <- list(shiny::tags$p(shiny::tags$b(paste("Chunk", i))))
        integer_output[length(integer_output) + 1] <- list(shiny::tags$p(tmp_sequence))
      }

      token_sequence <- model()$encode(
        raw_text = input$text_for_encode,
        token_encodings_only = TRUE,
        to_int = FALSE,
        trace = FALSE
      )[[1]]

      token_output <- NULL
      for (i in 1:length(token_sequence)) {
        tmp_sequence <- paste(token_sequence[[i]], collapse = " ")
        token_output[length(token_output) + 1] <- list(shiny::tags$p(shiny::tags$b(paste("Chunk", i))))
        token_output[length(token_output) + 1] <- list(shiny::tags$p(tmp_sequence))
      }

      return(list(
        integer_encodings = integer_output,
        token_encodings = token_output
      ))
    })

    # Display encodings
    output$txt_to_int <- shiny::renderUI(encodings()$integer_encodings)
    output$txt_to_tokens <- shiny::renderUI(encodings()$token_encodings)

    # Clear display for encodings
    shiny::observeEvent(input$encode_clear, {
      shiny::updateTextAreaInput(inputId = "text_for_encode", value = "")
      output$txt_to_int <- shiny::renderUI(NULL)
      output$txt_to_tokens <- shiny::renderUI(NULL)
    })

    # Decode----------------------------------------------------------------------
    # Calculate decodings
    decodings <- shiny::eventReactive(input$decode_start, {
      shiny::req(model)

      #int_sequence <- stringr::str_extract_all(input$ids_for_decode, "\\d+")
      int_sequence <- stringi::stri_extract_all_regex(str=input$ids_for_decode, pattern="\\d+")

      output_list_text <- model()$decode(int_sequence, to_token = FALSE)

      output_list_token <- model()$decode(int_sequence, to_token = TRUE)

      text_list <- NULL
      token_list <- NULL

      for (i in 1:length(output_list_text)) {
        text_list[length(text_list) + 1] <- list(shiny::tags$p(paste("Chunk", i)))
        text_list[length(text_list) + 1] <- list(shiny::tags$p(output_list_text[[i]]))

        token_list[length(token_list) + 1] <- list(shiny::tags$p(paste("Chunk", i)))
        token_list[length(token_list) + 1] <- list(shiny::tags$p(output_list_token[[i]]))
      }

      return(list(
        text_decode = text_list,
        token_decode = token_list
      ))
    })

    # Display decodings
    output$ids_to_txt <- shiny::renderUI(decodings()$text_decode)
    output$ids_to_tokens <- shiny::renderUI(decodings()$token_decode)

    # Clear display for decodings
    shiny::observeEvent(input$decode_clear, {
      shiny::updateTextAreaInput(inputId = "ids_for_decode", value = "")
      output$ids_to_txt <- shiny::renderUI(NULL)
      output$ids_to_tokens <- shiny::renderUI(NULL)
    })
  })
}
