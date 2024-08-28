#' @title Function for creating a first draft of a vocabulary
#' @description This function creates a list of tokens which refer to specific universal part-of-speech tags (UPOS) and
#'   provides the corresponding lemmas.
#'
#' @param path_language_model `string` Path to a udpipe language model that should be used for tagging and
#'   lemmatization.
#' @param data `vector` containing the raw texts.
#' @param upos `vector` containing the universal part-of-speech tags which should be used to build the vocabulary.
#' @param label_language_model `string` Label for the udpipe language model used.
#' @param language `string` Name of the language (e.g., English, German)
#' @param chunk_size `int` Number of raw texts which should be processed at once.
#' @param trace `bool` `TRUE` if information about the progress should be printed to console.
#' @return `list` with the following components:
#'   * `vocab`: `data.frame` containing the tokens, lemmas, tokens in lower case, and lemmas in lower case.
#'   * `ud_language_model`: udpipe language model that is used for tagging.
#'   * `label_language_model`: Label of the udpipe language model.
#'   * `language`: Language of the raw texts.
#'   * `upos`: Used univerisal part-of-speech tags.
#'   * `n_sentence`: `int` Estimated number of sentences in the raw texts.
#'   * `n_token`: `int` Estimated number of tokens in the raw texts.
#'   * `n_document_segments`: `int` Estimated number of document segments/raw texts.
#'
#' @note A list of possible tags can be found here: <https://universaldependencies.org/u/pos/index.html>.
#' @note A huge number of models can be found here: <https://ufal.mff.cuni.cz/udpipe/2/models>.
#' @importFrom stats na.omit
#' @importFrom stringr str_length str_replace_all str_remove_all
#' @family Preparation
#' @export
bow_pp_create_vocab_draft <- function(path_language_model,
                                      data,
                                      upos = c("NOUN", "ADJ", "VERB"),
                                      label_language_model = NULL,
                                      language = NULL,
                                      chunk_size = 100,
                                      trace = TRUE) {
  requireNamespace(package = "udpipe")
  requireNamespace(package = "quanteda")

  # Phase 1: Analyze Texts with 'udpipe'
  n_document_segments <- length(data)
  n_sentence_init <- 0
  n_token_init <- 0
  n_iterations <- ceiling(n_document_segments / chunk_size)
  final_vocab_draft <- NULL

  ud_language_model <- udpipe::udpipe_load_model(file = path_language_model)

  for (i in 1:n_iterations) {
    selected_documents <- seq(
      from = 1 + (i - 1) * chunk_size,
      to = min(n_document_segments, chunk_size + (i - 1) * chunk_size)
    )

    if (trace == TRUE) {
      cat(paste(date(), "Processing chunk", i, "/", n_iterations))
    }
    tmp_text <- data[selected_documents]
    tmp_text <- stringr::str_replace_all(tmp_text,
      pattern = "/",
      replacement = " "
    )
    tmp_text <- stringr::str_remove_all(tmp_text,
      pattern = "-\\n"
    )
    tmp_text <- stringr::str_remove_all(tmp_text,
      pattern = ":"
    )


    ud_text_analysis <- udpipe::udpipe_annotate(ud_language_model,
      x = tmp_text,
      doc_id = selected_documents,
      trace = FALSE,
      tagger = "default",
      parser = "none"
    )

    ud_text_analysis <- as.data.frame(ud_text_analysis)
    ud_text_analysis$ID <- udpipe::unique_identifier(
      ud_text_analysis,
      fields = c("doc_id", "paragraph_id", "sentence_id")
    )
    tmp_n_sentence_init <- length(table(ud_text_analysis$ID))
    tmp_n_token_init <- nrow(ud_text_analysis)

    selection_token <- ud_text_analysis$upos %in% upos

    vocab <- subset(
      ud_text_analysis,
      selection_token
    )
    vocab_draft <- cbind(vocab$token, vocab$lemma)
    colnames(vocab_draft) <- c("token", "lemma")
    vocab_draft <- as.data.frame(vocab_draft)
    vocab_draft$lemma <- replace(vocab_draft$lemma,
      vocab_draft$lemma == "unknown",
      values = NA
    )
    vocab_draft$lemma <- replace(vocab_draft$lemma,
      stringr::str_length(vocab_draft$lemma) <= 2,
      values = NA
    )
    vocab_draft <- stats::na.omit(vocab_draft)
    vocab_draft <- unique(vocab_draft)

    if (is.null(final_vocab_draft) == TRUE) {
      final_vocab_draft <- vocab_draft
    } else {
      final_vocab_draft <- rbind(final_vocab_draft, vocab_draft)
      final_vocab_draft <- unique(final_vocab_draft)
    }
    n_sentence_init <- n_sentence_init + tmp_n_sentence_init
    n_token_init <- n_token_init + tmp_n_token_init
  }

  final_vocab_draft$token_tolower <- tolower(final_vocab_draft$token)
  final_vocab_draft$lemma_tolower <- tolower(final_vocab_draft$lemma)

  if (trace == TRUE) {
    cat(paste(date(), "Done"))
  }

  results <- NULL
  results["vocab"] <- list(final_vocab_draft)
  results["language_model"] <- list(ud_language_model)
  results["label_language_model"] <- list(label_language_model)
  results["language"] <- list(language)
  results["upos"] <- list(upos)
  results["n_sentence"] <- list(n_sentence_init)
  results["n_token"] <- list(n_token_init)
  results["n_document_segments"] <- list(n_document_segments)
  return(results)
}


#' @title Prepare texts for text embeddings with a bag of word approach
#' @description This function prepares raw texts for use with [TextEmbeddingModel].
#'
#' @param data `vector` containing the raw texts.
#' @param vocab_draft Object created with [bow_pp_create_vocab_draft].
#' @param remove_punct `bool` `TRUE` if punctuation should be removed.
#' @param remove_symbols `bool` `TRUE` if symbols should be removed.
#' @param remove_numbers `bool` `TRUE` if numbers should be removed.
#' @param remove_url `bool` `TRUE` if urls should be removed.
#' @param remove_separators `bool` `TRUE` if separators should be removed.
#' @param split_hyphens `bool` `TRUE` if hyphens should be split into several tokens.
#' @param split_tags `bool` `TRUE` if tags should be split.
#' @param use_lemmata `bool` `TRUE` lemmas instead of original tokens should be used.
#' @param to_lower `bool` `TRUE` if tokens or lemmas should be used with lower cases.
#' @param language_stopwords `string` Abbreviation for the language for which stopwords should be removed.
#' @param min_termfreq `int` Minimum frequency of a token to be part of the vocabulary.
#' @param min_docfreq `int` Minimum appearance of a token in documents to be part of the vocabulary.
#' @param max_docfreq `int` Maximum appearance of a token in documents to be part of the vocabulary.
#' @param window `int` size of the window for creating the feature-co-occurance matrix.
#' @param weights `vector` weights for the corresponding window. The vector length must be equal to the window size.
#' @param trace `bool` `TRUE` if information about the progress should be printed to console.
#' @return Returns a `list` of class `basic_text_rep` with the following components:
#'   * `dfm`: Document-Feature-Matrix. Rows correspond to the documents. Columns represent the number of tokens in the
#'     document.
#'
#'   * `fcm`: Feature-Co-Occurance-Matrix.
#'
#'   * `information`: `list` containing information about the used vocabulary. These are:
#'     - `n_sentence`: Number of sentences
#'     - `n_document_segments`: Number of document segments/raw texts
#'     - `n_token_init`: Number of initial tokens
#'     - `n_token_final`: Number of final tokens
#'     - `n_lemmata`: Number of lemmas
#'
#'   * `configuration`: `list` containing information if the vocabulary was created with lower cases and if the
#'     vocabulary uses original tokens or lemmas.
#'
#'   * `language_model` :`list` containing information about the applied language model. These are:
#'     - `model`: the udpipe language model
#'     - `label`: the label of the udpipe language model
#'     - `upos`: the applied universal part-of-speech tags
#'     - `language`: the language
#'     - `vocab`: a `data.frame` with the original vocabulary
#'
#' @importFrom stats na.omit
#' @family Preparation
#' @export
bow_pp_create_basic_text_rep <- function(data,
                                         vocab_draft,
                                         remove_punct = TRUE,
                                         remove_symbols = TRUE,
                                         remove_numbers = TRUE,
                                         remove_url = TRUE,
                                         remove_separators = TRUE,
                                         split_hyphens = FALSE,
                                         split_tags = FALSE,
                                         language_stopwords = "de",
                                         use_lemmata = FALSE,
                                         to_lower = FALSE,
                                         min_termfreq = NULL,
                                         min_docfreq = NULL,
                                         max_docfreq = NULL,
                                         window = 5,
                                         weights = 1 / (1:5),
                                         trace = TRUE) {
  requireNamespace(package = "quanteda")

  textual_corpus <- quanteda::corpus(data)
  token <- quanteda::tokens(textual_corpus,
    remove_punct = remove_punct,
    remove_symbols = remove_symbols,
    remove_numbers = remove_numbers,
    remove_url = remove_url,
    remove_separators = remove_separators,
    split_hyphens = split_hyphens,
    split_tags = split_tags,
    verbose = trace
  )
  if (use_lemmata == TRUE) {
    token <- quanteda::tokens_replace(
      x = token,
      pattern = vocab_draft$vocab$token,
      replacement = vocab_draft$vocab$lemma_tolower,
      valuetype = "fixed",
      verbose = trace
    )
  } else {
    token <- quanteda::tokens_replace(
      x = token,
      pattern = vocab_draft$vocab$token,
      replacement = vocab_draft$vocab$token,
      valuetype = "fixed",
      verbose = trace
    )
  }

  token <- quanteda::tokens_remove(
    x = token,
    pattern = quanteda::stopwords(language = language_stopwords)
  )

  if (to_lower == TRUE) {
    token <- quanteda::tokens_tolower(x = token)
  }

  dfm <- quanteda::dfm(token)
  dfm <- quanteda::dfm_trim(
    x = dfm,
    min_termfreq = min_termfreq,
    min_docfreq = min_docfreq,
    max_docfreq = max_docfreq,
    docfreq_type = "count"
  )

  if (use_lemmata == TRUE) {
    if (to_lower == TRUE) {
      vocab_intersect <- intersect(
        vocab_draft$vocab$token_tolower,
        colnames(dfm)
      )
      vocab <- subset(vocab_draft$vocab, vocab_draft$vocab$token_tolower %in% vocab_intersect)
    } else {
      vocab_intersect <- intersect(
        vocab_draft$vocab$lemma,
        colnames(dfm)
      )
      vocab <- subset(vocab_draft$vocab, vocab_draft$vocab$lemma %in% vocab_intersect)
    }
  } else {
    if (to_lower == TRUE) {
      vocab_intersect <- intersect(
        vocab_draft$vocab$lemma_tolower,
        colnames(dfm)
      )
      vocab <- subset(vocab_draft$vocab, vocab_draft$vocab$lemma_tolower %in% vocab_intersect)
    } else {
      vocab_intersect <- intersect(
        vocab_draft$vocab$token,
        colnames(dfm)
      )
      vocab <- subset(vocab_draft$vocab, vocab_draft$vocab$token %in% vocab_intersect)
    }
  }

  # Creating Indices------------------------------------------------------------
  # Token-----------------------------------------------------------------------
  vocab$index_token <- seq(
    from = 1,
    to = nrow(vocab),
    by = 1
  )
  # Lemmata---------------------------------------------------------------------
  tmp_lemmata <- unique(vocab$lemma)
  tmp_lemmata_index <- vector(length = nrow(vocab))
  for (i in 1:nrow(vocab)) {
    tmp_lemmata_index[i] <- match(
      x = vocab[i, "lemma"],
      table = tmp_lemmata
    )
  }
  vocab$index_lemma <- tmp_lemmata_index
  # Token to lower--------------------------------------------------------------
  tmp_token_lower <- unique(vocab$token_tolower)
  tmp_token_lower_index <- vector(length = nrow(vocab))
  for (i in 1:nrow(vocab)) {
    tmp_token_lower_index[i] <- match(
      x = vocab[i, "token_tolower"],
      table = tmp_token_lower
    )
  }
  vocab$index_token_lower <- tmp_token_lower_index
  # Lemmata to lower------------------------------------------------------------
  tmp_lemma_lower <- unique(vocab$lemma_tolower)
  tmp_lemma_lower_index <- vector(length = nrow(vocab))
  for (i in 1:nrow(vocab)) {
    tmp_lemma_lower_index[i] <- match(
      x = vocab[i, "lemma_tolower"],
      table = tmp_lemma_lower
    )
  }
  vocab$index_lemma_lower <- tmp_lemma_lower_index

  # Creating dfm and fcm--------------------------------------------------------
  dfm <- quanteda::dfm(token)
  fcm <- quanteda::fcm(token,
    context = "window",
    window = window,
    count = "weighted",
    weights = weights,
    tri = TRUE
  )
  if (use_lemmata == TRUE) {
    if (to_lower == TRUE) {
      dfm <- quanteda::dfm_keep(
        x = dfm,
        pattern = vocab$lemma_tolower,
        valuetype = "fixed",
        padding = FALSE
      )

      fcm <- quanteda::fcm_select(
        x = fcm,
        pattern = vocab$lemma_tolower,
        valuetype = "fixed",
        padding = FALSE
      )
    } else {
      dfm <- quanteda::dfm_keep(
        x = dfm,
        pattern = vocab$lemma,
        valuetype = "fixed",
        padding = FALSE
      )
      fcm <- quanteda::fcm_select(
        x = fcm,
        pattern = vocab$lemma,
        valuetype = "fixed",
        padding = FALSE
      )
    }
  } else {
    if (to_lower == TRUE) {
      dfm <- quanteda::dfm_keep(
        x = dfm,
        pattern = vocab$token_tolower,
        valuetype = "fixed",
        padding = FALSE
      )
      fcm <- quanteda::fcm_select(
        x = fcm,
        pattern = vocab$token_tolower,
        valuetype = "fixed",
        padding = FALSE
      )
    } else {
      dfm <- quanteda::dfm_keep(
        x = dfm,
        pattern = vocab$token,
        valuetype = "fixed",
        padding = FALSE
      )
      fcm <- quanteda::fcm_select(
        x = fcm,
        pattern = vocab$token,
        valuetype = "fixed",
        padding = FALSE
      )
    }
  }


  language_model <- NULL
  language_model["model"] <- list(vocab_draft$language_model)
  language_model["label"] <- list(vocab_draft$label_language_model)
  language_model["upos"] <- list(vocab_draft$upos)
  language_model["language"] <- list(vocab_draft$language)
  language_model["vocab"] <- list(vocab)

  configuration <- NULL
  configuration["to_lower"] <- list(to_lower)
  configuration["use_lemmata"] <- list(use_lemmata)

  information <- NULL
  information["n_sentence"] <- list(vocab_draft$n_sentence)
  information["n_document_segments"] <- list(vocab_draft$n_document_segments)
  information["n_token_init"] <- list(vocab_draft$n_token)
  information["n_token_final"] <- list(nrow(vocab))
  information["n_lemmata"] <- list(length(tmp_lemmata))

  results <- NULL
  results <- list(
    "dfm" = dfm,
    "fcm" = fcm,
    "information" = information,
    "language_model" = language_model,
    "configuration" = configuration
  )

  class(results) <- "basic_text_rep"
  return(results)
}
