#'Function for creating a first draft of a vocabulary
#'
#'This function creates a list of tokens which refer to specific
#'universal part-of-speech tags (UPOS) and provides the corresponding lemmas.
#'
#'@param path_language_model \code{string} Path to a udpipe language model that
#'should be used for tagging and lemmatisation.
#'@param data \code{vector} containing the raw texts.
#'@param upos \code{vector} containing the universal part-of-speech tags which
#'should be used to build the vocabulary.
#'@param label_language_model \code{string} Label for the udpipe language model used.
#'@param language \code{string} Name of the language (e.g., English, German)
#'@param chunk_size \code{int} Number of raw texts which should be processed at one time.
#'@param trace \code{bool} \code{TRUE} if information about the progress should be printed to console.
#'@return \code{list} with the following components.
#'\itemize{
#'\item{\code{vocab}}{\code{data.frame} containing the tokens, lemmas, tokens in lower case, and
#'lemmas in lower case.}
#'\item{\code{language_model}}{\code{}}
#'\item{\code{ud_language_model}}{udpipe language model that is used for tagging.}
#'\item{\code{label_language_model}}{Label of the udpipe language model.}
#'\item{\code{language}}{Language of the raw texts.}
#'\item{\code{upos}}{Used univerisal part-of-speech tags.}
#'\item{\code{n_sentence}}{\code{int} Estimated number of sentence in the raw texts.}
#'\item{\code{n_token}}{\code{int} Estimated number of tokens in the raw texts.}
#'\item{\code{n_document_segments}}{\code{int} Estimated number of document segments/raw texts.}
#'}
#'@note A list of possible tags can be found
#'here: \url{https://universaldependencies.org/u/pos/index.html}.
#'@note A huge number of models can be found
#'here: \url{https://ufal.mff.cuni.cz/udpipe/2/models}.
#'@importFrom udpipe udpipe_load_model udpipe_annotate unique_identifier
#'@importFrom stats na.omit
#'@importFrom stringr str_length
#'@export
bow_pp_create_vocab_draft<-function(path_language_model,
                                    data,
                                    upos=c("NOUN", "ADJ","VERB"),
                                    label_language_model=NULL,
                                    language=NULL,
                                    chunk_size=100,
                                    trace=TRUE)
{
  #Phase 1: Analyse der Texte mit udpipe
  n_document_segments<-length(data)
  n_sentence_init<-0
  n_token_init<-0
  n_iterations<-ceiling(n_document_segments/chunk_size)
  final_vocab_draft=NULL

  ud_language_model<-udpipe::udpipe_load_model(file = path_language_model)

  for(i in 1:n_iterations){
    selected_documents<-seq(
      from=1+(i-1)*chunk_size,
      to=min(n_document_segments,chunk_size+(i-1)*chunk_size))

    if(trace==TRUE){
      print(paste(date(),"Processing chunk",i,"/",n_iterations))
    }
    tmp_text<-data[selected_documents]
    tmp_text<-stringr::str_replace_all(tmp_text,
                                      pattern = "/",
                                      replacement = " ")
    tmp_text<-stringr::str_remove_all(tmp_text,
                                      pattern = "-\\n")
    tmp_text<-stringr::str_remove_all(tmp_text,
                                      pattern = ":")


    ud_text_analysis<-udpipe::udpipe_annotate(ud_language_model,
                                              x=tmp_text,
                                              doc_id = selected_documents,
                                              trace = FALSE,
                                              tagger="default",
                                              parser="none")

    ud_text_analysis<-as.data.frame(ud_text_analysis)
    ud_text_analysis$ID<-udpipe::unique_identifier(
      ud_text_analysis,
      fields = c("doc_id", "paragraph_id", "sentence_id"))
    tmp_n_sentence_init<-length(table(ud_text_analysis$ID))
    tmp_n_token_init<-nrow(ud_text_analysis)

    selection_token<-ud_text_analysis$upos %in% upos

    vocab<-subset(ud_text_analysis,
                  selection_token)
    vocab_draft<-cbind(vocab$token,vocab$lemma)
    colnames(vocab_draft)<-c("token","lemma")
    vocab_draft<-as.data.frame(vocab_draft)
    vocab_draft$lemma<-replace(vocab_draft$lemma,
                               vocab_draft$lemma=="unknown",
                               values=NA)
    vocab_draft$lemma<-replace(vocab_draft$lemma,
                               stringr::str_length(vocab_draft$lemma)<=2,
                               values=NA)
    vocab_draft<-stats::na.omit(vocab_draft)
    vocab_draft<-unique(vocab_draft)

    if(is.null(final_vocab_draft)==TRUE){
      final_vocab_draft<-vocab_draft
    } else {
      final_vocab_draft<-rbind(final_vocab_draft,vocab_draft)
      final_vocab_draft<-unique(final_vocab_draft)
    }
    n_sentence_init<-n_sentence_init+tmp_n_sentence_init
    n_token_init<-n_token_init+tmp_n_token_init
  }

  final_vocab_draft$token_tolower<-tolower(final_vocab_draft$token)
  final_vocab_draft$lemma_tolower<-tolower(final_vocab_draft$lemma)

  if(trace==TRUE){
    print(paste(date(),"Done"))
  }

  results<-NULL
  results["vocab"]<-list(final_vocab_draft)
  results["language_model"]<-list(ud_language_model)
  results["label_language_model"]<-list(label_language_model)
  results["language"]<-list(language)
  results["upos"]<-list(upos)
  results["n_sentence"]<-list(n_sentence_init)
  results["n_token"]<-list(n_token_init)
  results["n_document_segments"]<-list(n_document_segments)
  return(results)
}


#'Prepare texts for text embeddings with a bag of word approach.
#'
#'This function prepares raw texts for use with \link{TextEmbeddingModel}.
#'
#'@param data \code{vector} containing the raw texts.
#'@param vocab_draft Object created with \link{bow_pp_create_vocab_draft}.
#'@param remove_punct \code{bool} \code{TRUE} if punctuation should be removed.
#'@param remove_symbols \code{bool} \code{TRUE} if symbols should be removed.
#'@param remove_numbers \code{bool} \code{TRUE} if numbers should be removed.
#'@param remove_url \code{bool} \code{TRUE} if urls should be removed.
#'@param remove_separators \code{bool} \code{TRUE} if separators should be removed.
#'@param split_hyphens \code{bool} \code{TRUE} if hyphens should be splitted into several tokens.
#'@param split_tags \code{bool} \code{TRUE} if tags should be splitted.
#'@param use_lemmata \code{bool} \code{TRUE} lemmas instead of original tokens should be used.
#'@param to_lower \code{bool} \code{TRUE} if token or lemmas should be used with lower cases.
#'
#'@param min_termfreq \code{int} Minimum frequency of a token to be part of the vocabulary.
#'@param min_docfreq \code{int} Minimum appearance of a token in documents to be part of the vocabulary.
#'@param max_docfreq \code{int} Maximum appearance of a token in documents to be part of the vocabulary.
#'@param window \code{int} size of the window for creating the feature co-occurance matrix.
#'@param weights \code{vector} weights for the corresponding window. The vector length must be equal to window's size.
#'@param trace \code{bool} \code{TRUE} if information about the progress should
#'printed to console.
#'@return Returns a \code{list} of class \code{basic_text_rep} with the following components.
#'\itemize{
#'\item{\code{dfm: }}{Document-Feature-Matrix. Rows correspond to the documents. Columns represent
#'the number of tokens in the document.}
#'
#'\item{\code{fcm: }}{Feature-Co-Occurance-Matrix.}
#'
#'\item{\code{information: }}{\code{list} containing information about the used vocabulary. These are:
#'  \itemize{
#'  \item{\code{n_sentence: }} {Number of sentence}
#'  \item{\code{n_document_segments: }} {Number of document segments/raw texts}
#'  \item{\code{n_token_init: }} {Number of initial tokens}
#'  \item{\code{n_token_final: }} {Number of final tokens}
#'  \item{\code{n_lemmata: }} {Number of lemmas}
#'   }}
#'
#'\item{\code{configuration: }}{\code{list} containing information if the vocabulary was
#'created with lower cases and if the vocabulary uses original tokens or lemmas.}
#'
#'\item{\code{language_model: }}{\code{list} containing information about the applied
#'language model. These are:
#'\itemize{
#'\item{\code{model: }} {the udpipe language model}
#'\item{\code{label: }} {the label of the udpipe language model}
#'\item{\code{upos: }} {the applied universal part-of-speech tags}
#'\item{\code{language: }} {the language}
#'\item{\code{vocab: }} {a \code{data.frame} with the original vocabulary}
#'}}
#'
#'}
#'
#'@importFrom quanteda corpus tokens tokens_replace tokens_remove tokens_tolower
#'@importFrom quanteda dfm dfm_trim fcm dfm_keep fcm_select
#'@importFrom stats na.omit
#'@export
bow_pp_create_basic_text_rep<-function(data,
                                      vocab_draft,
                                      remove_punct = TRUE,
                                      remove_symbols = TRUE,
                                      remove_numbers = TRUE,
                                      remove_url = TRUE,
                                      remove_separators = TRUE,
                                      split_hyphens = FALSE,
                                      split_tags = FALSE,
                                      language_stopwords="de",
                                      use_lemmata = FALSE,
                                      to_lower=FALSE,
                                      min_termfreq = NULL,
                                      min_docfreq= NULL,
                                      max_docfreq=NULL,
                                      window = 5,
                                      weights = 1 / (1:5),
                                      trace=TRUE)
{
  textual_corpus <-quanteda::corpus(data)
  token<-quanteda::tokens(textual_corpus,
                          remove_punct = remove_punct,
                          remove_symbols = remove_symbols,
                          remove_numbers = remove_numbers,
                          remove_url = remove_url,
                          remove_separators = remove_separators,
                          split_hyphens = split_hyphens,
                          split_tags = split_tags,
                          verbose = trace)
  if(use_lemmata==TRUE){
    token<-quanteda::tokens_replace(x=token,
                                    pattern = vocab_draft$vocab$token,
                                    replacement = vocab_draft$vocab$lemma_tolower,
                                    valuetype = "fixed",
                                    verbose=trace)
  } else {
    token<-quanteda::tokens_replace(x=token,
                                    pattern = vocab_draft$vocab$token,
                                    replacement = vocab_draft$vocab$token,
                                    valuetype = "fixed",
                                    verbose=trace)
  }

  token<-quanteda::tokens_remove(x=token,
                                 pattern=quanteda::stopwords(language =language_stopwords)
                                )

  if(to_lower==TRUE){
    token<-quanteda::tokens_tolower(x=token)
  }

  dfm<-quanteda::dfm(token)
  dfm<-quanteda::dfm_trim(
    x=dfm,
    min_termfreq = min_termfreq,
    min_docfreq = min_docfreq,
    max_docfreq =  max_docfreq,
    docfreq_type = "count"
  )

  if(use_lemmata==TRUE){
    if(to_lower==TRUE){
      vocab_intersect=intersect(vocab_draft$vocab$token_tolower,
                      colnames(dfm))
      vocab=subset(vocab_draft$vocab,vocab_draft$vocab$token_tolower %in% vocab_intersect)
    } else {
      vocab_intersect=intersect(vocab_draft$vocab$lemma,
                                colnames(dfm))
      vocab=subset(vocab_draft$vocab,vocab_draft$vocab$lemma %in% vocab_intersect)
    }
  } else {
    if(to_lower==TRUE){
      vocab_intersect=intersect(vocab_draft$vocab$lemma_tolower,
                                colnames(dfm))
      vocab=subset(vocab_draft$vocab,vocab_draft$vocab$lemma_tolower %in% vocab_intersect)
    } else {
      vocab_intersect=intersect(vocab_draft$vocab$token,
                                colnames(dfm))
      vocab=subset(vocab_draft$vocab,vocab_draft$vocab$token %in% vocab_intersect)
    }
  }

  #Creating Indices------------------------------------------------------------
  #Token-----------------------------------------------------------------------
  vocab$index_token<-seq(from=1,
                         to=nrow(vocab),
                         by=1)
  #Lemmata---------------------------------------------------------------------
  tmp_lemmata<-unique(vocab$lemma)
  tmp_lemmata_index<-vector(length = nrow(vocab))
  for(i in 1:nrow(vocab)){
    tmp_lemmata_index[i]<-match(x = vocab[i,"lemma"],
                                table = tmp_lemmata)
  }
  vocab$index_lemma<-tmp_lemmata_index
  #Token to lower--------------------------------------------------------------
  tmp_token_lower<-unique(vocab$token_tolower)
  tmp_token_lower_index<-vector(length = nrow(vocab))
  for(i in 1:nrow(vocab)){
    tmp_token_lower_index[i]<-match(x = vocab[i,"token_tolower"],
                                table = tmp_token_lower)
  }
  vocab$index_token_lower<-tmp_token_lower_index
  #Lemmata to lower------------------------------------------------------------
  tmp_lemma_lower<-unique(vocab$lemma_tolower)
  tmp_lemma_lower_index<-vector(length = nrow(vocab))
  for(i in 1:nrow(vocab)){
    tmp_lemma_lower_index[i]<-match(x = vocab[i,"lemma_tolower"],
                                    table = tmp_lemma_lower)
  }
  vocab$index_lemma_lower<-tmp_lemma_lower_index

  #Creating dfm and fcm--------------------------------------------------------
  dfm<-quanteda::dfm(token)
  fcm<-quanteda::fcm(token,
                     context = "window",
                     window=window,
                     count = "weighted",
                     weights = weights,
                     tri = TRUE)
  if(use_lemmata==TRUE){
    if(to_lower==TRUE){
      dfm<-quanteda::dfm_keep(x=dfm,
                              pattern = vocab$lemma_tolower,
                              valuetype = "fixed",
                              padding = FALSE)

      fcm<-quanteda::fcm_select(x=fcm,
                              pattern = vocab$lemma_tolower,
                              valuetype = "fixed",
                              padding = FALSE)
    } else {
      dfm<-quanteda::dfm_keep(x=dfm,
                              pattern = vocab$lemma,
                              valuetype = "fixed",
                              padding = FALSE)
      fcm<-quanteda::fcm_select(x=fcm,
                              pattern = vocab$lemma,
                              valuetype = "fixed",
                              padding = FALSE)
    }
  } else {
      if(to_lower==TRUE){
        dfm<-quanteda::dfm_keep(x=dfm,
                                pattern = vocab$token_tolower,
                                valuetype = "fixed",
                                padding = FALSE)
        fcm<-quanteda::fcm_select(x=fcm,
                                pattern = vocab$token_tolower,
                                valuetype = "fixed",
                                padding = FALSE)
      } else {
        dfm<-quanteda::dfm_keep(x=dfm,
                                pattern = vocab$token,
                                valuetype = "fixed",
                                padding = FALSE)
        fcm<-quanteda::fcm_select(x=fcm,
                                pattern = vocab$token,
                                valuetype = "fixed",
                                padding = FALSE)
      }
  }


  language_model<-NULL
  language_model["model"]<-list(vocab_draft$language_model)
  language_model["label"]<-list(vocab_draft$label_language_model)
  language_model["upos"]<-list(vocab_draft$upos)
  language_model["language"]<-list(vocab_draft$language)
  language_model["vocab"]<-list(vocab)

  configuration<-NULL
  configuration["to_lower"]<-list(to_lower)
  configuration["use_lemmata"]<-list(use_lemmata)

  information<-NULL
  information["n_sentence"]<-list(vocab_draft$n_sentence)
  information["n_document_segments"]<-list(vocab_draft$n_document_segments)
  information["n_token_init"]<-list(vocab_draft$n_token)
  information["n_token_final"]<-list(nrow(vocab))
  information["n_lemmata"]<-list(length(tmp_lemmata))

  results<-NULL
  results<-list(
    "dfm"=dfm,
    "fcm"=fcm,
    "information"=information,
    "language_model"=language_model,
    "configuration"=configuration
  )

  class(results)<-"basic_text_rep"
  return(results)
}



