#'@title Text embedding model
#'@description This \link[R6]{R6} class stores a text embedding model which can be
#'used to tokenize, encode, decode, and embed raw texts. The object provides a
#'unique interface for different text processing methods.
#'@export
TextEmbeddingModel<-R6::R6Class(
  classname = "TextEmbeddingModel",
  private = list(
    model_info=list(
      model_license=NA,
      model_name=NA,
      model_label=NA,
      model_date=NA,
      model_version=NA,
      model_language=NA
    ),
    publication_info=list(
      developed_by=list(
        authors=NULL,
        citation=NULL,
        url=NULL
      ),
      modified_by=list(
        authors=NULL,
        citation=NULL,
        url=NULL
      )
    ),
    model_description=list(
      eng=NULL,
      native=NULL,
      abstract_eng=NULL,
      abstract_native=NULL,
      keywords_eng=NULL,
      keywords_native=NULL
    )
  ),
  public = list(
    #'@field basic_components ('list()')\cr
    #'List storing information which can apply to all methods.
    #'\itemize{
    #'\item{\code{basic_components$method: }}{Method underlying the text embedding model.}
    #'\item{\code{basic_components$max_length: }}{Maximum number of tokens in the sequence the model processes. In general,
    #'shorter sequences will be padded and longer sequences will be divided
    #'into chunks and/or truncated.}
    #'}
    basic_components=list(
      method=NULL,
      max_length=NULL
    ),

    #'@field transformer_components ('list()')\cr
    #'List storing information which only apply to BERT models.
    #'\itemize{
    #'\item{\code{transformer_components$model: }}{An object of class transformers.TFBertModel for using with transformers library.}
    #'\item{\code{transformer_components$tokenizer: }}{An object of class transformers.BertTokenizerFast for using with transformers library.}
    #'\item{\code{transformer_components$aggregation: }}{Aggregation method for the hidden states.}
    #'\item{\code{transformer_components$chunks: }}{Maximal number of chunks processed with the model.}
    #'\item{\code{ transformer_components$overlap: }}{Number of tokens which should be added at the beginning of the sequence
    #'of the next chunk.}
    #'}
    transformer_components=list(
      model=NULL,
      tokenizer=NULL,
      aggregation=NULL,
      chunks=NULL,
      overlap=NULL),

    #'@field bow_components ('list()')\cr
    #'List storing information which only apply to bow_models.
    #'\itemize{
    #'\item{\code{bow_components$model: }}{data.frame describing the relationship between tokens and their corresponding
    #'text embeddings.}
    #'\item{\code{bow_components$vocab: }}{data.frame saving tokens, lemmas and their corresponding integer index.}
    #'\item{\code{bow_components$configuration: }}{List of the configuration parameters of the model.
    #'\itemize{
    #'\item{\code{bow_components$configuration$to_lower }}{\code{TRUE} if tokens are transformed to lower case.}
    #'\item{\code{bow_components$configuration$use_lemmata }}{\code{TRUE} if the corresponding lemma should be used instead of the token.}
    #'\item{\code{bow_components$configuration$bow_n_dim }}{Number of dimensions for GlobalVectors and Topic Modeling.}
    #'\item{\code{bow_components$configuration$bow_n_cluster }}{Number of clusters for grouping tokens based in their global vectors.
    #'Does not apply to method lda.}
    #'\item{\code{bow_components$configuration$bow_max_iter }}{Maximum number of iterations to calculate global vectors and topics.}
    #'\item{\code{bow_components$configuration$bow_max_iter_cluster }}{Maximum number of iterations for creating clusters. Applies only to method
    #'glove.}
    #'\item{\code{bow_components$configuration$bow_cr_criterion }}{Convergence criterion for calculating global vectors and topics.}
    #'\item{\code{bow_components$configuration$bow_learning_rate  }}{Initial learning rate for estimating global vectors.}}
    #'}
    #'\item{\code{bow_components$aggregation: }}{Does currently not apply to these methods.}
    #'\item{\code{bow_components$chunks: }}{Does currently not apply to these methods.}
    #'\item{\code{bow_components$overlap: }}{Does currently not apply to these methods.}
    #'}
    bow_components=list(
      model=NULL,
      vocab=NULL,
      configuration=list(
        to_lower = NA,
        use_lemmata = NA,
        bow_n_dim = NA,
        bow_n_cluster = NA,
        bow_max_iter = NA,
        bow_max_iter_cluster = NA,
        bow_cr_criterion = NA,
        bow_learning_rate = NA
        ),
      aggregation="none",
      chunks="none",
      overlap="none"
      ),

    #--------------------------------------------------------------------------
    #'@description Method for creating a new text embedding model
    #'@param model_name \code{string} containing the name of the new model.
    #'@param model_label \code{string} containing the label/title of the new model.
    #'@param model_version \code{string} version of the model.
    #'@param model_language \code{string} containing the language which the model
    #'represents (e.g., English).
    #'@param method \code{string} determining the kind of embedding model. Currently
    #'three types are supported. \code{method="bert"} for Bidirectional Encoder
    #'Representations from Transformers (BERT), \code{method="glove"} for
    #'GlobalVector Clusters, and \code{method="lda"} for topic modeling. See
    #'details for more information.
    #'@param max_length \code{int} determining the maximum length of token
    #'sequences uses in BERT models. Not relevant for the other methods.
    #'@param chunks \code{int} Maximum number of chunks. Only relevant for
    #'BERT models.
    #'@param overlap \code{int} determining the number of tokens which should be added
    #'at the beginning of the next chunk. Only relevant for BERT models.
    #'@param aggregation \code{string} method for aggregating the text embeddings
    #'created by BERT models. See details for more information.
    #'@param model_dir \code{string} path to the directory where the
    #'BERT model is stored.
    #'@param bow_basic_text_rep object of class \code{basic_text_rep} created via
    #'the function \link{bow_pp_create_basic_text_rep}. Only relevant for \code{method="glove"}
    #'and \code{method="lda"}.
    #'@param bow_n_dim \code{int} Number of dimensions of the GlobalVector or
    #'number of topics for LDA.
    #'@param bow_n_cluster \code{int} Number of clusters created on the basis
    #'of GlobalVectors. Parameter is not relevant for \code{method="lda"} and
    #'\code{method="bert"}
    #'@param bow_max_iter \code{int} Maximum number of iterations for fitting
    #'GlobalVectors and Topic Models.
    #'@param bow_max_iter_cluster \code{int} Maximum number of iterations for
    #'fitting cluster if \code{method="glove"}.
    #'@param bow_cr_criterion \code{double} convergence criterion for GlobalVectors.
    #'@param bow_learning_rate \code{double} initial learning rate for GlobalVectors.
    #'@param trace \code{bool} \code{TRUE} prints information about the progress.
    #'\code{FALSE} does not.
    #'@details \itemize{
    #'\item{method: }{In the case of \code{method="bert"}, a pretrained BERT model
    #'must be supplied via \code{model_dir}. For \code{method="glove"}
    #'and \code{method="lda"} a new model will be created based on the data provided
    #'via \code{bow_basic_text_rep}. The original algorithm for GlobalVectors provides
    #'only word embeddings, not text embeddings. To achieve text embeddings the words
    #'are clustered based on their word embeddings with kmeans.}
    #'
    #'\item{aggregation: }{For creating a text embedding with a BERT model, several options
    #'are possible:
    #'\itemize{
    #'\item{last: }{\code{aggregation="last"} uses only the hidden states of the last layer.}
    #'\item{second_to_last: }{\code{aggregation="second_to_last"} uses the hidden states of the second to last layer.}
    #'\item{fourth_to_last: }{\code{aggregation="fourth_to_last"} uses the hidden states of the fourth to last layer.}
    #'\item{all: }{\code{aggregation="all"} uses the mean of the hidden states of all hidden layers.}
    #'\item{last_four: }{\code{aggregation="last_four"} uses the mean of the hidden states of the last four layers.}
    #'}}
    #'
    #'}
    #'@import tidytext
    #'@importFrom topicmodels LDA
    #'@import quanteda
    #'@importFrom text2vec GlobalVectors
    #'@import reticulate
    #'@import stats
    #'@import reshape2
    initialize=function(model_name=NULL,
                        model_label=NULL,
                        model_version=NULL,
                        model_language=NULL,
                        method=NULL,
                        max_length=0,
                        chunks=1,
                        overlap=0,
                        aggregation="last",
                        model_dir,
                        bow_basic_text_rep,
                        bow_n_dim=10,
                        bow_n_cluster=100,
                        bow_max_iter=500,
                        bow_max_iter_cluster=500,
                        bow_cr_criterion=1e-8,
                        bow_learning_rate=1e-8,
                        trace=FALSE){
      #Parameter check---------------------------------------------------------
      if(is.null(model_name)){
        stop("model_name must be a character.")
      }
      if(is.null(model_label)){
        stop("model_label must be a character.")
      }
      if(is.null(model_version)){
        stop("model_version must be a character.")
      }
      if(is.null(model_language)){
        stop("model_language must be a character.")
      }
      if(is.null(method)){
        stop("method must be bert, glove_cluster or lda.")
      }
      if(!is.integer(as.integer(max_length))){
        stop("max_length must an integer.")
      }
      if(!is.integer(as.integer(chunks))){
        stop("chunks must an integer.")
      }
      if(!is.integer(as.integer(overlap))){
        stop("overlap must an integer.")
      }
      if((aggregation %in% c("last",
                            "second_to_last",
                            "fourth_to_last",
                            "all",
                            "last_four"))==FALSE){
        stop("aggregation must be last, second_to_last, fourth_to_last, all or
                            last_four.")

      }
      #------------------------------------------------------------------------

      #basic_components-------------------------------------------------------
      self$basic_components$method=method
      self$basic_components$max_length=as.integer(max_length)
      #------------------------------------------------------------------------
      if(self$basic_components$method=="bert" |
         self$basic_components$method=="roberta" |
         self$basic_components$method=="longformer"){

        if(self$basic_components$method=="bert"){
          self$transformer_components$tokenizer<-transformers$BertTokenizerFast$from_pretrained(model_dir)
          self$transformer_components$model<-transformers$TFBertForMaskedLM$from_pretrained(model_dir)
        } else if(self$basic_components$method=="roberta"){
          self$transformer_components$tokenizer<-transformers$RobertaTokenizerFast$from_pretrained(model_dir)
          self$transformer_components$model<-transformers$TFRobertaForMaskedLM$from_pretrained(model_dir)
        } else if(self$basic_components$method=="longformer"){
          self$transformer_components$tokenizer<-transformers$LongformerTokenizerFast$from_pretrained(model_dir)
          self$transformer_components$model<-transformers$TFLongformerForMaskedLM$from_pretrained(model_dir)
        }

        if(self$basic_components$method=="longformer" |
           self$basic_components$method=="roberta"){
          if(max_length>(self$transformer_components$model$config$max_position_embeddings)){
            stop(paste("max_length is",max_length,". This value is not allowed to exceed",
                       self$transformer_components$model$config$max_position_embeddings))
          }
        }

        self$transformer_components$chunks<-chunks
        self$transformer_components$overlap<-overlap
        self$transformer_components$aggregation<-aggregation
        #------------------------------------------------------------------------
      } else if(self$basic_components$method=="glove_cluster"){
        glove <- text2vec::GlobalVectors$new(rank = bow_n_dim,
                                             x_max = 10
        )
        set.seed(0)
        wv_main <- glove$fit_transform(bow_basic_text_rep$fcm,
                                       n_iter = bow_max_iter,
                                       convergence_tol = bow_cr_criterion,
                                       n_threads = 8,
                                       progressbar = trace,
                                       learning_rate = bow_learning_rate)
        wv_context <-glove$components
        transformation_matrix <- wv_main + t(wv_context)

        embedding=matrix(data=NA,
                         nrow=nrow(transformation_matrix),
                         ncol=ncol(transformation_matrix)+1)
        tmp_row_names<-rownames(transformation_matrix)

        for(i in 1:nrow(transformation_matrix)){
          if(bow_basic_text_rep$configuration$use_lemmata==TRUE){
            if(bow_basic_text_rep$configuration$to_lower==TRUE){
              tmp<-bow_basic_text_rep$language_model$vocab$lemma_tolower
              index<-match(x = tmp_row_names[i],table = tmp)
              embedding[i,1]<-bow_basic_text_rep$language_model$vocab$index_lemma_lower[index]
              embedding[i,2:ncol(embedding)]<-transformation_matrix[i,]
            } else {
              tmp<-bow_basic_text_rep$language_model$vocab$lemma
              index<-match(x = tmp_row_names[i],table = tmp)
              embedding[i,1]<-bow_basic_text_rep$language_model$vocab$index_lemma[index]
              embedding[i,2:ncol(embedding)]<-transformation_matrix[i,]
            }
          } else {
            if(bow_basic_text_rep$configuration$to_lower==TRUE){
              tmp<-bow_basic_text_rep$language_model$vocab$token_tolower
              index<-match(x = tmp_row_names[i],table = tmp)
              embedding[i,1]<-bow_basic_text_rep$language_model$vocab$index_token_lower[index]
              embedding[i,2:ncol(embedding)]<-transformation_matrix[i,]
            } else {
              tmp<-bow_basic_text_rep$language_model$vocab$token
              index<-match(x = tmp_row_names[i],table = tmp)
              embedding[i,1]<-bow_basic_text_rep$language_model$vocab$index_token[index]
              embedding[i,2:ncol(embedding)]<-transformation_matrix[i,]
            }
          }

        }
        embedding<-embedding[order(embedding[,1]),]
        rownames(embedding)<-embedding[,1]
        embedding<-embedding[,-1]
        #Creating Clusters-----------------------------------------------------
        cluster_structure<-stats::kmeans(x=embedding,
                                         centers = bow_n_cluster,
                                         iter.max = bow_max_iter_cluster,
                                         nstart=5,
                                         trace=trace,
                                         algorithm="Lloyd")
        token_cluster_assignments<-stats::fitted(object=cluster_structure,
                                                method="classes")
        model<-data.frame(index=names(token_cluster_assignments),
                          cluster=token_cluster_assignments)
        self$bow_components$model=model
        self$bow_components$vocab=bow_basic_text_rep$language_model$vocab
        self$bow_components$configuration$to_lower=bow_basic_text_rep$configuration$to_lower
        self$bow_components$configuration$use_lemmata=bow_basic_text_rep$configuration$use_lemmata
        self$bow_components$configuration$bow_n_dim=bow_n_dim
        self$bow_components$configuration$bow_n_cluster=bow_n_cluster
        self$bow_components$configuration$bow_max_iter=bow_max_iter
        self$bow_components$configuration$bow_max_iter_cluster=bow_max_iter_cluster
        self$bow_components$configuration$bow_cr_criterion=bow_cr_criterion
        self$bow_components$configuration$bow_learning_rate=bow_learning_rate
        self$bow_components$chunks=1
        self$bow_components$overlap=0

        #Topic Modeling--------------------------------------------------------
      } else if(self$basic_components$method=="lda"){
        selection<-(rowSums(as.matrix(bow_basic_text_rep$dfm))>0)
        corrected_dfm<-quanteda::dfm_subset(x=bow_basic_text_rep$dfm,
                                            selection)

        lda <- topicmodels::LDA(
          x = corrected_dfm,
          k = bow_n_dim,
          control=list(
            verbose=as.integer(trace),
            best=1,
            initialize="random")
          )

        lda<-tidytext::tidy(lda)

        #Transforming tibble into a matrix
        term_topic_beta<-matrix(ncol = bow_n_dim,
                                nrow = corrected_dfm@Dim[2],
                                data = NA)
        tmp_features<-colnames(corrected_dfm)
        for(i in 1:corrected_dfm@Dim[2]){
          tmp<-subset(lda,lda$term==tmp_features[i])
          term_topic_beta[i,]<-as.numeric(tmp$beta)
        }

        #Adding token indices
        tmp_index=vector(length = corrected_dfm@Dim[2])
        for(i in 1:nrow(term_topic_beta)){
          if(bow_basic_text_rep$configuration$use_lemmata==TRUE){
            if(bow_basic_text_rep$configuration$to_lower==TRUE){
              tmp<-bow_basic_text_rep$language_model$vocab$lemma_tolower
              index<-match(x = tmp_features[i],table = tmp)
              tmp_index[i]<-bow_basic_text_rep$language_model$vocab$index_lemma_lower[index]
            } else {
              tmp<-bow_basic_text_rep$language_model$vocab$lemma
              index<-match(x = tmp_features[i],table = tmp)
              tmp_index[i]<-bow_basic_text_rep$language_model$vocab$index_lemma[index]
            }
          } else {
            if(bow_basic_text_rep$configuration$to_lower==TRUE){
              tmp<-bow_basic_text_rep$language_model$vocab$token_tolower
              index<-match(x = tmp_features[i],table = tmp)
              tmp_index[i]<-bow_basic_text_rep$language_model$vocab$index_token_lower [index]
            } else {
              tmp<-bow_basic_text_rep$language_model$vocab$token
              index<-match(x = tmp_features[i],table = tmp)
              tmp_index[i]<-bow_basic_text_rep$language_model$vocab$index_token[index]
            }
          }
        }

      term_topic_beta<- matrix(mapply(term_topic_beta,FUN=as.numeric),
               nrow = nrow(term_topic_beta),
               ncol = ncol(term_topic_beta),
               byrow = FALSE)
      colnames(term_topic_beta)<-colnames(term_topic_beta,
                                          do.NULL = FALSE,
                                          prefix = "topic_")
      model<-data.frame(index=tmp_index,
                        topic=term_topic_beta)
      model<-model[order(model$index),]
      rownames(model)<-model$index

      self$bow_components$model=model
      self$bow_components$vocab=bow_basic_text_rep$language_model$vocab
      self$bow_components$configuration$to_lower=bow_basic_text_rep$configuration$to_lower
      self$bow_components$configuration$use_lemmata=bow_basic_text_rep$configuration$use_lemmata
      self$bow_components$configuration$bow_n_dim=bow_n_dim
      self$bow_components$configuration$bow_n_cluster=bow_n_cluster
      self$bow_components$configuration$bow_max_iter=bow_max_iter
      self$bow_components$configuration$bow_max_iter_cluster=bow_max_iter_cluster
      self$bow_components$configuration$bow_cr_criterion=bow_cr_criterion
      self$bow_components$configuration$bow_learning_rate=bow_learning_rate
      self$bow_components$chunks=1
      self$bow_components$overlap=0
      }

      private$model_info$model_name<-model_name
      private$model_info$model_label<-model_label
      private$model_info$model_version<-model_version
      private$model_info$model_language<-model_language
      private$model_info$model_date<-date()
    },
    #--------------------------------------------------------------------------
    #'@description Method for loading a transformers model into R.
    #'@param model_dir \code{string} containing the path to the relevant
    #'model.
    load_model=function(model_dir){
        if(self$basic_components$method=="bert" |
           self$basic_components$method=="roberta" |
           self$basic_components$method=="longformer"){

          if(self$basic_components$method=="bert"){
            self$transformer_components$tokenizer<-transformers$BertTokenizerFast$from_pretrained(model_dir)
            self$transformer_components$model<-transformers$TFBertForMaskedLM$from_pretrained(model_dir)
          } else if(self$basic_components$method=="roberta"){
            self$transformer_components$tokenizer<-transformers$RobertaTokenizerFast$from_pretrained(model_dir)
            self$transformer_components$model<-transformers$TFRobertaForMaskedLM$from_pretrained(model_dir)
          } else if(self$basic_components$method=="longformer"){
            self$transformer_components$tokenizer<-transformers$LongformerTokenizerFast$from_pretrained(model_dir)
            self$transformer_components$model<-transformers$TFLongformerForMaskedLM$from_pretrained(model_dir)
          }
      } else {
        message("Method only relevant for transformers models.")
      }
    },
    #-------------------------------------------------------------------------
    #'@description Method for encoding words of raw texts into integers.
    #'@param raw_text \code{vector} containing the raw texts.
    #'@param token_encodings_only \code{bool} If \code{TRUE}, only the token
    #'encodings are returned. If \code{FALSE}, the complete encoding is returned
    #'which is important for BERT models.
    #'@param trace \code{bool} If \code{TRUE}, information of the progress
    #'is printed. \code{FALSE} if not requested.
    #'@return \code{list} containing the integer sequences of the raw texts with
    #'special tokens.
    encode=function(raw_text,
                    token_encodings_only=FALSE,
                    trace = FALSE){
      n_units<-length(raw_text)

      if(self$basic_components$method=="bert" |
         self$basic_components$method=="roberta" |
         self$basic_components$method=="longformer"){
        chunk_list<-vector(length = n_units)
        encodings<-NULL
        #---------------------------------------------------------------------
        if(token_encodings_only==TRUE){
          for(i in 1:n_units){
            preparation_tokens<-quanteda::tokens(raw_text[i])
            preparation_tokens<-quanteda::tokens_chunk(
              x=preparation_tokens,
              size=self$basic_components$max_length,
              overlap = self$transformer_components$overlap,
              use_docvars = FALSE)

            chunks=min(length(preparation_tokens),self$transformer_components$chunks)
            tokens_unit<-NULL
            for(j in 1:chunks){
              tokens_unit[j]<-list(
                self$transformer_components$tokenizer(
                  paste(preparation_tokens[j],collapse = " "),
                  padding=TRUE,
                  truncation=TRUE,
                  max_length=as.integer(self$basic_components$max_length),
                  return_tensors="tf")
              )
              if(trace==TRUE){
                cat(paste(date(),i,"/",n_units,"block",j,"/",chunks))
              }
            }
            encodings[i]<-list(tokens_unit)
          }
          encodings_only=NULL
          for(i in 1:length(encodings)){
            encodings_only[i]=list(as.vector(encodings[[as.integer(i)]][[as.integer(1)]][["input_ids"]])$numpy())
          }
          return(encodings_only)
          #--------------------------------------------------------------------
        } else {
          text_chunks<-NULL
          for(i in 1:n_units){
            preparation_tokens<-quanteda::tokens(raw_text[i])
            preparation_tokens<-quanteda::tokens_chunk(
              x=preparation_tokens,
              size=self$basic_components$max_length,
              overlap = self$transformer_components$overlap,
              use_docvars = FALSE)
            preparation_tokens=as.list(preparation_tokens)
            preparation_tokens=lapply(X=preparation_tokens,FUN=paste,collapse=" ")

            chunk_list[i]=min(length(preparation_tokens),self$transformer_components$chunks)
            preparation_tokens<-preparation_tokens[1:chunk_list[i]]

            index_min=length(text_chunks)+1
            index_max=length(text_chunks)+length(preparation_tokens)
            #cat(index_min)
            text_chunks=append(x=text_chunks,values = unname(preparation_tokens))
            #text_chunks[index_min:index_max]<-list(preparation_tokens)
          }

          encodings=self$transformer_components$tokenizer(
            text_chunks,
            padding=TRUE,
            truncation=TRUE,
            max_length=as.integer(self$basic_components$max_length),
            return_tensors="tf")

          return(encodings_list=list(encodings=encodings,
                                     chunks=chunk_list))
        }
      } else if(self$basic_components$method=="glove_cluster"|
                self$basic_components$method=="lda"){
        textual_corpus <-quanteda::corpus(raw_text)
        token<-quanteda::tokens(textual_corpus)
        if(self$bow_components$configuration$use_lemmata==TRUE){
          if(self$bow_components$configuration$to_lower==TRUE){
            token<-quanteda::tokens_keep(x=token,
                                         pattern = self$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = self$bow_components$vocab$token,
                                            replacement = as.character(self$bow_components$vocab$index_lemma_lower),
                                            valuetype = "fixed",
                                            verbose=verbose)
          } else {
            token<-quanteda::tokens_keep(x=token,
                                         pattern = self$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = self$bow_components$vocab$token,
                                            replacement = as.character(self$bow_components$vocab$index_lemma),
                                            valuetype = "fixed",
                                            verbose=verbose)
          }
        } else {
          if(self$bow_components$configuration$to_lower==TRUE){
            token<-quanteda::tokens_keep(x=token,
                                         pattern = self$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = self$bow_components$vocab$token,
                                            replacement = as.character(self$bow_components$vocab$index_token_lower),
                                            valuetype = "fixed",
                                            verbose=verbose)
          } else {
            token<-quanteda::tokens_keep(x=token,
                                         pattern = self$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = self$bow_components$vocab$token,
                                            replacement = as.character(self$bow_components$vocab$index_token),
                                            valuetype = "fixed",
                                            verbose=verbose)
          }
        }
        encodings<-NULL
        for(i in 1:length(token)){
          encodings[i]<-list(as.integer(as.vector(token[[i]])))
        }
        return(encodings)
      }
    },
    #--------------------------------------------------------------------------
    #'@description Method for decoding a sequence of integers into tokens
    #'@param int_seqence \code{list} containing the integer sequences which
    #'should be transformed to tokens or a single integer sequence as \code{vector}
    #'@return \code{list} of token sequences
    decode=function(int_seqence){

      if(!is.list(int_seqence)){
        tmp=NULL
        tmp[1]=list(int_seqence)
        int_seqence=tmp[1]
      }
      #-------------------------------------------------------------------------
      if(self$basic_components$method=="bert" |
         self$basic_components$method=="roberta" |
         self$basic_components$method=="longformer"){
        tmp_token_list=NULL
        for(i in 1:length(int_seqence)){
          tmp_vector<-int_seqence[[i]]
          mode(tmp_vector)="integer"
          tmp_token_list[i]=list(self$transformer_components$tokenizer$decode(tmp_vector))
        }
        return(tmp_token_list)

      #-------------------------------------------------------------------------
      } else if(self$basic_components$method=="glove_cluster" |
                self$basic_components$method=="lda"){
        if(self$bow_components$configuration$to_lower==TRUE){
          if(self$bow_components$configuration$use_lemmata==FALSE){
            input_column="index_token_lower"
            target_coumn="token_tolower"
          } else {
            input_column="index_lemma_lower"
            target_coumn="lemma_tolower"
          }
        } else {
          if(self$bow_components$configuration$use_lemmata==FALSE){
            input_column="index_token"
            target_coumn="token"
          } else {
            input_column="index_lemma"
            target_coumn="lemma "
          }
        }

        tmp_token_list=NULL
        for(i in 1:length(int_seqence)){
          tmp_int_seq=int_seqence[[i]]
          tmp_token_seq=vector(length = length(tmp_int_seq))
          for(j in 1:length(tmp_int_seq)){
            index=match(x=tmp_int_seq[j],
                        table=self$bow_components$vocab[,input_column])
                        #table=global_vector_clusters_modeling$bow_components$vocab[,input_column])
            tmp_token_seq[j]=self$bow_components$vocab[index,target_coumn]
            #tmp_token_seq[j]=global_vector_clusters_modeling$bow_components$vocab[index,target_coumn]
          }
          tmp_token_list[i]=list(tmp_token_seq)
        }
        return(tmp_token_list)
      }
    },
    #Embedding------------------------------------------------------------------
    #'@description Method for creating text embeddings from raw texts
    #'@param raw_text \code{vector} containing the raw texts.
    #'@param doc_id \code{vector} containing the corresponding IDs for every text.
    #'@param batch_size \code{int} determining the maximal size of every batch.
    #'@param trace \code{bool} \code{TRUE}, if information about the progression
    #'should be printed on console.
    #'@return Method returns a \link[R6]{R6} object of class \link{EmbeddedText}. This object
    #'contains the embeddings as a \code{data.frame} and information about the
    #'model creating the embeddings.
    embed=function(raw_text=NULL,doc_id=NULL,batch_size=8, trace = FALSE){

      #bert---------------------------------------------------------------------
      if(self$basic_components$method=="bert" |
         self$basic_components$method=="roberta" |
         self$basic_components$method=="longformer"){

        n_units<-length(raw_text)
        n_layer<-self$transformer_components$model$config$num_hidden_layers
        n_layer_size<-self$transformer_components$model$config$hidden_size

        #Batch refers to the number of cases
        n_batches=ceiling(n_units/batch_size)
        batch_results<-NULL
        for (b in 1:n_batches){
          tokens<-self$encode(raw_text = raw_text,
                              trace = trace,
                              token_encodings_only=FALSE)

          index_min=1+(b-1)*batch_size
          index_max=min(b*batch_size,n_units)
          batch=index_min:index_max
          #cat(batch)

          tokens<-self$encode(raw_text = raw_text[batch],
                              trace = trace,
                              token_encodings_only=FALSE)

          text_embedding<-array(
            data = 0,
            dim = c(length(batch),
                    self$transformer_components$chunks,
                    n_layer_size))

          #Clear session to ensure enough memory
          tf$keras$backend$clear_session()

          #Calculate tensors
          tensor_embeddings<-self$transformer_components$model(
            tokens$encodings,
            output_hidden_states=TRUE)$hidden_states

          #Selecting the relevant layers
          if(self$transformer_components$aggregation=="last"){
            selected_layer=self$transformer_components$model$config$num_hidden_layers
          } else if (self$transformer_components$aggregation=="second_to_last") {
            selected_layer=self$transformer_components$model$config$num_hidden_layers-2
          } else if (self$transformer_components$aggregation=="fourth_to_last") {
            selected_layer=self$transformer_components$model$config$num_hidden_layers-4
          } else if (self$transformer_components$aggregation=="all") {
            selected_layer=2:self$transformer_components$model$config$num_hidden_layers
          } else if (self$transformer_components$aggregation=="last_four") {
            selected_layer=(self$transformer_components$model$config$num_hidden_layers-4):self$transformer_components$model$config$num_hidden_layers
          }

          #Sorting the hidden states to the corresponding cases and times
          #If more than one layer is selected the mean is calculated
          #CLS Token is always the first token
          index=0
          tmp_selected_layer=1+selected_layer
          for(i in 1:length(batch)){
            for(j in 1:tokens$chunks[i]){
              for(layer in tmp_selected_layer){
                text_embedding[i,j,]<-text_embedding[i,j,]+as.vector(
                  tensor_embeddings[[as.integer(layer)]][[as.integer(index)]][[as.integer(0)]]$numpy()
                  )
              }
              text_embedding[i,j,]<-text_embedding[i,j,]/length(tmp_selected_layer)
              index=index+1
            }
          }
          dimnames(text_embedding)[[3]]<-colnames(text_embedding[,1,],
                                                  do.NULL = FALSE,
                                                  prefix = "bert_")
            #Add ID of every case
            dimnames(text_embedding)[[1]]<-doc_id[batch]
            batch_results[b]=list(text_embedding)
            if(trace==TRUE){
            cat(paste(date(),
                        "Batch",b,"/",n_batches,"Done"))
            }
          }

      #Summarizing the results over all batchtes
      text_embedding=abind::abind(batch_results,along = 1)
        #Glove Cluster----------------------------------------------------------
      } else if(self$basic_components$method=="glove_cluster"){
        tokens<-self$encode(raw_text = raw_text,
                            trace = trace,
                            token_encodings_only=FALSE)


        text_embedding<-array(
          data = 0,
          dim = c(length(tokens),
                  1,
                  self$bow_components$configuration$bow_n_cluster))
        #text_embedding<-matrix(nrow = length(tokens),
        #                       ncol =  self$bow_components$configuration$bow_n_cluster,
        #                       data = 0)
        for(i in 1:length(tokens)){
          token_freq<-table(tokens[[i]])
          tmp_tokens<-names(token_freq)
          for(j in 1:length(token_freq)){
            index<-match(x=as.integer(tmp_tokens[j]),
                  table = self$bow_components$model$index)
            text_embedding[i,1,self$bow_components$model$cluster[index]]<-token_freq[j]+
              text_embedding[i,1,self$bow_components$model$cluster[index]]
          }
        }

        #text_embedding=text_embedding/rowSums(text_embedding)
        #text_embedding[is.nan(text_embedding)]<-0

        dimnames(text_embedding)[[3]]<-colnames(text_embedding[,1,],
                                                do.NULL = FALSE,
                                                prefix = "cluster_")
        #Add ID of every case
        dimnames(text_embedding)[[1]]<-doc_id

        #text_embedding<-as.data.frame(text_embedding)
        #Topic Modeling---------------------------------------------------------
      } else if(self$basic_components$method=="lda"){
        tokens<-self$encode(raw_text = raw_text,
                            trace = trace,
                            token_encodings_only=FALSE)

        text_embedding<-array(
          data = 0,
          dim = c(length(tokens),
                  1,
                  self$bow_components$configuration$bow_n_dim))
        #text_embedding<-matrix(nrow = length(tokens),
        #                       ncol =  self$bow_components$configuration$bow_n_dim,
        #                       data = 0)
        for(i in 1:length(tokens)){
          token_freq<-table(tokens[[i]])
          tmp_tokens<-names(token_freq)
          if(length(tmp_tokens)>0){
            for(j in 1:length(token_freq)){
              index<-match(x=as.integer(tmp_tokens[j]),
                           table = self$bow_components$model$index)
              if(is.na(index)==FALSE){
                text_embedding[i,1,]<-text_embedding[i,1,]+token_freq[j]*as.matrix(self$bow_components$model[index,-1])
              }
            }
          }
        }
        #text_embedding<-text_embedding/rowSums(self$bow_components$model[,-1])
        text_embedding<-text_embedding/rowSums(text_embedding)
        #Replace NaN with 0 which indicate that the rowsum is 0 and division ist not
        #possible
        text_embedding[is.nan(text_embedding)]<-0

        dimnames(text_embedding)[[3]]<-colnames(text_embedding[,1,],
                                                do.NULL = FALSE,
                                                prefix = "lda_")
        #Add ID of every case
        dimnames(text_embedding)[[1]]<-doc_id

        #text_embedding<-as.data.frame(text_embedding)
      }
      #------------------------------------------------------------------------

      if(self$basic_components$method=="bert" |
         self$basic_components$method=="roberta" |
         self$basic_components$method=="longformer" ){
        embeddings<-EmbeddedText$new(
          model_name = private$model_info$model_name,
          model_label = private$model_info$model_label,
          model_date = private$model_info$model_date,
          model_method = self$basic_components$method,
          model_version = private$model_info$model_version,
          model_language = private$model_info$model_language,
          param_seq_length =self$basic_components$max_length,
          param_chunks = self$transformer_components$chunks,
          param_overlap = self$transformer_components$overlap,
          param_aggregation = self$transformer_components$aggregation,
          embeddings = text_embedding
        )
      } else if(self$basic_components$method=="glove_cluster" |
                self$basic_components$method=="lda"){
        embeddings<-EmbeddedText$new(
          model_name = private$model_info$model_name,
          model_date = private$model_info$model_date,
          model_label = private$model_info$model_label,
          model_method = self$basic_components$method,
          model_version = private$model_info$model_version,
          model_language = private$model_info$model_language,
          param_seq_length =self$basic_components$max_length,
          param_chunks = self$bow_components$chunks,
          param_overlap = self$bow_components$overlap,
          param_aggregation = self$bow_components$aggregation,
          embeddings = text_embedding
        )
      }
      return(embeddings)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting the bibliographic information of the model.
    #'@param type \code{string} Type of information which should be changed/added.
    #'\code{type="developer"}, and \code{type="modifier"} are possible.
    #'@param authors List of people.
    #'@param citation \code{string} Citation in free text.
    #'@param url \code{string} Corresponding URL if applicable.
    set_publication_info=function(type,
                                  authors,
                                  citation,
                                  url=NULL){
      if(type=="developer"){
        private$publication_info$developed_by$authors<-authors
        private$publication_info$developed_by$citation<-citation
        private$publication_info$developed_by$url<-url
      } else if(type=="modifier"){
        private$publication_info$modified_by$authors<-authors
        private$publication_info$modified_by$citation<-citation
        private$publication_info$modified_by$url<-url
      }
     },
    #--------------------------------------------------------------------------
    #'@description Method for getting the bibliographic information of the model.
    #'@return \code{list} of bibliographic information.
    get_publication_info=function(){
      return(private$publication_info)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting the license of the model
    #'@param license \code{string} containing the abbreviation of the license or
    #'the license text.
    set_license=function(license){
      private$model_info$model_license<-license
    },
    #'@description Method for requesting the license of the model
    #'@return \code{string} License of the model
    get_license=function(){
      return(private$model_info$model_license)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting a description of the model
    #'@param eng \code{string} A text describing the training of the classifier,
    #'its theoretical and empirical background, and the different output labels
    #'in English.
    #'@param native \code{string} A text describing the training of the classifier,
    #'its theoretical and empirical background, and the different output labels
    #'in the native language of the model.
    #'@param abstract_eng \code{string} A text providing a summary of the description
    #'in English.
    #'@param abstract_native \code{string} A text providing a summary of the description
    #'in the native language of the classifier.
    #'@param keywords_eng \code{vector} of keywords in English.
    #'@param keywords_native \code{vector} of keywords in the native language of the classifier.
    set_model_description=function(eng=NULL,
                                   native=NULL,
                                   abstract_eng=NULL,
                                   abstract_native=NULL,
                                   keywords_eng=NULL,
                                   keywords_native=NULL){
      if(!is.null(eng)){
        private$model_description$eng=eng
      }
      if(!is.null(native)){
        private$model_description$native=native
      }

      if(!is.null(abstract_eng)){
        private$model_description$abstract_eng=abstract_eng
      }
      if(!is.null(abstract_native)){
        private$model_description$abstract_native=abstract_native
      }

      if(!is.null(keywords_eng)){
        private$model_description$keywords_eng=keywords_eng
      }
      if(!is.null(keywords_native)){
        private$model_description$keywords_native=keywords_native
      }



    },
    #'@description Method for requesting the model description.
    #'@return \code{list} with the description of the model in English
    #'and the native language.
    get_model_description=function(){
      return(private$model_description)
    },
    #--------------------------------------------------------------------------
    #'@description Method for requesting the model information
    #'@return \code{list} of all relevant model information
    get_model_info=function(){
      return(list(
        model_license=private$model_info$model_license,
        model_name=private$model_info$model_name,
        model_label=private$model_info$model_label,
        model_date=private$model_info$model_date,
        model_version=private$model_info$model_version,
        model_language=private$model_info$model_language
        )
        )
    }
  )
)


#'@title Embedded text
#'@description Object of class \link[R6]{R6} which stores the text embeddings
#'generated by an object of class \link{TextEmbeddingModel} via the method
#'\code{embed()}.
#'@export
EmbeddedText<-R6::R6Class(
  classname = "EmbeddedText",
  private = list(

    #model_name \code{string} Name of the model that generates this embedding.
    model_name=NA,


    #Label of the model that generates this embedding.
    model_label=NA,


    #Date when the embedding generating model was created.
    model_date=NA,


    #Method of the underlying embedding model
    model_method=NA,


    #Version of the model that generated this embedding.
    model_version=NA,


    #Language of the model that generated this embedding.
    model_language=NA,


    #Maximal number of tokens that processes the generating model for a chunk.
    param_seq_length=NA,


    #Number of tokens that were added at the beginning of the sequence for the next chunk
    #by this model.
    param_overlap=NA,


    #Maximal number of chunks which are supported by the generating model.
    param_chunks=NA,


    #Aggregation method of the hidden states.
    param_aggregation=NA
  ),
  public = list(
    #'@field embeddings ('data.frame()')\cr
    #'data.frame containing the text embeddings for all chunks. Documents are
    #'in the rows. Embedding dimensions are in the columns.
    embeddings=NA,

    #'@description Creates a new object representing text embeddings.
    #'@param model_name \code{string} Name of the model that generates this embedding.
    #'@param model_label \code{string} Label of the model that generates this embedding.
    #'@param model_date \code{string} Date when the embedding generating model was created.
    #'@param model_method \code{string} Method of the underlying embedding model.
    #'@param model_version \code{string} Version of the model that generated this embedding.
    #'@param model_language \code{string} Language of the model that generated this embedding.
    #'@param param_seq_length \code{int} Maximum number of tokens that processes the generating model for a chunk.
    #'@param param_chunks \code{int} Maximum number of chunks which are supported by the generating model.
    #'@param param_overlap \code{int} Number of tokens that were added at the beginning of the sequence for the next chunk
    #'by this model.
    #'@param param_aggregation \code{string} Aggregation method of the hidden states.
    #'@param embeddings \code{data.frame} containing the text embeddings.
    initialize=function(model_name=NA,
                        model_label=NA,
                        model_date=NA,
                        model_method=NA,
                        model_version=NA,
                        model_language=NA,
                        param_seq_length=NA,
                        param_chunks=NULL,
                        param_overlap=NULL,
                        param_aggregation=NULL,
                        embeddings){
      private$model_name = model_name
      private$model_label = model_label
      private$model_date = model_date
      private$model_method = model_method
      private$model_version = model_version
      private$model_language = model_language
      private$param_seq_length = param_seq_length
      private$param_chunks = param_chunks
      private$param_overlap = param_overlap
      private$param_aggregation = param_aggregation
      self$embeddings=embeddings
    },
    #--------------------------------------------------------------------------
    #'@description Method for retrieving information about the model that
    #'generated this embedding.
    #'@return \code{list} contain all saved information about the underlying
    #'text embedding model.
    get_model_info=function(){
      tmp<-list(model_name=private$model_name,
                model_name=private$model_label,
                model_date=private$model_date,
                model_method=private$model_method,
                model_version=private$model_version,
                model_language=private$model_language,
                param_seq_length=private$param_seq_length,
                param_chunks=private$param_chunks,
                param_overlap=private$param_overlap,
                param_aggregation=private$param_aggregation)
      return(tmp)
    },
    #--------------------------------------------------------------------------
    #'@description Method for retrieving the label of the model that
    #'generated this embedding.
    #'@return \code{string} Lable of the corresponding text embedding model
    get_model_label=function(){
      return(private$model_label)
    }
  )
)

#'Combine embedded texts
#'
#'Function for combining embedded texts of the same model
#'
#'@param embeddings_list \code{list} of objects of class \link{EmbeddedText}.
#'@return Returns an object of class \link{EmbeddedText} which contains all
#'unique cases of the input objects.
#'@export
#'@importFrom methods isClass
#'@importFrom abind abind
combine_embeddings<-function(embeddings_list){

  #Check for the right class---------------------------------------------------
  for(i in 1:length(embeddings_list)){
    if(methods::isClass(where=embeddings_list[[i]],
                        Class="EmbeddedText")==TRUE){
      stop("All elements of the embeddings_list must be of class
           EmbeddedText.")
    }
  }

  #Check for the right underlining embedding model-------------------------------
  result<-check_embedding_models(object_list = embeddings_list,
                                 same_class = FALSE)
  if(result==FALSE){
    stop("The models which created the embeddings are not similar
           accros all elements in embeddings_list. Please check
           the elements.")
  }


  #Check for unique names------------------------------------------------------
  tmp_names=NULL
  tmp_cases=NULL
  for(i in 1:length(embeddings_list)){
    if(i==1){
      tmp_names=rownames(embeddings_list[[i]]$embeddings)
      tmp_cases=nrow(embeddings_list[[i]]$embeddings)
    } else {
      tmp_names=c(tmp_names,rownames(embeddings_list[[i]]$embeddings))
      tmp_cases=tmp_cases+nrow(embeddings_list[[i]]$embeddings)
    }
  }
  tmp_names=unique(tmp_names)
  if(length(tmp_names)<tmp_cases){
    stop("There are cases with duplicated names. Please check your data. Names
         must be unique.")
  }


  #Combine embeddings-----------------------------------------------------------

  for(i in 1:length(embeddings_list)){
     if(i==1){
      combined_embeddings<-embeddings_list[[i]]$embeddings
    } else {
      combined_embeddings<-abind::abind(combined_embeddings,embeddings_list[[i]]$embeddings,
                                        along = 1)
    }
  }

  new_embedding<-EmbeddedText$new(
    embeddings = combined_embeddings,
    model_name = embeddings_list[[1]]$get_model_info()$model_name,
    model_date =embeddings_list[[1]]$get_model_info()$model_date,
    model_method=embeddings_list[[1]]$get_model_info()$model_method,
    model_version=embeddings_list[[1]]$get_model_info()$model_version,
    model_language=embeddings_list[[1]]$get_model_info()$model_language,
    param_seq_length=embeddings_list[[1]]$get_model_info()$param_seq_length,
    param_chunks=embeddings_list[[1]]$get_model_info()$param_chunks,
    param_overlap=embeddings_list[[1]]$get_model_info()$param_overlap,
    param_aggregation=embeddings_list[[1]]$get_model_info()$param_aggregation

  )

  return(new_embedding)
}
