WordEmbeddingModel<-R6::R6Class(
  classname = "WordEmbeddingModel",
  public = list(
    method=NULL,
    max_length=NULL,
    tf_components=list(
      tokenizer=NULL),
    bow_components=list(
      vocab=NULL,
      configuration=list(
        to_lower = NA,
        use_lemmata = NA,
        bow_n_dim = NA,
        bow_max_iter = NA,
        bow_cr_criterion = NA,
        bow_learning_rate = NA
      )),
    embedding=NULL,
    #--------------------------------------------------------------------------
    initialize=function(method,
                        max_length,
                        tf_model,
                        bow_basic_text_rep,
                        bow_n_dim,
                        bow_max_iter,
                        bow_cr_criterion,
                        bow_learning_rate,
                        trace = FALSE){
      self$method=method
      self$max_length=as.integer(max_length)
      #------------------------------------------------------------------------
    if(method=="transformer"){
      transformer = reticulate::import("transformers")
      self$tf_components$tokenizer <- transformer$AutoTokenizer$from_pretrained(tf_model)
      self$embedding<-transformer$TFBertModel$from_pretrained(tf_model)
      #------------------------------------------------------------------------
    } else if(method=="glove"){
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
              embedding[i,1]<-bow_basic_text_rep$language_model$vocab$index_token_lower [index]
              embedding[i,2:ncol(embedding)]<-transformation_matrix[i,]
            } else {
              tmp<-bow_basic_text_rep$language_model$vocab$token
              index<-match(x = tmp_row_names[i],table = tmp)
              embedding[i,1]<-bow_basic_text_rep$language_model$vocab$index_token[index]
              embedding[i,2:ncol(embedding)]<-transformation_matrix[i,]
            }
          }

      }
      #embedding<-as.data.frame(embedding)
      embedding<-embedding[order(embedding[,1]),]
      self$embedding=embedding
      self$bow_components$vocab=bow_basic_text_rep$language_model$vocab
      self$bow_components$configuration$to_lower=bow_basic_text_rep$configuration$to_lower
      self$bow_components$configuration$use_lemmata=bow_basic_text_rep$configuration$use_lemmata
      self$bow_components$configuration$bow_n_dim=bow_n_dim
      self$bow_components$configuration$bow_max_iter=bow_max_iter
      self$bow_components$configuration$bow_cr_criterion=bow_cr_criterion
      self$bow_components$configuration$bow_learning_rate=bow_learning_rate
    }
    },
    #--------------------------------------------------------------------------
    tokenize=function(raw_text,trace = FALSE){
      if(self$method=="transformer"){
        tokens<-NULL
        n_units<-length(raw_text)
        for(i in 1:n_units){
          tokens[i]<-list(
            self$tf_components$tokenizer(
              raw_text[i],
              padding=TRUE,
              truncation=TRUE,
              max_length=self$max_length,
              return_tensors="tf")
          )
          if(trace==TRUE){
            print(paste(date(),i,"/",n_units))
          }
        }
        return(tokens)
      } else if(self$method=="glove"){
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
      return(token)
      }
      #------------------------------------------------------------------------
    }
    #---------------------------------------------------------------------------
  )
)

