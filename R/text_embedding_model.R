#' @title Text Embedding Model
#'
#'@export
TextEmbeddingModel<-R6::R6Class(
  classname = "TextEmbeddingModel",
  private = list(
    model_info=list(
      model_license=NA,
      model_name=NA,
      model_date=NA,
      model_version=NA,
      model_language=NA
    ),
    publication_info=list(
      developed_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      ),
      trained_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      ),
      modifided_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      )
    ),
    model_description=list(
      eng=NULL,
      native=NULL
    )
  ),
  public = list(
    basic_components=list(
      method=NULL,
      max_length=NULL
    ),
    bert_components=list(
      model=NULL,
      tokenizer=NULL,
      aggregation=NULL,
      chunks=NULL,
      overlap=NULL),
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
    initialize=function(model_name,
                        model_version,
                        model_language,
                        method,
                        max_length,
                        chunks=1,
                        overlap=0,
                        aggregation="last",
                        bert_model_dir_path,
                        bow_basic_text_rep,
                        bow_n_dim=NULL,
                        bow_n_cluster=NULL,
                        bow_max_iter=500,
                        bow_max_iter_cluster=NULL,
                        bow_cr_criterion=1e-8,
                        bow_learning_rate=NULL,
                        trace=FALSE){
      self$basic_components$method=method
      self$basic_components$max_length=as.integer(max_length)
      #------------------------------------------------------------------------
      if(self$basic_components$method=="bert"){
        if(reticulate::py_module_available("transformers")==FALSE){
          reticulate::py_install('transformers', pip = TRUE)
        }
        transformer = reticulate::import('transformers')
        #self$bert_components$tokenizer<-transformer$AutoTokenizer$from_pretrained(bert_model)
        self$bert_components$tokenizer<-transformer$BertTokenizerFast$from_pretrained(bert_model_dir_path)
        self$bert_components$model<-transformer$TFBertModel$from_pretrained(bert_model_dir_path)
        self$bert_components$chunks<-chunks
        self$bert_components$overlap<-overlap
        self$bert_components$aggregation<-aggregation
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
                                         nstart=25,
                                         trace=trace)
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
        self$bow_components$chunks=chunks
        self$bow_components$overlap=overlap

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
          #tmp_index[i]<-tmp_features[i]
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
      self$bow_components$chunks=chunks
      self$bow_components$overlap=overlap
      }

      private$model_info$model_name<-model_name
      private$model_info$model_version<-model_version
      private$model_info$model_language<-model_language
      private$model_info$model_date<-date()
    },
    #--------------------------------------------------------------------------
    load_bert=function(bert_model_dir_path){
      if(self$basic_components$method=="bert"){
        transformer = reticulate::import('transformers')
        self$bert_components$tokenizer<-transformer$BertTokenizerFast$from_pretrained(bert_model_dir_path)
        self$bert_components$model<-transformer$TFBertModel$from_pretrained(bert_model_dir_path)
      } else {
        message("Method only relevant for bert models.")
      }
    },
    #--------------------------------------------------------------------------
    save_bert_model=function(bert_model_dir_path){
      if(self$basic_components$method=="bert"){
        self$bert_components$model$save_pretrained(
          save_directory=bert_model_dir_path)
        print(paste(date(),"Bert model saved."))
        self$bert_components$tokenizer$save_pretrained(
          bert_model_dir_path)
        print(paste(date(),"Tokenizer saved."))
      } else {
        message("Method only relevant for bert models.")
      }
    },
    #-------------------------------------------------------------------------
    fine_tune_bert_model=function(output_dir,
                                  bert_model_dir_path,
                                  model_name,
                                  raw_texts,
                                  p_mask=0.15,
                                  n_epoch,
                                  chunk_size,
                                  n_workers=1,
                                  multi_process=FALSE,
                                  trace=TRUE){
      transformer = reticulate::import('transformers')
      tf = reticulate::import('tensorflow')
      np=reticulate::import("numpy")

      mlm_model=transformer$TFBertForMaskedLM$from_pretrained(bert_model_dir_path)

      print(paste(date(),"Tokenize Raw Texts"))
      prepared_texts<-quanteda::tokens(
        x = raw_texts,
        what = "word",
        remove_punct = FALSE,
        remove_symbols = TRUE,
        remove_numbers = FALSE,
        remove_url = TRUE,
        remove_separators = TRUE,
        split_hyphens = FALSE,
        split_tags = FALSE,
        include_docvars = TRUE,
        padding = FALSE,
        verbose = trace)

      print(paste(date(),"Creating Text Chunks"))
      prepared_texts_chunks<-quanteda::tokens_chunk(
        x=prepared_texts,
        size=chunk_size,
        overlap = 0,
        use_docvars = FALSE)

      prepared_text_chunks_strings<-lapply(prepared_texts_chunks,paste,collapse = " ")
      prepared_text_chunks_strings<-as.character(prepared_text_chunks_strings)
      print(paste(date(),length(prepared_text_chunks_strings),"Chunks Created"))

      print(paste(date(),"Creating Input"))
      tokenized_texts=self$bert_components$tokenizer(prepared_text_chunks_strings,
                                                     truncation =TRUE,
                                                     padding= TRUE,
                                                     max_length=as.integer(chunk_size),
                                                     return_tensors="np")

      input_ids<-tokenized_texts["input_ids"]
      token_type_ids<-tokenized_texts["token_type_ids"]
      attention_mask<-tokenized_texts["attention_mask"]

      print(paste(date(),"Creating Masked Data"))
      masked_ids<-input_ids
      for(i in 1:nrow(masked_ids)){
        tmp_sample<-sample(2:(chunk_size-1),size = p_mask*chunk_size)
        masked_ids[i,tmp_sample]<-tokenizer$mask_token_id
      }

      mode(input_ids) <- "integer"
      mode(token_type_ids) <- "integer"
      mode(attention_mask) <- "integer"
      mode(masked_ids) <- "integer"

      data_new<-reticulate::dict("input_ids"=masked_ids,
                                 "token_type_ids"=token_type_ids,
                                 "attention_mask"=attention_mask)

      print(paste(date(),"Preparing Training of the Model"))
      adam<-tf$keras$optimizers$Adam

      print(paste(date(),"Compile Model"))
      mlm_model$compile(optimizer=adam(3e-5))

      print(paste(date(),"Start Fine Tuning"))
      mlm_model$fit(x=data_new,
                                     y=input_ids,
                                     epochs=as.integer(n_epoch),
                                     workers=as.integer(n_workers),
                                     use_multiprocessing=multi_process
      )

      print(paste(date(),"Saving Bert Model"))
      mlm_model$save_pretrained(
        save_directory=output_dir)

      print(paste(date(),"Saving Tokenizer"))
      self$bert_components$tokenizer$save_pretrained(
        output_dir)

      print(paste(date(),"Setting new Model Name and Date"))
      self$model_info$model_name<-model_name
      self$model_info$model_date<-date()

      print(paste(date(),"Reloading model"))
      self$bert_components$model<-transformer$TFBertModel$from_pretrained(output_dir)

      print(paste(date(),"Done"))

    },
    #-------------------------------------------------------------------------
    tokenize=function(raw_text, trace = FALSE){
      n_units<-length(raw_text)

      if(self$basic_components$method=="bert"){
        tokens<-NULL
        for(i in 1:n_units){
          preparation_tokens<-quanteda::tokens(raw_text[i])
          preparation_tokens<-quanteda::tokens_chunk(
            x=preparation_tokens,
            size=self$basic_components$max_length,
            overlap = self$bert_components$overlap,
            use_docvars = FALSE)

          chunks=min(length(preparation_tokens),self$bert_components$chunks)
          tokens_unit<-NULL
          for(j in 1:chunks){
            tokens_unit[j]<-list(
              self$bert_components$tokenizer(
                paste(preparation_tokens[j],collapse = " "),
                padding=TRUE,
                truncation=TRUE,
                max_length=as.integer(self$basic_components$max_length),
                return_tensors="tf")
            )
            if(trace==TRUE){
              print(paste(date(),i,"/",n_units,"block",j,"/",chunks))
            }
          }
          tokens[i]<-list(tokens_unit)
        }
        return(tokens)
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
        integer_token<-NULL
        for(i in 1:length(token)){
          integer_token[i]<-list(as.integer(as.vector(token[[i]])))
        }
        return(integer_token)
      }
    },
    #Embedding------------------------------------------------------------------
    embed=function(raw_text=NULL,doc_id=NULL, trace = FALSE){
      tokens<-self$tokenize(raw_text = raw_text,
                            trace = trace)
      n_units<-length(tokens)
      #bert---------------------------------------------------------------------
      if(self$basic_components$method=="bert"){
        n_layer<-self$bert_components$model$config$num_hidden_layers
        n_layer_size<-self$bert_components$model$config$hidden_size

        text_embedding<-matrix(
          nrow=n_units,
          ncol=n_layer_size*self$bert_components$chunks,
          data=0)


        for(i in 1:n_units){
          for(j in 1:length(tokens[[i]])){
            tmp_hidden_states<-self$bert_components$model(
              tokens[[i]][[j]],
              output_hidden_states=TRUE)$hidden_states

            if(self$bert_components$aggregation=="last"){
              #The first token is always the CLS token
              tmp_embedding<-tmp_hidden_states[[1+n_layer]]
              tmp_embedding<-tmp_embedding[[0]][[0]]
              tmp_embedding<-reticulate::array_reshape(
                tmp_embedding,dim=c(1,self$bert_components$model$config$hidden_size))
              tmp_embedding<-reticulate::py_to_r(tmp_embedding)
            } else if(self$bert_components$aggregation=="second_to_last"){
              tmp_embedding<-tmp_hidden_states[[1+n_layer-2]]
              tmp_embedding<-tmp_embedding[[0]][[0]]
              tmp_embedding<-reticulate::array_reshape(
                tmp_embedding,dim=c(1,self$bert_components$model$config$hidden_size))
              tmp_embedding<-reticulate::py_to_r(tmp_embedding)
            } else if(self$bert_components$aggregation=="fourth_to_last"){
              tmp_embedding<-tmp_hidden_states[[1+n_layer-4]]
              tmp_embedding<-tmp_embedding[[0]][[0]]
              tmp_embedding<-reticulate::array_reshape(
                tmp_embedding,dim=c(1,self$bert_components$model$config$hidden_size))
              tmp_embedding<-reticulate::py_to_r(tmp_embedding)
            } else if(self$bert_components$aggregation=="all"){
              tmp_embedding<-NULL
              tmp_embedding_sum<-NULL
              for(l in 2:(n_layer+1)){
                tmp_embedding_sum<-tmp_hidden_states[[l]]
                tmp_embedding_sum<-tmp_embedding_sum[[0]][[0]]
                tmp_embedding_sum<-reticulate::array_reshape(
                  tmp_embedding_sum,dim=c(1,self$bert_components$model$config$hidden_size))
                tmp_embedding_sum<-reticulate::py_to_r(tmp_embedding_sum)
                if(l==2){
                  tmp_embedding=tmp_embedding_sum
                } else {
                  tmp_embedding=tmp_embedding+tmp_embedding_sum
                }
              }
              tmp_embedding=tmp_embedding/n_layer

            } else if(self$bert_components$aggregation=="last_four"){
              tmp_embedding<-NULL
              tmp_embedding_sum<-NULL
              for(l in (n_layer-3):(n_layer+1)){
                tmp_embedding_sum<-tmp_hidden_states[[l]]
                tmp_embedding_sum<-tmp_embedding_sum[[0]][[0]]
                tmp_embedding_sum<-reticulate::array_reshape(
                  tmp_embedding_sum,dim=c(1,self$bert_components$model$config$hidden_size))
                tmp_embedding_sum<-reticulate::py_to_r(tmp_embedding_sum)
                if(l==(n_layer-3)){
                  tmp_embedding=tmp_embedding_sum
                } else {
                  tmp_embedding=tmp_embedding+tmp_embedding_sum
                }
              }
              tmp_embedding=tmp_embedding/4
            }
            text_embedding[i,(1:n_layer_size)+(j-1)*(1:n_layer_size)]<-tmp_embedding
            if(trace==TRUE){
              print(paste(date(),i,"/",n_units,"block",j,"/",length(tokens[[i]])))
            }
          }
          #text_embedding[i,]<-reticulate::py_to_r(
          #  reticulate::array_reshape(
          #    x=self$bert_components$model(
          #      tokens[[i]])[[1]][[0]][[0]],
          #    dim=c(1, n_layer_size)
          #  )
          #)
        }
        colnames(text_embedding)<-colnames(text_embedding,
                                           do.NULL = FALSE,
                                           prefix = "bert_")
        text_embedding<-as.data.frame(text_embedding)
        #Glove Cluster----------------------------------------------------------
      } else if(self$basic_components$method=="glove_cluster"){
        text_embedding<-matrix(nrow = length(tokens),
                               ncol =  self$bow_components$configuration$bow_n_cluster,
                               data = 0)
        for(i in 1:length(tokens)){
          token_freq<-table(tokens[[i]])
          tmp_tokens<-names(token_freq)
          for(j in 1:length(token_freq)){
            index<-match(x=as.integer(tmp_tokens[j]),
                  table = self$bow_components$model$index)
            text_embedding[i,self$bow_components$model$cluster[index]]<-token_freq[j]+
              text_embedding[i,self$bow_components$model$cluster[index]]
          }
        }
        colnames(text_embedding)<-colnames(text_embedding,
                                           do.NULL = FALSE,
                                           prefix = "cluster_")
        text_embedding<-as.data.frame(text_embedding)
        #Topic Modeling---------------------------------------------------------
      } else if(self$basic_components$method=="lda"){
        text_embedding<-matrix(nrow = length(tokens),
                               ncol =  self$bow_components$configuration$bow_n_dim,
                               data = 0)
        for(i in 1:length(tokens)){
          token_freq<-table(tokens[[i]])
          tmp_tokens<-names(token_freq)
          if(length(tmp_tokens)>0){
            for(j in 1:length(token_freq)){
              index<-match(x=as.integer(tmp_tokens[j]),
                           table = self$bow_components$model$index)
              if(is.na(index)==FALSE){
                text_embedding[i,]<-text_embedding[i,]+token_freq[j]*as.matrix(self$bow_components$model[index,-1])
              }
            }
          }
        }
        #text_embedding<-text_embedding/rowSums(self$bow_components$model[,-1])
        text_embedding<-text_embedding/rowSums(text_embedding)
        #Replace NaN with 0 which indicate that the rowsum is 0 and division ist not
        #possible
        text_embedding[is.nan(text_embedding)]<-0

        colnames(text_embedding)<-colnames(text_embedding,
                                           do.NULL = FALSE,
                                           prefix = "lda_")
        text_embedding<-as.data.frame(text_embedding)
      }

      rownames(text_embedding)<-doc_id

      if(self$basic_components$method=="bert"){
        embeddings<-EmbeddedText$new(
          model_name = private$model_info$model_name,
          model_date = private$model_info$model_date,
          model_method = self$basic_components$method,
          model_version = private$model_info$model_version,
          model_language = private$model_info$model_language,
          param_seq_length =self$basic_components$max_length,
          param_chunks = self$bert_components$chunks,
          param_overlap = self$bert_components$overlap,
          param_aggregation = self$bert_components$aggregation,
          embeddings = text_embedding
        )
      } else if(self$basic_components$method=="glove_cluster" |
                self$basic_components$method=="lda"){
        embeddings<-EmbeddedText$new(
          model_name = private$model_info$model_name,
          model_date = private$model_info$model_date,
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
    set_publication_info=function(type,
                                  autors,
                                  citation,
                                  url=NULL){
      if(type=="developer"){
        private$publication_info$developed_by$authors<-autors
        private$publication_info$developed_by$citation<-citation
        private$publication_info$developed_by$url<-url
      } else if(type=="trainer"){
        private$publication_info$trained_by$authors<-autors
        private$publication_info$trained_by$citation<-citation
        private$publication_info$trained_by$url<-url
      } else if(type=="modifier"){
        private$publication_info$modifided_by$authors<-autors
        private$publication_info$modifided_by$citation<-citation
        private$publication_info$modifided_by$url<-url
      }
     },
    #--------------------------------------------------------------------------
    get_publication_info=function(){
      return(private$publication_info)
    },
    #--------------------------------------------------------------------------
    set_license=function(license){
      private$model_info$model_license<-license
    },
    get_license=function(){
      return(private$model_info$model_license)
    },
    #--------------------------------------------------------------------------
    set_model_description=function(eng=NULL,native=NULL){
      if(!is.null(description)){
        private$model_description$eng=description
      }
      if(!is.null(description_native)){
        private$model_description$native=description
      }
    },
    get_model_description=function(){
      return(private$model_description)
    },
    #--------------------------------------------------------------------------
    get_model_info=function(){
      return(list(
        model_license=private$model_info$model_license,
        model_name=private$model_info$model_name,
        model_date=private$model_info$model_date,
        model_version=private$model_info$model_version,
        model_language=private$model_info$model_language
        )
        )
    }
  )
)


#'@title Embedded Text
#'
#'@export
EmbeddedText<-R6::R6Class(
  classname = "EmbeddedText",
  private = list(
    model_name=NA,
    model_date=NA,
    model_method=NA,
    model_version=NA,
    model_language=NA,
    param_seq_length=NA,
    param_overlap=NA,
    param_chunks=NA,
    param_aggregation=NA
  ),
  public = list(
    embeddings=NA,
    initialize=function(model_name,
                        model_date,
                        model_method,
                        model_version,
                        model_language,
                        param_seq_length,
                        param_chunks=NULL,
                        param_overlap=NULL,
                        param_aggregation=NULL,
                        embeddings){
      private$model_name = model_name
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
    get_model_info=function(){
      tmp<-list(model_name=private$model_name,
                model_date=private$model_date,
                model_method=private$model_method,
                model_version=private$model_version,
                model_language=private$model_language,
                param_seq_length=private$param_seq_length,
                param_chunks=private$param_chunks,
                param_overlap=private$param_overlap,
                param_aggregation=private$param_aggregation)
      return(tmp)
    }
  )
)

#'Combine Embedded Texts
#'
#'Function for combining embedded texts of the same model
#'
#'@export
combine_embeddings<-function(embeddings_list){
#Check for the right class
  for(i in 1:length(embeddings_list)){
    if(methods::isClass(where=embeddings_list[[i]],
                        Class="EmbeddedText")==TRUE){
      stop("All elements of the embeddings_list must be of class
           EmbeddedText.")
    }
  }
#Check for the right underlining embedding model
  for(i in 2:length(embeddings_list)){
    embedding_1<-embeddings_list[[i-1]]$get_model_info()
    embedding_2<-embeddings_list[[1]]$get_model_info()
    for(j in 1:length(embedding_2)){
      if(embedding_1[[j]]!=embedding_2[[j]]){
        stop("The models which created the embeddings are not similar
             accros all elements in embeddings_list. Please check
             the elements.")
      }
    }
  }
#Combine embeddings

  for(i in 1:length(embeddings_list)){
     if(i==1){
      combined_embeddings<-embeddings_list[[i]]$embeddings
    } else {
      combined_embeddings<-rbind(combined_embeddings,embeddings_list[[i]]$embeddings)
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
