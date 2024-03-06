#'@title Text embedding model
#'@description This \link[R6]{R6} class stores a text embedding model which can be
#'used to tokenize, encode, decode, and embed raw texts. The object provides a
#'unique interface for different text processing methods.
#'@return Objects of class \code{TextEmbeddingModel} transform raw texts into numerical
#'representations which can be used for downstream tasks. For this aim objects of this class
#'allow to tokenize raw texts, to encode tokens to sequences of integers, and to decode sequences
#'of integers back to tokens.
#'@family Text Embedding
#'@export
TextEmbeddingModel<-R6::R6Class(
  classname = "TextEmbeddingModel",
  private = list(
    r_package_versions=list(
      aifeducation=NA,
      reticulate=NA
    ),

    supported_transformers=c(
      "bert",
      "roberta",
      "longformer",
      "funnel",
      "deberta_v2"
    ),

    basic_components=list(
      method=NULL,
      max_length=NULL
    ),

    transformer_components=list(
      model=NULL,
      model_mlm=NULL,
      tokenizer=NULL,
      emb_layer_min=NULL,
      emb_layer_max=NULL,
      emb_pool_type=NULL,
      chunks=NULL,
      overlap=NULL,
      ml_framework=NULL),

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


    model_info=list(
      model_license=NA,
      model_name_root=NA,
      model_name=NA,
      model_label=NA,
      model_date=NA,
      model_version=NA,
      model_language=NA
    ),

    sustainability=list(
      sustainability_tracked=FALSE,
      track_log=NA
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
      keywords_native=NULL,
      license=NA
    )
  ),
  public = list(

    #'@field last_training ('list()')\cr
    #'List for storing the history and the results of the last training. This
    #'information will be overwritten if a new training is started.
    last_training=list(
      history=NULL
    ),

    #--------------------------------------------------------------------------
    #'@description Method for creating a new text embedding model
    #'@param model_name \code{string} containing the name of the new model.
    #'@param model_label \code{string} containing the label/title of the new model.
    #'@param model_version \code{string} version of the model.
    #'@param model_language \code{string} containing the language which the model
    #'represents (e.g., English).
    #'@param ml_framework \code{string} Framework to use for the model.
    #'\code{ml_framework="tensorflow"} for 'tensorflow' and \code{ml_framework="pytorch"}
    #'for 'pytorch'. Only relevant for transformer models.
    #'@param method \code{string} determining the kind of embedding model. Currently
    #'the following models are supported:
    #'\code{method="bert"} for Bidirectional Encoder Representations from Transformers (BERT),
    #'\code{method="roberta"} for A Robustly Optimized BERT Pretraining Approach (RoBERTa),
    #'\code{method="longformer"} for Long-Document Transformer,
    #'\code{method="funnel"} for Funnel-Transformer,
    #'\code{method="deberta_v2"} for Decoding-enhanced BERT with Disentangled Attention (DeBERTa V2),
    #'\code{method="glove"} for
    #'GlobalVector Clusters, and
    #'\code{method="lda"} for topic modeling. See
    #'details for more information.
    #'@param max_length \code{int} determining the maximum length of token
    #'sequences used in transformer models. Not relevant for the other methods.
    #'@param chunks \code{int} Maximum number of chunks. Only relevant for
    #'transformer models.
    #'@param overlap \code{int} determining the number of tokens which should be added
    #'at the beginning of the next chunk. Only relevant for BERT models.
    #'@param emb_layer_min \code{int} or \code{string} determining the first layer to be included
    #'in the creation of embeddings. An integer correspondents to the layer number. The first
    #'layer has the number 1. Instead of an integer the following strings are possible:
    #'\code{"start"} for the first layer, \code{"middle"} for the middle layer,
    #'\code{"2_3_layer"} for the layer two-third layer, and \code{"last"} for the last layer.
    #'@param emb_layer_max \code{int} or \code{string} determining the last layer to be included
    #'in the creation of embeddings. An integer correspondents to the layer number. The first
    #'layer has the number 1. Instead of an integer the following strings are possible:
    #'\code{"start"} for the first layer, \code{"middle"} for the middle layer,
    #'\code{"2_3_layer"} for the layer two-third layer, and \code{"last"} for the last layer.
    #'@param emb_pool_type \code{string} determining the method for pooling the token embeddings
    #'within each layer. If \code{"cls"} only the embedding of the CLS token is used. If
    #'\code{"average"} the token embedding of all tokens are averaged (excluding padding tokens).
    #'@param model_dir \code{string} path to the directory where the
    #'BERT model is stored.
    #'@param bow_basic_text_rep object of class \code{basic_text_rep} created via
    #'the function \link{bow_pp_create_basic_text_rep}. Only relevant for \code{method="glove_cluster"}
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
    #'@return Returns an object of class \link{TextEmbeddingModel}.
    #'@details \itemize{
    #'\item{method: In the case of \code{method="bert"}, \code{method="roberta"}, and \code{method="longformer"},
    #'a pretrained transformer model
    #'must be supplied via \code{model_dir}. For \code{method="glove"}
    #'and \code{method="lda"} a new model will be created based on the data provided
    #'via \code{bow_basic_text_rep}. The original algorithm for GlobalVectors provides
    #'only word embeddings, not text embeddings. To achieve text embeddings the words
    #'are clustered based on their word embeddings with kmeans.}
    #'
    #'}
    #'@import reticulate
    #'@import stats
    #'@import reshape2
    initialize=function(model_name=NULL,
                        model_label=NULL,
                        model_version=NULL,
                        model_language=NULL,
                        method=NULL,
                        ml_framework=aifeducation_config$get_framework()$TextEmbeddingFramework,
                        max_length=0,
                        chunks=1,
                        overlap=0,
                        emb_layer_min="middle",
                        emb_layer_max="2_3_layer",
                        emb_pool_type="average",
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

      if((method %in% c("glove_cluster","lda"))==FALSE){
        if((ml_framework %in% c("tensorflow","pytorch"))==FALSE) {
          stop("ml_framework must be 'tensorflow' or 'pytorch'.")
        }
      }
      if(method=="funnel" & emb_pool_type!="cls"){
        stop("Funnel currently supports only cls as pooling type.")
      }

      #------------------------------------------------------------------------
      private$r_package_versions$aifeducation<-packageVersion("aifeducation")
      private$r_package_versions$reticulate<-packageVersion("reticulate")

      #Load Sustainability Data-----------------------------------------------
      if(!(method %in% c("lda","glove_cluster"))==TRUE){
        sustainability_datalog_path=paste0(model_dir,"/","sustainability.csv")
        if(file.exists(sustainability_datalog_path)){
          tmp_sustainability_data<-read.csv(sustainability_datalog_path)
          private$sustainability$sustainability_tracked=TRUE
          private$sustainability$track_log=tmp_sustainability_data
        } else {
          private$sustainability$sustainability_tracked=FALSE
          private$sustainability$track_log=NA
        }
      }

      #basic_components-------------------------------------------------------
      private$basic_components$method=method
      private$basic_components$max_length=as.integer(max_length)
      #------------------------------------------------------------------------
      if((private$basic_components$method %in%private$supported_transformers)==TRUE){
        private$transformer_components$ml_framework=ml_framework
        private$transformer_components$chunks<-chunks
        private$transformer_components$overlap<-overlap

        if(emb_pool_type%in%c("cls","average")==FALSE){
          stop("emb_pool_type must be 'cls' or 'average'.")
        }

        #Search for the corresponding files
        if(private$transformer_components$ml_framework=="tensorflow"){
          if(file.exists(paste0(model_dir,"/tf_model.h5"))){
            from_pt=FALSE
          } else if (file.exists(paste0(model_dir,"/pytorch_model.bin"))|
                     file.exists(paste0(model_dir,"/model.safetensors"))){
            from_pt=TRUE
          } else {
            stop("Directory does not contain a tf_model.h5, pytorch_model.bin
                 or a model.saftensors file.")
          }
        } else {
          if(file.exists(paste0(model_dir,"/pytorch_model.bin"))|
             file.exists(paste0(model_dir,"/model.safetensors"))){
            from_tf=FALSE
          } else if (file.exists(paste0(model_dir,"/tf_model.h5"))){
            from_tf=TRUE
          } else {
            stop("Directory does not contain a tf_model.h5,pytorch_model.bin
                 or a model.saftensors file.")
          }
        }

        #In the case of pytorch
        #Check to load from pt/bin or safetensors
        #Use safetensors as preferred method
        if(ml_framework=="pytorch"){
          if((file.exists(paste0(model_dir,"/model.safetensors"))==FALSE &
              from_tf==FALSE)|
             reticulate::py_module_available("safetensors")==FALSE){
            load_safe=FALSE
          } else {
            load_safe=TRUE
          }
        }

        if(private$basic_components$method=="bert"){
          private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir)
          if(ml_framework=="tensorflow"){
            private$transformer_components$model<-transformers$TFBertModel$from_pretrained(model_dir,from_pt=from_pt)
            private$transformer_components$model_mlm<-transformers$TFBertForMaskedLM$from_pretrained(model_dir,from_pt=from_pt)
          } else {
            private$transformer_components$model<-transformers$BertModel$from_pretrained(model_dir,
                                                                                         from_tf=from_tf,
                                                                                         use_safetensors=load_safe)
            private$transformer_components$model_mlm<-transformers$BertForMaskedLM$from_pretrained(model_dir,
                                                                                                   from_tf=from_tf,
                                                                                                   use_safetensors=load_safe)
          }
        } else if(private$basic_components$method=="roberta"){
          private$transformer_components$tokenizer<-transformers$RobertaTokenizerFast$from_pretrained(model_dir)
          if(ml_framework=="tensorflow"){
            private$transformer_components$model<-transformers$TFRobertaModel$from_pretrained(model_dir,from_pt=from_pt)
            private$transformer_components$model_mlm<-transformers$TFRobertaForMaskedLM$from_pretrained(model_dir,from_pt=from_pt)
          } else {
            private$transformer_components$model<-transformers$RobertaModel$from_pretrained(model_dir,
                                                                                            from_tf=from_tf,
                                                                                            use_safetensors=load_safe)
            private$transformer_components$model_mlm<-transformers$RobertaForMaskedLM$from_pretrained(model_dir,
                                                                                                      from_tf=from_tf,
                                                                                                      use_safetensors=load_safe)
          }

        } else if(private$basic_components$method=="longformer"){
          private$transformer_components$tokenizer<-transformers$LongformerTokenizerFast$from_pretrained(model_dir)
          if(ml_framework=="tensorflow"){
            private$transformer_components$model<-transformers$TFLongformerModel$from_pretrained(model_dir,from_pt=from_pt)
            private$transformer_components$model_mlm<-transformers$TFLongformerForMaskedLM$from_pretrained(model_dir,from_pt=from_pt)
          } else {
            private$transformer_components$model<-transformers$LongformerModel$from_pretrained(model_dir,
                                                                                               from_tf=from_tf,
                                                                                               use_safetensors=load_safe)
            private$transformer_components$model_mlm<-transformers$LongformerForMaskedLM$from_pretrained(model_dir,
                                                                                                         from_tf=from_tf,
                                                                                                         use_safetensors=load_safe)
          }
        } else if(private$basic_components$method=="funnel"){
          private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir)
          if(ml_framework=="tensorflow"){
            private$transformer_components$model<-transformers$TFFunnelBaseModel$from_pretrained(model_dir,from_pt=from_pt)
            private$transformer_components$model_mlm<-transformers$TFFunnelForMaskedLM$from_pretrained(model_dir,from_pt=from_pt)
          } else {
            private$transformer_components$model<-transformers$FunnelBaseModel$from_pretrained(model_dir,
                                                                                               from_tf=from_tf,
                                                                                               use_safetensors=load_safe)
            private$transformer_components$model_mlm<-transformers$FunnelForMaskedLM$from_pretrained(model_dir,
                                                                                                     from_tf=from_tf,
                                                                                                     use_safetensors=load_safe)
          }
        } else if(private$basic_components$method=="deberta_v2"){
          private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir)
          if(ml_framework=="tensorflow"){
            private$transformer_components$model<-transformers$TFDebertaV2Model$from_pretrained(model_dir,from_pt=from_pt)
            private$transformer_components$model_mlm<-transformers$TFDebertaForMaskedLM$from_pretrained(model_dir,from_pt=from_pt)
          } else {
            private$transformer_components$model<-transformers$DebertaV2Model$from_pretrained(model_dir,
                                                                                              from_tf=from_tf,
                                                                                              use_safetensors=load_safe)
            private$transformer_components$model_mlm<-transformers$DebertaForMaskedLM$from_pretrained(model_dir,
                                                                                                      from_tf=from_tf,
                                                                                                      use_safetensors=load_safe)
          }
        } else if(private$basic_components$method=="rwkv"){
          private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir)
          if(ml_framework=="tensorflow"){
            #private$transformer_components$model<-transformers$TFDebertaV2ForMaskedLM$from_pretrained(model_dir,from_pt=from_pt)
          } else {
            private$transformer_components$model<-transformers$RwkvForCausalLM$from_pretrained(model_dir,
                                                                                               from_tf=from_tf,
                                                                                               use_safetensors=load_safe)
          }
        }

        if(private$basic_components$method=="longformer" |
           private$basic_components$method=="roberta"){
          if(max_length>(private$transformer_components$model$config$max_position_embeddings)){
            stop(paste("max_length is",max_length,". This value is not allowed to exceed",
                       private$transformer_components$model$config$max_position_embeddings))
          }
        }

        #Sustainability tracking
        sustainability_datalog_path=paste0(model_dir,"/","sustainability.csv")
        if(file.exists(sustainability_datalog_path)){
          tmp_sustainability_data<-read.csv(sustainability_datalog_path)
          private$sustainability$sustainability_tracked=TRUE
          private$sustainability$track_log=tmp_sustainability_data
        } else {
          private$sustainability$sustainability_tracked=FALSE
          private$sustainability$track_log=NA
        }

        #Training history
        training_datalog_path=paste0(model_dir,"/","history.log")
        if(file.exists(training_datalog_path)==TRUE){
          self$last_training$history=read.csv2(file = training_datalog_path)
        } else {
          self$last_training$history=NA
        }


        #Check Embedding Configuration
        if(method=="funnel"){
          max_layers_funnel=sum(private$transformer_components$model$config$block_repeats*
            private$transformer_components$model$config$block_sizes)

          if(emb_layer_min=="first"){
            emb_layer_min=1
          } else if (emb_layer_min=="middle"){
            emb_layer_min=floor(0.5*max_layers_funnel)
          } else if(emb_layer_min=="2_3_layer"){
            emb_layer_min=floor(2/3*max_layers_funnel)
          } else if (emb_layer_min=="last"){
            emb_layer_min=max_layers_funnel
          }

          if(emb_layer_max=="first"){
            emb_layer_max=1
          } else if (emb_layer_max=="middle"){
            emb_layer_max=floor(0.5*max_layers_funnel)
          } else if(emb_layer_max=="2_3_layer"){
            emb_layer_max=floor(2/3*max_layers_funnel)
          } else if (emb_layer_max=="last"){
            emb_layer_max=max_layers_funnel
          }

        } else {
          if(emb_layer_min=="first"){
            emb_layer_min=1
          } else if (emb_layer_min=="middle"){
            emb_layer_min=floor(0.5*private$transformer_components$model$config$num_hidden_layers)
          } else if(emb_layer_min=="2_3_layer"){
            emb_layer_min=floor(2/3*private$transformer_components$model$config$num_hidden_layers)
          } else if (emb_layer_min=="last"){
            emb_layer_min=private$transformer_components$model$config$num_hidden_layers
          }

          if(emb_layer_max=="first"){
            emb_layer_max=1
          } else if (emb_layer_max=="middle"){
            emb_layer_max=floor(0.5*private$transformer_components$model$config$num_hidden_layers)
          } else if(emb_layer_max=="2_3_layer"){
            emb_layer_max=floor(2/3*private$transformer_components$model$config$num_hidden_layers)
          } else if (emb_layer_max=="last"){
            emb_layer_max=private$transformer_components$model$config$num_hidden_layers
          }

        }

        if(emb_layer_min>emb_layer_max){
          stop("emb_layer_min layer must be smaller or equal emb_layer_max.")
        }
        if(emb_layer_min<1){
          stop("emb_laser_min must be at least 1.")
        }
        if(method=="funnel"){
          if(emb_layer_max>private$transformer_components$model$config$num_hidden_layers){
            stop(paste0("emb_layer_max can not exceed the number of layers. The transformer has",
                        max_layers_funnel,"layers."))
          }
        } else {
          if(emb_layer_max>private$transformer_components$model$config$num_hidden_layers){
            stop(paste0("emb_layer_max can not exceed the number of layers. The transformer has",
                        private$transformer_components$model$config$num_hidden_layers,"layers."))
          }
        }

        if(is.integer(as.integer(emb_layer_min))==FALSE | is.integer(as.integer(emb_layer_max))==FALSE){
          stop("emb_layer_min and emb_layer_max must be integers or the following string:
               'first','last','middle','2_3_layer'")
        }

        private$transformer_components$emb_layer_min=emb_layer_min
        private$transformer_components$emb_layer_max=emb_layer_max
        private$transformer_components$emb_pool_type=emb_pool_type

        #------------------------------------------------------------------------
      } else if(private$basic_components$method=="glove_cluster"){
        requireNamespace(package="text2vec")

        glove <- text2vec::GlobalVectors$new(rank = bow_n_dim,
                                             x_max = 10
        )
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
        private$bow_components$model=model
        private$bow_components$vocab=bow_basic_text_rep$language_model$vocab
        private$bow_components$configuration$to_lower=bow_basic_text_rep$configuration$to_lower
        private$bow_components$configuration$use_lemmata=bow_basic_text_rep$configuration$use_lemmata
        private$bow_components$configuration$bow_n_dim=bow_n_dim
        private$bow_components$configuration$bow_n_cluster=bow_n_cluster
        private$bow_components$configuration$bow_max_iter=bow_max_iter
        private$bow_components$configuration$bow_max_iter_cluster=bow_max_iter_cluster
        private$bow_components$configuration$bow_cr_criterion=bow_cr_criterion
        private$bow_components$configuration$bow_learning_rate=bow_learning_rate
        private$bow_components$chunks=1
        private$bow_components$overlap=0

        #Topic Modeling--------------------------------------------------------
      } else if(private$basic_components$method=="lda"){
        requireNamespace(package="quanteda")
        requireNamespace(package="topicmodels")
        requireNamespace(package="tidytext")

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

      private$bow_components$model=model
      private$bow_components$vocab=bow_basic_text_rep$language_model$vocab
      private$bow_components$configuration$to_lower=bow_basic_text_rep$configuration$to_lower
      private$bow_components$configuration$use_lemmata=bow_basic_text_rep$configuration$use_lemmata
      private$bow_components$configuration$bow_n_dim=bow_n_dim
      private$bow_components$configuration$bow_n_cluster=bow_n_cluster
      private$bow_components$configuration$bow_max_iter=bow_max_iter
      private$bow_components$configuration$bow_max_iter_cluster=bow_max_iter_cluster
      private$bow_components$configuration$bow_cr_criterion=bow_cr_criterion
      private$bow_components$configuration$bow_learning_rate=bow_learning_rate
      private$bow_components$chunks=1
      private$bow_components$overlap=0
      }

      private$model_info$model_name_root<-model_name
      private$model_info$model_name<-paste0(model_name,"_Id_",generate_id(16))
      private$model_info$model_label<-model_label
      private$model_info$model_version<-model_version
      private$model_info$model_language<-model_language
      private$model_info$model_date<-date()
    },
    #--------------------------------------------------------------------------
    #'@description Method for loading a transformers model into R.
    #'@param model_dir \code{string} containing the path to the relevant
    #'model directory.
    #'@param ml_framework \code{string} Determines the machine learning framework
    #'for using the model. Possible are \code{ml_framework="pytorch"} for 'pytorch',
    #'\code{ml_framework="tensorflow"} for 'tensorflow', and \code{ml_framework="auto"}.
    #'@return Function does not return a value. It is used for loading a saved
    #'transformer model into the R interface.
    #'
    #'@importFrom utils read.csv
    load_model=function(model_dir,
                        ml_framework="auto"){

      if((ml_framework %in% c("tensorflow","pytorch","auto","not_specified"))==FALSE) {
        stop("ml_framework must be 'auto', 'tensorflow' or 'pytorch'.")
      }

      if(ml_framework=="not_specified"){
        stop("The global machine learning framework is not set. Please use
             aifeducation_config$set_global_ml_backend() directly after loading
             the library to set the global framework. ")
      }

      model_dir_main<-paste0(model_dir,"/","model_data")

      #Change ml framework if requested
      if(ml_framework=="tensorflow"){
        private$transformer_components$ml_framework="tensorflow"
      } else if(ml_framework=="pytorch"){
        private$transformer_components$ml_framework="pytorch"
      }

      #Search for the corresponding files
      if(private$transformer_components$ml_framework=="tensorflow"){
        if(file.exists(paste0(model_dir_main,"/tf_model.h5"))){
          from_pt=FALSE
        } else if (file.exists(paste0(model_dir_main,"/pytorch_model.bin"))|
                   file.exists(paste0(model_dir_main,"/model.safetensors"))){
          from_pt=TRUE
        } else {
          stop("Directory does not contain a tf_model.h5 or pytorch_model.bin
               or a model.safetensors file.")
        }
      } else if(private$transformer_components$ml_framework=="pytorch"){
        if(file.exists(paste0(model_dir_main,"/pytorch_model.bin"))|
           file.exists(paste0(model_dir_main,"/model.safetensors"))){
          from_tf=FALSE
        } else if (file.exists(paste0(model_dir_main,"/tf_model.h5"))){
          from_tf=TRUE
        } else {
          stop("Directory does not contain a tf_model.h5 or pytorch_model.bin
               or a model.safetensors file.")
        }
      } else {
        from_pt=FALSE
        from_tf=FALSE
      }

      #In the case of pytorch
      #Check to load from pt/bin or safetensors
      #Use safetensors as preferred method
      if(ml_framework=="pytorch"){
        if((file.exists(paste0(model_dir_main,"/model.safetensors"))==FALSE &
            from_tf==FALSE)|
           reticulate::py_module_available("safetensors")==FALSE){
          load_safe=FALSE
        } else {
          load_safe=TRUE
        }
      }


        if((private$basic_components$method %in%private$supported_transformers)==TRUE){

          if(private$basic_components$method=="bert"){
            private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir_main)
            if(private$transformer_components$ml_framework=="tensorflow"){
              private$transformer_components$model<-transformers$TFBertModel$from_pretrained(model_dir_main,from_pt=from_pt)
              private$transformer_components$model_mlm<-transformers$TFBertForMaskedLM$from_pretrained(model_dir_main,from_pt=from_pt)
            } else {
              private$transformer_components$model<-transformers$BertModel$from_pretrained(model_dir_main,
                                                                                           from_tf=from_tf,
                                                                                           use_safetensors=load_safe)
              private$transformer_components$model_mlm<-transformers$BertForMaskedLM$from_pretrained(model_dir_main,
                                                                                                     from_tf=from_tf,
                                                                                                     use_safetensors=load_safe)
            }
          } else if(private$basic_components$method=="roberta"){
            private$transformer_components$tokenizer<-transformers$RobertaTokenizerFast$from_pretrained(model_dir_main)
            if(private$transformer_components$ml_framework=="tensorflow"){
              private$transformer_components$model<-transformers$TFRobertaModel$from_pretrained(model_dir_main,from_pt=from_pt)
              private$transformer_components$model_mlm<-transformers$TFRobertaForMaskedLM$from_pretrained(model_dir_main,from_pt=from_pt)
            } else {
              private$transformer_components$model<-transformers$RobertaModel$from_pretrained(model_dir_main,
                                                                                              from_tf=from_tf,
                                                                                              use_safetensors=load_safe)
              private$transformer_components$model_mlm<-transformers$RobertaForMaskedLM$from_pretrained(model_dir_main,
                                                                                                        from_tf=from_tf,
                                                                                                        use_safetensors=load_safe)
            }
          } else if(private$basic_components$method=="longformer"){
            private$transformer_components$tokenizer<-transformers$LongformerTokenizerFast$from_pretrained(model_dir_main)
            if(private$transformer_components$ml_framework=="tensorflow"){
              private$transformer_components$model<-transformers$TFLongformerModel$from_pretrained(model_dir_main,from_pt=from_pt)
              private$transformer_components$model_mlm<-transformers$TFLongformerForMaskedLM$from_pretrained(model_dir_main,from_pt=from_pt)
            } else {
              private$transformer_components$model<-transformers$LongformerModel$from_pretrained(model_dir_main,
                                                                                                 from_tf=from_tf,
                                                                                                 use_safetensors=load_safe)
              private$transformer_components$model_mlm<-transformers$LongformerForMaskedLM$from_pretrained(model_dir_main,
                                                                                                           from_tf=from_tf,
                                                                                                           use_safetensors=load_safe)
            }
          }  else if(private$basic_components$method=="funnel"){
            private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir_main)
            if(private$transformer_components$ml_framework=="tensorflow"){
              private$transformer_components$model<-transformers$TFFunnelBaseModel$from_pretrained(model_dir_main,from_pt=from_pt)
              private$transformer_components$model_mlm<-transformers$TFFunnelForMaskedLM$from_pretrained(model_dir_main,from_pt=from_pt)
            } else {
              private$transformer_components$model<-transformers$FunnelBaseModel$from_pretrained(model_dir_main,
                                                                                                 from_tf=from_tf,
                                                                                                 use_safetensors=load_safe)
              private$transformer_components$model_mlm<-transformers$FunnelForMaskedLM$from_pretrained(model_dir_main,
                                                                                                       from_tf=from_tf,
                                                                                                       use_safetensors=load_safe)
            }
          } else if(private$basic_components$method=="deberta_v2"){
            private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir_main)
            if(private$transformer_components$ml_framework=="tensorflow"){
              private$transformer_components$model<-transformers$TFDebertaV2Model$from_pretrained(model_dir_main,from_pt=from_pt)
              private$transformer_components$model_mlm<-transformers$TFDebertaV2ForMaskedLM$from_pretrained(model_dir_main,from_pt=from_pt)
            } else {
              private$transformer_components$model<-transformers$DebertaV2Model$from_pretrained(model_dir_main,
                                                                                                from_tf=from_tf,
                                                                                                use_safetensors=load_safe)
              private$transformer_components$model_mlm<-transformers$DebertaV2ForMaskedLM$from_pretrained(model_dir_main,
                                                                                                        from_tf=from_tf,
                                                                                                        use_safetensors=load_safe)
            }
          } else if(private$basic_components$method=="rwkv"){
            private$transformer_components$tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir_main)
            if(private$transformer_components$ml_framework=="tensorflow"){
              #private$transformer_components$model<-transformers$TFDebertaV2ForMaskedLM$from_pretrained(model_dir_main,from_pt=from_pt)
            } else {
              private$transformer_components$model<-transformers$RwkvForCausalLM$from_pretrained(model_dir_main,
                                                                                                 from_tf=from_tf,
                                                                                                 use_safetensors=load_safe)
            }
          }

          #Sustainability Data
          sustainability_datalog_path=paste0(model_dir,"/","sustainability.csv")
          if(file.exists(sustainability_datalog_path)){
            tmp_sustainability_data<-read.csv(sustainability_datalog_path)
            private$sustainability$sustainability_tracked=TRUE
            private$sustainability$track_log=tmp_sustainability_data
          } else {
            private$sustainability$sustainability_tracked=FALSE
            private$sustainability$track_log=NA
          }

          #Training History
          training_datalog_path=paste0(model_dir,"/","history.log")
          if(file.exists(training_datalog_path)){
            self$last_training$history=read.csv2(file = training_datalog_path)
          } else {
            self$last_training$history=NULL
          }


      } else {
        message("Method only relevant for transformer models.")
      }
    },
    #'@description Method for saving a transformer model on disk.Relevant
    #'only for transformer models.
    #'@param model_dir \code{string} containing the path to the relevant
    #'model directory.
    #'@param save_format Format for saving the model. For 'tensorflow'/'keras' models
    #' \code{"h5"} for HDF5.
    #'For 'pytorch' models \code{"safetensors"} for 'safetensors' or
    #'\code{"pt"} for 'pytorch' via pickle.
    #'Use \code{"default"} for the standard format. This is h5 for
    #''tensorflow'/'keras' models and safetensors for 'pytorch' models.
    #'@return Function does not return a value. It is used for saving a transformer model
    #'to disk.
    #'
    #'@importFrom utils write.csv
    save_model=function(model_dir,save_format="default"){
      if((private$basic_components$method %in%private$supported_transformers)==TRUE){

      if(save_format%in%c("default","h5","pt","safetensors")==FALSE){
        stop("For TextEmbeddingModels save_format must be 'h5', 'pt', or 'safetensors'.")
      }

      if(save_format=="default"){
        if(private$transformer_components$ml_framework=="tensorflow"){
          save_format="h5"
        } else if(private$transformer_components$ml_framework=="pytorch"){
          save_format="safetensors"
        }
      }

      model_dir_data_path<-paste0(model_dir,"/","model_data")

      if(dir.exists(model_dir)==FALSE){
        dir.create(model_dir)
        cat("Creating Directory\n")
      }

      if(private$transformer_components$ml_framework=="pytorch"){
        if(save_format=="safetensors" & reticulate::py_module_available("safetensors")==TRUE){

          private$transformer_components$model$save_pretrained(save_directory=model_dir_data_path,
                                                               safe_serilization=TRUE)
          private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)

        } else if (save_format=="safetensors" & reticulate::py_module_available("safetensors")==FALSE){
          private$transformer_components$model$save_pretrained(save_directory=model_dir_data_path,
                                                               safe_serilization=FALSE)
          private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)
          warning("Python library 'safetensors' is not available. Saving model in standard
                  pytorch format.")
        } else if (save_format=="pt"){
          private$transformer_components$model$save_pretrained(save_directory=model_dir_data_path,
                                                               safe_serilization=FALSE)
          private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)
        }
      } else {
        private$transformer_components$model$save_pretrained(save_directory=model_dir_data_path)
        private$transformer_components$tokenizer$save_pretrained(model_dir_data_path)
      }

      #Saving Sustainability Data
      sustain_matrix=private$sustainability$track_log
      write.csv(
        x=sustain_matrix,
        file=paste0(model_dir,"/","sustainability.csv"),
        row.names = FALSE
      )

      #Saving training history
      if(is.null_or_na(self$last_training$history)==FALSE){
        write.csv2(
          x=self$last_training$history,
          file=paste0(model_dir,"/","history.log"),
          row.names = FALSE,
          quote = FALSE)
      }

      } else {
        message("Method only relevant for transformer models.")
      }
    },
    #-------------------------------------------------------------------------
    #'@description Method for encoding words of raw texts into integers.
    #'@param raw_text \code{vector} containing the raw texts.
    #'@param token_encodings_only \code{bool} If \code{TRUE}, only the token
    #'encodings are returned. If \code{FALSE}, the complete encoding is returned
    #'which is important for BERT models.
    #'@param to_int \code{bool} If \code{TRUE} the integer ids of the tokens are
    #'returned. If \code{FALSE} the tokens are returned. Argument only applies
    #'for transformer models and if \code{token_encodings_only==TRUE}.
    #'@param trace \code{bool} If \code{TRUE}, information of the progress
    #'is printed. \code{FALSE} if not requested.
    #'@return \code{list} containing the integer sequences of the raw texts with
    #'special tokens.
    encode=function(raw_text,
                    token_encodings_only=FALSE,
                    to_int=TRUE,
                    trace = FALSE){
      n_units<-length(raw_text)

      if((private$basic_components$method %in%private$supported_transformers)==TRUE){
        chunk_list<-vector(length = n_units)
        encodings<-NULL
        #---------------------------------------------------------------------
        if(token_encodings_only==TRUE){
          encodings_only=NULL
          for(i in 1:n_units){

            tokens_unit<-NULL

            tokens<-private$transformer_components$tokenizer(
              raw_text[i],
              stride=as.integer(private$transformer_components$overlap),
              padding="max_length",
              truncation=TRUE,
              return_overflowing_tokens = TRUE,
              return_length = FALSE,
              return_offsets_mapping = FALSE,
              return_attention_mask = FALSE,
              max_length=as.integer(private$basic_components$max_length),
              return_tensors="np")

            seq_len=nrow(tokens[["input_ids"]])

            chunks=min(seq_len,private$transformer_components$chunks)

            for(j in 1:chunks){
              tokens_unit[j]<-list(tokens["input_ids"][j,])
              if(trace==TRUE){
                cat(paste(date(),i,"/",n_units,"block",j,"/",chunks,"\n"))
              }
            }
            encodings_only[i]<-list(tokens_unit)
          }
          if(to_int==TRUE){
            return(encodings_only)
          } else {
            #Convert ids to tokens
            if((private$basic_components$method %in%private$supported_transformers)==TRUE){
              token_seq_list=NULL
              for(i in 1:length(encodings_only)){
                tmp_sequence=encodings_only[[i]]
                tmp_seqeunce_tok=NULL
                for(j in 1:length(tmp_sequence)){
                  tmp_seqeunce_tok[length(tmp_seqeunce_tok)+1]=list(
                    private$transformer_components$tokenizer$convert_ids_to_tokens(
                      ids=as.integer(tmp_sequence[[j]]),skip_special_tokens=FALSE)
                  )
                }
                token_seq_list[length(token_seq_list)+1]=list(tmp_seqeunce_tok)
              }
              return(token_seq_list)
            }
          }

          #--------------------------------------------------------------------
        } else {
          #text_chunks<-NULL
          encodings<-NULL
          for(i in 1:n_units){

          if(private$transformer_components$ml_framework=="tensorflow"){
            tokens=private$transformer_components$tokenizer(
              raw_text[i],
              stride=as.integer(private$transformer_components$overlap),
              padding="max_length",
              truncation=TRUE,
              max_length=as.integer(private$basic_components$max_length),
              return_overflowing_tokens = TRUE,
              return_length = FALSE,
              return_offsets_mapping = FALSE,
              return_attention_mask = TRUE,
              return_token_type_ids=TRUE,
              return_tensors="tf")
            } else {
              tokens=private$transformer_components$tokenizer(
                raw_text[i],
                stride=as.integer(private$transformer_components$overlap),
                padding="max_length",
                truncation=TRUE,
                max_length=as.integer(private$basic_components$max_length),
                return_overflowing_tokens = TRUE,
                return_length = FALSE,
                return_offsets_mapping = FALSE,
                return_attention_mask = TRUE,
                return_token_type_ids=TRUE,
                return_tensors="pt")
            }

            tmp_dataset=datasets$Dataset$from_dict(tokens)

            seq_len=tmp_dataset$num_rows
            chunk_list[i]=min(seq_len,private$transformer_components$chunks)

            if(chunk_list[i]==1){
              encodings=datasets$concatenate_datasets(c(encodings,tmp_dataset))
            } else {
              tmp_dataset=tmp_dataset$select(as.integer((1:chunk_list[[i]])-1))
              encodings=datasets$concatenate_datasets(c(encodings,tmp_dataset))
            }
          }
          #print(encodings["input_ids"])
          return(encodings_list=list(encodings=encodings,
                                     chunks=chunk_list))
        }
      } else if(private$basic_components$method=="glove_cluster"|
                private$basic_components$method=="lda"){

        requireNamespace(package="quanteda")

        textual_corpus <-quanteda::corpus(raw_text)
        token<-quanteda::tokens(textual_corpus)
        if(private$bow_components$configuration$use_lemmata==TRUE){
          if(private$bow_components$configuration$to_lower==TRUE){
            token<-quanteda::tokens_keep(x=token,
                                         pattern = private$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = private$bow_components$vocab$token,
                                            replacement = as.character(private$bow_components$vocab$index_lemma_lower),
                                            valuetype = "fixed",
                                            verbose=verbose)
          } else {
            token<-quanteda::tokens_keep(x=token,
                                         pattern = private$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = private$bow_components$vocab$token,
                                            replacement = as.character(private$bow_components$vocab$index_lemma),
                                            valuetype = "fixed",
                                            verbose=verbose)
          }
        } else {
          if(private$bow_components$configuration$to_lower==TRUE){
            token<-quanteda::tokens_keep(x=token,
                                         pattern = private$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = private$bow_components$vocab$token,
                                            replacement = as.character(private$bow_components$vocab$index_token_lower),
                                            valuetype = "fixed",
                                            verbose=verbose)
          } else {
            token<-quanteda::tokens_keep(x=token,
                                         pattern = private$bow_components$vocab$token)
            token<-quanteda::tokens_replace(x=token,
                                            pattern = private$bow_components$vocab$token,
                                            replacement = as.character(private$bow_components$vocab$index_token),
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
    #'should be transformed to tokens or plain text.
    #'@param to_token \code{bool} If \code{FALSE} a plain text is returned.
    #'if \code{TRUE} a sequence of tokens is returned. Argument only relevant
    #'if the model is based on a transformer.
    #'
    #'@return \code{list} of token sequences
    decode=function(int_seqence,to_token=FALSE){

      if(!is.list(int_seqence)){
        tmp=NULL
        tmp[1]=list(int_seqence)
        int_seqence=tmp[1]
      }
      #-------------------------------------------------------------------------
      if((private$basic_components$method %in%private$supported_transformers)==TRUE){
        tmp_token_list=NULL
        for(i in 1:length(int_seqence)){
          tmp_seq_token_list=NULL
          for(j in 1:length(int_seqence[[i]])){
          tmp_vector<-int_seqence[[i]][[j]]
          mode(tmp_vector)="integer"
          if(to_token==FALSE){
            tmp_seq_token_list[j]=list(private$transformer_components$tokenizer$decode(
              token_ids=tmp_vector,
              skip_special_tokens=TRUE))
          } else {
            tmp_seq_token_list[j]=list(private$transformer_components$tokenizer$convert_ids_to_tokens(tmp_vector))
          }
          }
          tmp_token_list[i]=list(tmp_seq_token_list)
        }
        return(tmp_token_list)

      #-------------------------------------------------------------------------
      } else if(private$basic_components$method=="glove_cluster" |
                private$basic_components$method=="lda"){
        if(private$bow_components$configuration$to_lower==TRUE){
          if(private$bow_components$configuration$use_lemmata==FALSE){
            input_column="index_token_lower"
            target_coumn="token_tolower"
          } else {
            input_column="index_lemma_lower"
            target_coumn="lemma_tolower"
          }
        } else {
          if(private$bow_components$configuration$use_lemmata==FALSE){
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
                        table=private$bow_components$vocab[,input_column])
                        #table=global_vector_clusters_modeling$bow_components$vocab[,input_column])
            tmp_token_seq[j]=private$bow_components$vocab[index,target_coumn]
            #tmp_token_seq[j]=global_vector_clusters_modeling$bow_components$vocab[index,target_coumn]
          }
          tmp_token_list[i]=list(tmp_token_seq)
        }
        return(tmp_token_list)
      }
    },
    #'@description Method for receiving the special tokens of the model
    #'@return Returns a \code{matrix} containing the special tokens in the rows
    #'and their type, token, and id in the columns.
    get_special_tokens=function(){
      special_tokens=c("bos_token",
                       "eos_token",
                       "unk_token",
                       "sep_token",
                       "pad_token",
                       "cls_token",
                       "mask_token")
      tokens_map=matrix(nrow = length(special_tokens),
                        ncol=3,
                        data = NA)
      colnames(tokens_map)=c("type","token","id")

      for(i in 1:length(special_tokens)){
        tokens_map[i,1]=special_tokens[i]
        tokens_map[i,2]=private$transformer_components$tokenizer[special_tokens[i]]
        tokens_map[i,3]=private$transformer_components$tokenizer[paste0(special_tokens[i],"_id")]
      }

      #if(private$basic_components$method=="bert"|
      #   private$basic_components$method=="funnel"|
      #   private$basic_components$method=="deberta_v2"){
      #  for(i in 1:nrow(tokens_map)){
      #    tokens_map[i,1]=names(special_tokens)[[i]]
      #    tokens_map[i,2]=special_tokens[[i]]
      #    tokens_map[i,3]=private$transformer_components$tokenizer$convert_tokens_to_ids(special_tokens[[i]])
      #  }
      #} else {
      #  for(i in 1:nrow(tokens_map)){
      #    tokens_map[i,1]=names(special_tokens)[[i]]
      #    tokens_map[i,2]=special_tokens[[i]]$content
      #    tokens_map[i,3]=private$transformer_components$tokenizer$convert_tokens_to_ids(special_tokens[[i]]$content)
      #  }
      #}


      return(tokens_map)
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
    #'@description In the case of using a GPU and running out of memory reduce the
    #'batch size or restart R and switch to use cpu only via \link{set_config_cpu_only}.
    embed=function(raw_text=NULL,doc_id=NULL,batch_size=8, trace = FALSE){

      #transformer---------------------------------------------------------------------
      if((private$basic_components$method %in% private$supported_transformers)==TRUE){

        n_units<-length(raw_text)
        n_layer<-private$transformer_components$model$config$num_hidden_layers
        n_layer_size<-private$transformer_components$model$config$hidden_size

        #Batch refers to the number of cases
        n_batches=ceiling(n_units/batch_size)
        batch_results<-NULL

        update_aifeducation_progress_bar(value = 0,total = n_batches,title = "Embedding")

        if(private$transformer_components$emb_pool_type=="average"){
          if(private$transformer_components$ml_framework=="pytorch"){
            reticulate::py_run_file(system.file("python/pytorch_te_classifier.py",
                                                package = "aifeducation"))
            pooling=py$GlobalAveragePooling1D_PT()
          } else if(private$transformer_components$ml_framework=="tensorflow"){
            pooling=keras$layers$GlobalAveragePooling1D()
          }
        }

        for (b in 1:n_batches){

          if(private$transformer_components$ml_framework=="pytorch"){
            #Set model to evaluation mode
            private$transformer_components$model$eval()
            if(torch$cuda$is_available()){
              pytorch_device="cuda"
            } else {
              pytorch_device="cpu"
            }
            private$transformer_components$model$to(pytorch_device)
          }

          #tokens<-self$encode(raw_text = raw_text,
          #                    trace = trace,
          #                    token_encodings_only=FALSE)

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
                    private$transformer_components$chunks,
                    n_layer_size))

          #Selecting the relevant layers
          selected_layer=private$transformer_components$emb_layer_min:private$transformer_components$emb_layer_max
          tmp_selected_layer=1+selected_layer

          if(private$transformer_components$ml_framework=="tensorflow"){
            #Clear session to ensure enough memory
            tf$keras$backend$clear_session()

            #Calculate tensors
            tokens$encodings$set_format(type="tensorflow")

            tensor_embeddings<-private$transformer_components$model(
              input_ids=tokens$encodings["input_ids"],
              attention_mask=tokens$encodings["attention_mask"],
              token_type_ids=tokens$encodings["token_type_ids"],
              output_hidden_states=TRUE)$hidden_states
            if(private$transformer_components$emb_pool_type=="average"){
              #Average Pooling over all tokens
              for(i in tmp_selected_layer){
                tensor_embeddings[i]=list(pooling(
                  inputs=tensor_embeddings[[as.integer(i)]],
                  mask=tokens$encodings["attention_mask"]))
              }
            }


          } else {
            #Clear memory
            if(torch$cuda$is_available()){
              torch$cuda$empty_cache()
            }

            #Calculate tensors
            tokens$encodings$set_format(type="torch")

            with(data=torch$no_grad(),
            tensor_embeddings<-private$transformer_components$model(
              input_ids=tokens$encodings["input_ids"]$to(pytorch_device),
              attention_mask=tokens$encodings["attention_mask"]$to(pytorch_device),
              token_type_ids=tokens$encodings["token_type_ids"]$to(pytorch_device),
              output_hidden_states=TRUE)$hidden_states
            )
            #print(tensor_embeddings)
            if(private$transformer_components$emb_pool_type=="average"){
              #Average Pooling over all tokens
              for(i in tmp_selected_layer){
                tensor_embeddings[i]=list(pooling(x=tensor_embeddings[[as.integer(i)]],
                                                  mask=tokens$encodings["attention_mask"]))
              }
            }

            #print(tensor_embeddings)
            #print(tensor_embeddings[[as.integer(2)]])
            #print(tokens$encodings["attention_mask"])
          }




          #if(private$transformer_components$aggregation=="last"){
          #  selected_layer=private$transformer_components$model$config$num_hidden_layers
          #} else if (private$transformer_components$aggregation=="second_to_last") {
          #  selected_layer=private$transformer_components$model$config$num_hidden_layers-2
          #} else if (private$transformer_components$aggregation=="fourth_to_last") {
          #  selected_layer=private$transformer_components$model$config$num_hidden_layers-4
          #} else if (private$transformer_components$aggregation=="all") {
          #  selected_layer=2:private$transformer_components$model$config$num_hidden_layers
          #} else if (private$transformer_components$aggregation=="last_four") {
          #  selected_layer=(private$transformer_components$model$config$num_hidden_layers-4):private$transformer_components$model$config$num_hidden_layers
          #}

          #Sorting the hidden states to the corresponding cases and times
          #If more than one layer is selected the mean is calculated
          index=0
          for(i in 1:length(batch)){
            for(j in 1:tokens$chunks[i]){
              for(layer in tmp_selected_layer){
                if(private$transformer_components$ml_framework=="tensorflow"){
                  if(private$transformer_components$emb_pool_type=="cls"){
                    #CLS Token is always the first token
                    text_embedding[i,j,]<-text_embedding[i,j,]+as.vector(
                      tensor_embeddings[[as.integer(layer)]][[as.integer(index)]][[as.integer(0)]]$numpy()
                    )
                  } else if(private$transformer_components$emb_pool_type=="average"){
                    text_embedding[i,j,]<-text_embedding[i,j,]+as.vector(
                      tensor_embeddings[[as.integer(layer)]][[as.integer(index)]]$numpy()
                    )
                  }

                } else {
                  if(torch$cuda$is_available()==FALSE){
                    if(private$transformer_components$emb_pool_type=="cls"){
                      #CLS Token is always the first token
                      text_embedding[i,j,]<-text_embedding[i,j,]+as.vector(
                        tensor_embeddings[[as.integer(layer)]][[as.integer(index)]][[as.integer(0)]]$detach()$numpy()
                      )
                    } else if(private$transformer_components$emb_pool_type=="average"){
                      text_embedding[i,j,]<-text_embedding[i,j,]+as.vector(
                        tensor_embeddings[[as.integer(layer)]][[as.integer(index)]]$detach()$numpy()
                      )
                    }
                  } else {
                    if(private$transformer_components$emb_pool_type=="cls"){
                      #CLS Token is always the first token
                      text_embedding[i,j,]<-text_embedding[i,j,]+as.vector(
                        tensor_embeddings[[as.integer(layer)]][[as.integer(index)]][[as.integer(0)]]$detach()$cpu()$numpy()
                      )
                    } else if(private$transformer_components$emb_pool_type=="average"){
                      text_embedding[i,j,]<-text_embedding[i,j,]+as.vector(
                        tensor_embeddings[[as.integer(layer)]][[as.integer(index)]]$detach()$cpu()$numpy()
                      )
                    }
                  }

                }

              }
              text_embedding[i,j,]<-text_embedding[i,j,]/length(tmp_selected_layer)
              index=index+1
            }
          }
          dimnames(text_embedding)[[3]]<-paste0(private$basic_components$method,"_",seq(from=1,to=n_layer_size,by=1))

            #Add ID of every case
            dimnames(text_embedding)[[1]]<-doc_id[batch]
            batch_results[b]=list(text_embedding)
            if(trace==TRUE){
            cat(paste(date(),
                        "Batch",b,"/",n_batches,"Done","\n"))
            }
            base::gc(verbose = FALSE,full = TRUE)

            update_aifeducation_progress_bar(value = b,total = n_batches,title = "Embedding")
          }

      #Summarizing the results over all batches
      text_embedding=abind::abind(batch_results,along = 1)
        #Glove Cluster----------------------------------------------------------
      } else if(private$basic_components$method=="glove_cluster"){
        tokens<-self$encode(raw_text = raw_text,
                            trace = trace,
                            token_encodings_only=FALSE)


        text_embedding<-array(
          data = 0,
          dim = c(length(tokens),
                  1,
                  private$bow_components$configuration$bow_n_cluster))
        #text_embedding<-matrix(nrow = length(tokens),
        #                       ncol =  private$bow_components$configuration$bow_n_cluster,
        #                       data = 0)
        for(i in 1:length(tokens)){
          token_freq<-table(tokens[[i]])
          tmp_tokens<-names(token_freq)
          for(j in 1:length(token_freq)){
            index<-match(x=as.integer(tmp_tokens[j]),
                  table = private$bow_components$model$index)
            text_embedding[i,1,private$bow_components$model$cluster[index]]<-token_freq[j]+
              text_embedding[i,1,private$bow_components$model$cluster[index]]
          }
        }

        #text_embedding=text_embedding/rowSums(text_embedding)
        #text_embedding[is.nan(text_embedding)]<-0

        dimnames(text_embedding)[[3]]<-paste0(private$basic_components$method,"_",seq(from=1,to=private$bow_components$configuration$bow_n_cluster,by=1))
        #Add ID of every case
        dimnames(text_embedding)[[1]]<-doc_id

        #text_embedding<-as.data.frame(text_embedding)
        #Topic Modeling---------------------------------------------------------
      } else if(private$basic_components$method=="lda"){
        tokens<-self$encode(raw_text = raw_text,
                            trace = trace,
                            token_encodings_only=FALSE)

        text_embedding<-array(
          data = 0,
          dim = c(length(tokens),
                  1,
                  private$bow_components$configuration$bow_n_dim))
        #text_embedding<-matrix(nrow = length(tokens),
        #                       ncol =  private$bow_components$configuration$bow_n_dim,
        #                       data = 0)
        for(i in 1:length(tokens)){
          token_freq<-table(tokens[[i]])
          tmp_tokens<-names(token_freq)
          if(length(tmp_tokens)>0){
            for(j in 1:length(token_freq)){
              index<-match(x=as.integer(tmp_tokens[j]),
                           table = private$bow_components$model$index)
              if(is.na(index)==FALSE){
                text_embedding[i,1,]<-text_embedding[i,1,]+token_freq[j]*as.matrix(private$bow_components$model[index,-1])
              }
            }
          }
        }
        #text_embedding<-text_embedding/rowSums(private$bow_components$model[,-1])
        text_embedding<-text_embedding/rowSums(text_embedding)
        #Replace NaN with 0 which indicate that the rowsum is 0 and division ist not
        #possible
        text_embedding[is.nan(text_embedding)]<-0

        dimnames(text_embedding)[[3]]<-paste0(private$basic_components$method,"_",seq(from=1,to=private$bow_components$configuration$bow_n_dim,by=1))
        #Add ID of every case
        dimnames(text_embedding)[[1]]<-doc_id

        #text_embedding<-as.data.frame(text_embedding)
      }
      #------------------------------------------------------------------------

      if((private$basic_components$method %in%private$supported_transformers)==TRUE){
        embeddings<-EmbeddedText$new(
          model_name = private$model_info$model_name,
          model_label = private$model_info$model_label,
          model_date = private$model_info$model_date,
          model_method = private$basic_components$method,
          model_version = private$model_info$model_version,
          model_language = private$model_info$model_language,
          param_seq_length =private$basic_components$max_length,
          param_chunks = private$transformer_components$chunks,
          param_overlap = private$transformer_components$overlap,

          param_emb_layer_min=private$transformer_components$emb_layer_min,
          param_emb_layer_max=private$transformer_components$emb_layer_max,
          param_emb_pool_type=private$transformer_components$emb_pool_type,

          param_aggregation = NA,
          embeddings = text_embedding
        )
      } else if(private$basic_components$method=="glove_cluster" |
                private$basic_components$method=="lda"){
        embeddings<-EmbeddedText$new(
          model_name = private$model_info$model_name,
          model_date = private$model_info$model_date,
          model_label = private$model_info$model_label,
          model_method = private$basic_components$method,
          model_version = private$model_info$model_version,
          model_language = private$model_info$model_language,
          param_seq_length =private$basic_components$max_length,
          param_chunks = private$bow_components$chunks,
          param_overlap = private$bow_components$overlap,
          param_aggregation = private$bow_components$aggregation,
          embeddings = text_embedding
        )
      }
      return(embeddings)
    },
    #Fill Mask------------------------------------------------------------------
    #'@description Method for calculating tokens behind mask tokens.
    #'@param text \code{string} Text containing mask tokens.
    #'@param n_solutions \code{int} Number estimated tokens for every mask.
    #'@return Returns a \code{list} containing a \code{data.frame} for every
    #'mask. The \code{data.frame} contains the solutions in the rows and reports
    #'the score, token id, and token string in the columns.
    fill_mask=function(text,n_solutions=5){
      if(private$transformer_components$ml_framework=="pytorch"){
        framework="pt"
        private$transformer_components$model_mlm$to("cpu")
      } else {
        framework="tf"
      }

      fill_mask_pipline=transformers$FillMaskPipeline(
        model=private$transformer_components$model_mlm,
        tokenizer = private$transformer_components$tokenizer,
        framework = framework,
        num_workers=1,
        binary_output =FALSE,
        top_k=as.integer(n_solutions))

      special_tokens=self$get_special_tokens()
      mask_token=special_tokens[special_tokens[,"type"]=="mask_token","token"]

      n_mask_tokens=ncol(stringr::str_extract_all(text,
                                                    stringr::fixed(mask_token),
                                                    simplify = TRUE))

      if(n_mask_tokens==0){
        stop("There is no masking token. Please check your input.")
      }

      solutions=as.list(fill_mask_pipline(text))

      solutions_list=NULL

      if(n_mask_tokens==1){
        solution_data_frame=matrix(nrow = length(solutions),
                                   ncol = 3)
        colnames(solution_data_frame)=c(
          "score",
          "token",
          "token_str")
        for(i in 1:length(solutions)){
          solution_data_frame[i,"score"]=solutions[[i]]$score
          solution_data_frame[i,"token"]=solutions[[i]]$token
          solution_data_frame[i,"token_str"]=solutions[[i]]$token_str
        }
        solution_data_frame=as.data.frame(solution_data_frame)
        solution_data_frame$score=as.numeric(solution_data_frame$score)
        solutions_list[length(solutions_list)+1]=list(solution_data_frame)
      } else {
        for (j in 1:length(solutions)){
          solution_data_frame=matrix(nrow = length(solutions[[j]]),
                                     ncol = 3)
          colnames(solution_data_frame)=c(
            "score",
            "token",
            "token_str")
          for(i in 1:length(solutions[[j]])){
            solution_data_frame[i,"score"]=solutions[[j]][[i]]$score
            solution_data_frame[i,"token"]=solutions[[j]][[i]]$token
            solution_data_frame[i,"token_str"]=solutions[[j]][[i]]$token_str
          }
          solution_data_frame=as.data.frame(solution_data_frame)
          solution_data_frame$score=as.numeric(solution_data_frame$score)
          solutions_list[length(solutions_list)+1]=list(solution_data_frame)
        }
      }

      return(solutions_list)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting the bibliographic information of the model.
    #'@param type \code{string} Type of information which should be changed/added.
    #'\code{type="developer"}, and \code{type="modifier"} are possible.
    #'@param authors List of people.
    #'@param citation \code{string} Citation in free text.
    #'@param url \code{string} Corresponding URL if applicable.
    #'@return Function does not return a value. It is used to set the private
    #'members for publication information of the model.
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
    #'@return Function does not return a value. It is used for setting the private
    #'member for the software license of the model.
    set_software_license=function(license="GPL-3"){
      private$model_info$model_license<-license
    },
    #'@description Method for requesting the license of the model
    #'@return \code{string} License of the model
    get_software_license=function(){
      return(private$model_info$model_license)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting the license of models' documentation.
    #'@param license \code{string} containing the abbreviation of the license or
    #'the license text.
    #'@return Function does not return a value. It is used to set the private member for the
    #'documentation license of the model.
    set_documentation_license=function(license="CC BY-SA"){
      private$model_description$license<-license
    },
    #'@description Method for getting the license of the models' documentation.
    #'@param license \code{string} containing the abbreviation of the license or
    #'the license text.
    get_documentation_license=function(){
      return(private$model_description$license)
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
    #'@return Function does not return a value. It is used to set the private members for the
    #'description of the model.
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
        model_name_root=private$model_info$model_name_root,
        model_name=private$model_info$model_name,
        model_label=private$model_info$model_label,
        model_date=private$model_info$model_date,
        model_version=private$model_info$model_version,
        model_language=private$model_info$model_language,
        model_method=private$basic_components$method,
        model_max_size=private$basic_components$max_length
        )
        )
    },
    #---------------------------------------------------------------------------
    #'@description Method for requesting a summary of the R and python packages'
    #'versions used for creating the classifier.
    #'@return Returns a \code{list} containing the versions of the relevant
    #'R and python packages.
    get_package_versions=function(){
      return(
        private$r_package_versions
      )
    },
    #'@description Method for requesting the part of interface's configuration that is
    #'necessary for all models.
    #'@return Returns a \code{list}.
    get_basic_components=function(){
      return(
        private$basic_components
      )
    },
    #'@description Method for requesting the part of interface's configuration that is
    #'necessary bag-of-words models.
    #'@return Returns a \code{list}.
    get_bow_components=function(){
      return(
        private$bow_components
      )
    },
    #'@description Method for requesting the part of interface's configuration that is
    #'necessary for transformer models.
    #'@return Returns a \code{list}.
    get_transformer_components=function(){
      return(
        list(
          chunks=private$transformer_components$chunks,
          overlap=private$transformer_components$overlap,
          ml_framework=private$transformer_components$ml_framework,

          emb_layer_min=private$transformer_components$emb_layer_min,
          emb_layer_max=private$transformer_components$emb_layer_max,
          emb_pool_type=private$transformer_components$emb_pool_type

        )
      )
    },
    #'@description Method for requesting a log of tracked energy consumption
    #'during training and an estimate of the resulting CO2 equivalents in kg.
    #'@return Returns a \code{matrix} containing the tracked energy consumption,
    #'CO2 equivalents in kg, information on the tracker used, and technical
    #'information on the training infrastructure for every training run.
    get_sustainability_data=function(){
      return(private$sustainability$track_log)
    },
    #---------------------------------------------------------------------------
    #'@description Method for requesting the machine learning framework used
    #'for the classifier.
    #'@return Returns a \code{string} describing the machine learning framework used
    #'for the classifier
    get_ml_framework=function(){
      return(private$transformer_components$ml_framework)
    }
  )
)


#'@title Embedded text
#'@description Object of class \link[R6]{R6} which stores the text embeddings
#'generated by an object of class \link{TextEmbeddingModel} via the method
#'\code{embed()}.
#'@return Returns an object of class \code{EmbeddedText}. These objects are used
#'for storing and managing the text embeddings created with objects of class \link{TextEmbeddingModel}.
#'Objects of class \code{EmbeddedText} serve as input for classifiers of class
#'\link{TextEmbeddingClassifierNeuralNet}. The main aim of this class is to provide a structured link between
#'embedding models and classifiers. Since objects of this class save information on
#'the text embedding model that created the text embedding it ensures that only
#'embedding generated with same embedding model are combined. Furthermore, the stored information allows
#'classifiers to check if embeddings of the correct text embedding model are used for
#'training and predicting.
#'@family Text Embedding
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

    #Minimal layer to be included in the creation of embeddings.
    param_emb_layer_min=NA,

    #Maximal layer to be included in the creation of embeddings.
    param_emb_layer_max=NA,

    #Type of pooling tokens embeddings within each layer.
    param_emb_pool_type=NA,


    #Aggregation method of the hidden states. Deprecated. Included for backward
    #compatibility.
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
    #'
    #'@param param_emb_layer_min \code{int} or \code{string} determining the first layer to be included
    #'in the creation of embeddings.
    #'@param param_emb_layer_max \code{int} or \code{string} determining the last layer to be included
    #'in the creation of embeddings.
    #'@param param_emb_pool_type \code{string} determining the method for pooling the token embeddings
    #'within each layer.
    #'
    #'@param param_aggregation \code{string} Aggregation method of the hidden states. Deprecated. Only included
    #'for backward compatibility.
    #'@param embeddings \code{data.frame} containing the text embeddings.
    #'@return Returns an object of class \link{EmbeddedText} which stores the
    #'text embeddings produced by an objects of class \link{TextEmbeddingModel}.
    #'The object serves as input for objects of class \link{TextEmbeddingClassifierNeuralNet}.
    initialize=function(model_name=NA,
                        model_label=NA,
                        model_date=NA,
                        model_method=NA,
                        model_version=NA,
                        model_language=NA,
                        param_seq_length=NA,
                        param_chunks=NULL,
                        param_overlap=NULL,
                        param_emb_layer_min=NULL,
                        param_emb_layer_max=NULL,
                        param_emb_pool_type=NULL,

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


      private$param_emb_layer_min=param_emb_layer_min
      private$param_emb_layer_max=param_emb_layer_max
      private$param_emb_pool_type=param_emb_pool_type

      private$param_aggregation = param_aggregation
      self$embeddings=embeddings
    },
    #--------------------------------------------------------------------------
    #'@description Method for retrieving information about the model that
    #'generated this embedding.
    #'@return \code{list} contains all saved information about the underlying
    #'text embedding model.
    get_model_info=function(){
      tmp<-list(model_name=private$model_name,
                model_label=private$model_label,
                model_date=private$model_date,
                model_method=private$model_method,
                model_version=private$model_version,
                model_language=private$model_language,
                param_seq_length=private$param_seq_length,
                param_chunks=private$param_chunks,
                param_overlap=private$param_overlap,
                param_emb_layer_min=private$param_emb_layer_min,
                param_emb_layer_max=private$param_emb_layer_max,
                param_emb_pool_type=private$param_emb_pool_type,
                param_aggregation=private$param_aggregation)
      return(tmp)
    },
    #--------------------------------------------------------------------------
    #'@description Method for retrieving the label of the model that
    #'generated this embedding.
    #'@return \code{string} Label of the corresponding text embedding model
    get_model_label=function(){
      return(private$transformer_components$ml_framework)
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
#'@family Text Embedding
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
    model_label = embeddings_list[[1]]$get_model_info()$model_label,
    model_date =embeddings_list[[1]]$get_model_info()$model_date,
    model_method=embeddings_list[[1]]$get_model_info()$model_method,
    model_version=embeddings_list[[1]]$get_model_info()$model_version,
    model_language=embeddings_list[[1]]$get_model_info()$model_language,
    param_seq_length=embeddings_list[[1]]$get_model_info()$param_seq_length,
    param_chunks=embeddings_list[[1]]$get_model_info()$param_chunks,
    param_overlap=embeddings_list[[1]]$get_model_info()$param_overlap,

    param_emb_layer_min=embeddings_list[[1]]$get_model_info()$param_emb_layer_min,
    param_emb_layer_max=embeddings_list[[1]]$get_model_info()$param_emb_layer_max,
    param_emb_pool_type=embeddings_list[[1]]$get_model_info()$param_emb_pool_type,

    param_aggregation=embeddings_list[[1]]$get_model_info()$param_aggregation

  )

  return(new_embedding)
}
