LargeDataSetForTextEmbeddings<-R6::R6Class(
  classname = "LargeDataSetForTextEmbeddings",
  inherit = LargeDataSetBase,
  private = list(
    #'dataset based on pyarrow containing the text embeddings for all chunks.
    embeddings=NA,

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

    #Features of the embeddings
    param_features=NA,

    #Minimal layer to be included in the creation of embeddings.
    param_emb_layer_min=NA,

    #Maximal layer to be included in the creation of embeddings.
    param_emb_layer_max=NA,

    #Type of pooling tokens embeddings within each layer.
    param_emb_pool_type=NA,


    #Aggregation method of the hidden states. Deprecated. Included for backward
    #compatibility.
    param_aggregation=NA,

    #List containing information on the feature extractor if the embeddings
    #are compressed.
    feature_extractor=list()
  ),
  public = list(
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
                        param_features=NULL,
                        param_overlap=NULL,
                        param_emb_layer_min=NULL,
                        param_emb_layer_max=NULL,
                        param_emb_pool_type=NULL,
                        param_aggregation=NULL){
      private$model_name = model_name
      private$model_label = model_label
      private$model_date = model_date
      private$model_method = model_method
      private$model_version = model_version
      private$model_language = model_language
      private$param_seq_length = param_seq_length
      private$param_overlap = param_overlap

      private$param_features=param_features
      private$param_chunks = param_chunks

      private$param_emb_layer_min=param_emb_layer_min
      private$param_emb_layer_max=param_emb_layer_max
      private$param_emb_pool_type=param_emb_pool_type

      private$param_aggregation = param_aggregation
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
                param_features=private$param_features,
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
    },
    #--------------------------------------------------------------------------
    add_feature_extractor_info=function(model_name,
                                        model_label=NA,
                                        features=NA,
                                        method=NA,
                                        noise_factor=NA,
                                        optimizer=NA){
      private$feature_extractor=list(
        model_name=model_name,
        model_label=model_label,
        features=features,
        method=method,
        noise_factor=noise_factor,
        optimizer=optimizer
      )
    },
    #--------------------------------------------------------------------------
    get_feature_extractor_info=function(){
      if(is.null_or_na(private$feature_extractor$model_name)){
        return(NULL)
      } else {
        return(private$feature_extractor)
      }
    },
    #--------------------------------------------------------------------------
    is_compressed=function(){
      if(is.null_or_na(private$feature_extractor$model_name)){
        return(FALSE)
      } else {
        return(TRUE)
      }
    },
    #--------------------------------------------------------------------------
    get_times=function(){
      return(private$param_chunks)
    },
    #--------------------------------------------------------------------------
    get_features=function(){
      if(self$is_compressed()==TRUE){
        return(private$feature_extractor$features)
      } else {
        return(private$param_features)
      }
    },
    #-------------------------------------------------------------------------
    get_original_features=function(){
      return(private$param_features)
    },
    #--------------------------------------------------------------------------
    add_embeddings_from_array=function(embedding_array){
      if(is.array(embedding_array)==FALSE){
        stop("Input must be an array.")
      }
      if(self$get_features()!=dim(embedding_array)[3]){
        stop("The number of features does not fit to the underlying
             text embedding model. Please check if you either used compressed
             embedding for a dataset of uncompressed embeddings or uncrompressed
             embeddings for a dataset of compressed embeddings.")
      }
      if(self$get_times()!=dim(embedding_array)[2]){
        stop("Number of times/chunks does not fit to the underlying text embedding model.")
      }

      #Check the number of rows and duplicate if necessary
      if(dim(embedding_array)[1]==1){
        embedding_array=abind::abind(embedding_array,embedding_array,
                                     along = 1)
      }
      #Transform to a python dict
      new_dataset_dict=reticulate::dict(
        id=rownames(embedding_array),
        input= prepare_r_array_for_dataset(embedding_array),
        length=get_n_chunks(text_embeddings=embedding_array,
                            features=self$get_features(),
                            times=self$get_times()))
      #Create new dataset
      new_dataset=datasets$Dataset$from_dict(new_dataset_dict)
      #Check the number of rows and remove duplicate if necessary
      if(dim(embedding_array)[1]==1){
        new_dataset=new_dataset$select(indices=list(as.integer(0)))
      }
      #add dataset
      private$add(new_dataset)
    },
    #-------------------------------------------------------------------------
    add_embeddings_from_EmbeddedText=function(EmbeddedText){
      if("EmbeddedText"%in%class(EmbeddedText)==FALSE){
        stop("Input must be an object of class EmbeddedText.")
      }

      #Select array
      embedding_array=EmbeddedText$embeddings

      if(self$get_features()!=dim(embedding_array)[3]){
        stop("The number of features does not fit to the underlying
             text embedding model. Please check if you either used compressed
             embedding for a dataset of uncompressed embeddings or uncrompressed
             embeddings for a dataset of compressed embeddings.")
      }
      if(self$get_times()!=dim(embedding_array)[2]){
        stop("Number of times/chunks does not fit to the underlying text embedding model.")
      }

      #Check the number of rows and duplicate if necessary
      if(dim(embedding_array)[1]==1){
        embedding_array=abind::abind(embedding_array,embedding_array,
                                     along = 1)
      }
      #Transform to a python dict
      new_dataset_dict=reticulate::dict(
        id=rownames(embedding_array),
        input= prepare_r_array_for_dataset(embedding_array),
        length=get_n_chunks(text_embeddings=embedding_array,
                            features=self$get_features(),
                            times=self$get_times()))
      #Create new dataset
      new_dataset=datasets$Dataset$from_dict(new_dataset_dict)
      #Check the number of rows and remove duplicate if necessary
      if(dim(embedding_array)[1]==1){
        new_dataset=new_dataset$select(indices=list(as.integer(0)))
      }
      #add dataset
      private$add(new_dataset)
    },
    #-------------------------------------------------------------------------
    convert_to_EmbeddedText=function(){
      new_data_set=EmbeddedText$new(
        model_name=private$model_name,
        model_label=private$model_label,
        model_date=private$model_date,
        model_method=private$model_method,
        model_version=private$model_version,
        model_language=private$model_language,
        param_seq_length=private$param_seq_length,
        param_chunks=private$param_chunks,
        param_features=private$param_features,
        param_overlap=private$param_overlap,
        param_emb_layer_min=private$param_emb_layer_min,
        param_emb_layer_max=private$param_emb_layer_max,
        param_emb_pool_type=private$param_emb_pool_type,
        param_aggregation=private$param_aggregation,
        embeddings =py_dataset_to_embeddings(self$get_dataset())
      )

      if(self$is_compressed()==TRUE){
        new_data_set$add_feature_extractor_info(
          model_name=private$feature_extractor$model_name,
          model_label=private$feature_extractor$model_label,
          features=private$feature_extractor$features,
          method=private$feature_extractor$method,
          noise_factor=private$feature_extractor$noise_factor,
          optimizer=private$feature_extractor$optimizer
        )
      }
      return(new_data_set)
    }
  )
)
