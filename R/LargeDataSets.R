
#'@export
LargeDataSetBase<-R6::R6Class(
  classname = "LargeDataSetBase",
  public = list(
    n_cols=function(){
      retrun(private$data$num_columns)
    },
    #------------------------------------------------------
    n_rows=function(){
      return(private$data$num_rows)
    },
    #-----------------------------------------------------
    get_colnames=function(){
      return(private$data$column_names)
    },
    #-----------------------------------------------------
    get_dataset=function(){
      return(private$data)
    },
    #------------------------------------------------------
    reduce_to_unique_ids=function(){

    },
    #----------------------------------------------------
    select=function(indicies){
      return(private$data$select(indicies))
    }
  ),
  private = list(
    data=NULL,
    #--------------------------------------------------------------------------
    add=function(new_dataset){
      if(is.null(private$data)){
        private$data=new_dataset
      } else {
        private$data=datasets$concatenate_datasets(
          list(private$data,new_dataset))
      }
    }
  )
)


#'@export
LargeDataSetForText<-R6::R6Class(
  classname = "LargeDataSetForText",
  inherit = LargeDataSetBase,
  public = list(
    add_from_files_txt=function(dir_path,batch_size=500,trace=TRUE){
      #Gather all text files
      file_paths=private$get_file_paths(".txt")

      #calculate number of batches
      n_batches=ceiling(length(file_paths)/batch_size)

      #get indices for every batch
      batches=get_batches_index(number_rows=length(file_paths),
                                batch_size = batch_size)

      #Process every batch
      list_datasets=list()
      for(i in 1:n_batches){
        chunk=private$get_batch(batches[[i]],
                        file_paths=file_paths,
                        clean_text=TRUE)
        chunk_dataset=data.frame_to_py_dataset(chunk)
        list_datasets[i]=list(chunk_dataset)
        if(trace==TRUE){
          message(paste(date(),
                        "Batch",i,"from",n_batches,"processed"))
        }
      }

      #concatenate datasets
      new_dataset=datasets$concatenate_datasets(dsets = list_datasets,axis = 0L)

      #Add new dataset
      private$add(new_dataset)
     },
    add_from_files_pdf=function(dir_path,batch_size=500,trace=TRUE){
      #Gather all text files
      file_paths=private$get_file_paths(".pdf")

      #calculate number of batches
      n_batches=ceiling(length(file_paths)/batch_size)

      #get indices for every batch
      batches=get_batches_index(number_rows=length(file_paths),
                                        batch_size = batch_size)

      #Process every batch
      list_datasets=list()
      for(i in 1:n_batches){
        chunk=private$get_batch(batches[[i]],
                                file_paths=file_paths,
                                clean_text=TRUE)
        chunk_dataset=data.frame_to_py_dataset(chunk)
        list_datasets[i]=list(chunk_dataset)
        if(trace==TRUE){
          message(paste(date(),
                        "Batch",i,"from",n_batches,"processed"))
        }
      }

      #concatenate datasets
      new_dataset=datasets$concatenate_datasets(dsets = list_datasets,axis = 0L)

      #Add new dataset
      private$add(new_dataset)
    },
    add_from_filex_xlsx=function(dir_path,trace=TRUE,
                                 id_column="id",
                                 text_column="text",
                                 bib_entry_column="bib_entry",
                                 license_column="license"){
      #Gather all text files
      file_paths=private$get_file_paths(".xlsx")
      n_batches=length(file_paths)

      #Process every batch
      list_datasets=list()
      for(i in 1:n_batches){
        chunk=readtext::readtext(
          file=file_paths[i],
          docid_field = id_column,
          text_field = text_column)

        #Set correct name of id column
        index=which(colnames(chunk)%in%"doc_id")
        colnames(chunk)[index]="id"
        print(chunk)

        #Bib_entry column
        index=which(colnames(chunk)%in%bib_entry_column)
        if (length(index)==0)
        {
          bib_entry=vector(length = nrow(chunk))
          bib_entry[]=NA
          chunk$bib_entry=bib_entry
        } else {
          colnames(chunk)[index]="bib_entry"
        }
        print(chunk)

        #License column
        index=which(colnames(chunk)%in%license_column)
        if (length(index)==0)
        {
          license=vector(length = nrow(chunk))
          license[]=NA
          chunk$license=license
        } else {
          colnames(chunk)[index]="license"
        }
        print(chunk)

        chunk_dataset=data.frame_to_py_dataset(chunk)
        list_datasets[i]=list(chunk_dataset)
        if(trace==TRUE){
          message(paste(date(),
                        "Batch",i,"from",n_batches,"processed"))
        }

        #concatenate datasets
        new_dataset=datasets$concatenate_datasets(dsets = list_datasets,axis = 0L)

        #Add new dataset
        private$add(new_dataset)
      }

    },
    add_from_data.frame=function(data_frame){
      if(is.data.frame(data_frame)==FALSE){
        stop("Input must be of type data.frame")
      }
      if("id"%in%colnames(data_frame)==FALSE){
        stop("data.frame must contain a column id.")
      }
      if("text"%in%colnames(data_frame)==FALSE){
        stop("data.frame must contain a column text.")
      }
      if("bib_entry"%in%colnames(data_frame)==FALSE){
        stop("data.frame must contain a column bib_entry.")
      }
      if("license"%in%colnames(data_frame)==FALSE){
        stop("data.frame must contain a column license.")
      }

      #Transform to a python dataset
      new_dataset=data.frame_to_py_dataset(data_frame[c("id","text","bib_entry","license")])

      #Add new dataset
      private$add(new_dataset)
    }
  ),
        private=list(
          get_file_paths=function(file_type){
            file_paths=list.files(
              path = dir_path,
              include.dirs = FALSE,
              all.files = TRUE,
              full.names = TRUE,
              recursive = TRUE,
              pattern = paste0("*",file_type))
            file_paths=private$clean_path(file_paths)
            return(file_paths)
            },

          clean_path=function(paths){
            new_paths=vector(length = length(paths))
            new_paths[]=NA
            for(i in 1:length(paths)){
              path=paths[i]
              bib_entry_path=paste0(
                dirname(path),"/bib_entry.txt"
              )
              licence_path=paste0(
                dirname(path),"/license.txt"
              )
              if(path!=bib_entry_path & path!=licence_path){
                new_paths[i]=path
              }
            }
            return(na.omit(new_paths))
          },
          get_batch=function(batch,file_paths,clean_text=TRUE){
            data=matrix(data = NA,
                        nrow = length(batch),
                        ncol = 4)
            colnames(data)=c("id","text","bib_entry","license")

            for(i in batch){
              document=readtext::readtext(file=file_paths[i])

              #ID
              data[i,1]=private$remove_file_extenstion(document$doc_id)

              #Text
              if(clean_text==TRUE){
                text=private$clean_text(document$text)
              } else {
                text=document$text
              }
              data[i,2]=text

              #Bib_entry
              file_path=paste0(dir(file_paths[i]),"/bib_entry.txt")
              if(file.exists(file_path)==TRUE){
                data[i,3]=read.csv(file = (file_path))
              } else {
                data[i,3]=NA
              }

              #License
              file_path=paste0(dir(file_paths[i]),"/license.txt")
              if(file.exists(file_path)==TRUE){
                data[i,4]=read.csv(file = (file_path))
              } else {
                data[i,4]=NA
              }
            }
            return(as.data.frame(data))
          },
          clean_text=function(text){
            text=stringr::str_replace_all(text,pattern = "[:space:]{1,}",replacement = " ")
            text=stringr::str_replace_all(text,pattern = "-(?=[:space:])",replacement = "")
            return(text)
          },
          remove_file_extenstion=function(file){
            tmp_string=stringr::str_split_fixed(file,pattern="\\.",n=Inf)
            return(paste0(tmp_string[1,1:(ncol(tmp_string)-1)],collapse = "."))
          }
        )
)


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
    }
  )
)
