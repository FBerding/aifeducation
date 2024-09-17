#'Combine embedded texts
#'
#'Function for combining embedded texts of the same model
#'
#'@param embeddings_list `list` of objects of class \link{EmbeddedText}.
#'@return Returns an object of class \link{EmbeddedText} which contains all
#'unique cases of the input objects.
#'@family combine_embeddings
#'@export
#'@importFrom methods isClass
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
      combined_embeddings<-array_form_bind(combined_embeddings,embeddings_list[[i]]$embeddings)
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
