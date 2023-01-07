#' Function for predicting data with a trained AI
#'
#' @export
ai_predict<-function(text_embeddings,
                     additional_data=NULL,
                     trained_learner,
                     na.rm=TRUE,
                     verbose=FALSE){

  if(text_embeddings$get_model_info()$model_name!=trained_learner$text_model$model_name |
     text_embeddings$get_model_info()$model_date!=trained_learner$text_model$model_date |
     text_embeddings$get_model_info()$model_method!=trained_learner$text_model$model_method |
     text_embeddings$get_model_info()$param_seq_length!=trained_learner$text_model$param_seq_length |
     text_embeddings$get_model_info()$param_chunks!=trained_learner$text_model$param_chunks |
     text_embeddings$get_model_info()$param_overlap!=trained_learner$text_model$param_overlap |
     text_embeddings$get_model_info()$param_aggregation!=trained_learner$text_model$param_aggregation
     ){
    stop("Text embedding model of the learner does not match with the text embedding model
         of the supplied text embedding.")
  }

  if(is.null(additional_data)==FALSE){
    datamatrix_analysis<-cbind(text_embeddings$embeddings,additional_data)
  } else {
    datamatrix_analysis<-text_embeddings$embeddings
  }

  datamatrix_analysis<-datamatrix_analysis[,trained_learner$transformation$normalization_matrix[2,]]

  if(is.vector(datamatrix_analysis)==TRUE){
    datamatrix_analysis<-as.data.frame(t(datamatrix_analysis))
    n_cols=length(datamatrix_analysis)
  } else {
    datamatrix_analysis<-as.data.frame(datamatrix_analysis)
    n_cols=ncol(datamatrix_analysis)
  }

  if(na.rm==TRUE){
    datamatrix_analysis<-na.omit(datamatrix_analysis)
  }

  # Normalisierung der Daten, falls im Training erfolgt
  if(trained_learner$transformation$normalization_input==TRUE){
    if(verbose==TRUE){
    print(paste(date(),"Normalize Input Data"))
    }
    for(i in 1:n_cols){
          datamatrix_analysis[i]<-lapply(X=datamatrix_analysis[i],
                                      FUN=normalize_predict,
                                      min_x=as.numeric(trained_learner$transformation$normalization_matrix[3,i]),
                                      max_x=as.numeric(trained_learner$transformation$normalization_matrix[4,i]))
    }
  }

  if(verbose==TRUE){
    print(paste(date(),"Predicting output data"))
  }

  new_prediction<-trained_learner$learner$predict_newdata(as.data.frame(datamatrix_analysis))
  responses<-as.numeric(as.matrix(new_prediction$response))

  if(trained_learner$transformation$normalization_output==TRUE){
    if(verbose==TRUE){
    print(paste(date(),"Re-Normalizing Output Data"))
    }
        responses<-lapply(X=responses,
                        FUN=re_normalize_predict,
                        min_x=trained_learner$transformation$target_min,
                        max_x=trained_learner$transformation$target_max)
        responses<-as.numeric(responses)
  }

  responses<-as.data.frame(cbind(as.character(rownames(datamatrix_analysis)),
                                 as.numeric(responses)))
  colnames(responses)<-c("text_id",
                         trained_learner$category_summary$category_name)

  if(verbose==TRUE){
    print(paste(date(),"done"))
  }

  return(responses)
}


#' Function for predicting data with a trained ensemble AI
#'
#' @export
ai_ensemble_predict<-function(text_embeddings,ensemble_learner,verbose=FALSE){
  predictions<-NULL
  n_coders<-length(ensemble_learner)
  categories<-colnames(ensemble_learner[[1]]$reliability$Iota2_Object$categorical_level$raw_estimates$assignment_error_matrix)
  for(i in 1:n_coders){
    tmp_predictions<-ai_predict(text_embeddings = text_embeddings,
                                trained_learner = ensemble_learner[[i]],
                                verbose = verbose)
    predictions<-cbind(predictions,tmp_predictions[,2])
  }
  colnames(predictions)<-names(ensemble_learner)
  n_categories<-length(ensemble_learner[[1]]$category_summary$categories)
  probabilites_pattern<-matrix(data=NA,
                      nrow=nrow(predictions),
                      ncol=n_categories)
  probabilites<-matrix(data=NA,
                               nrow=nrow(predictions),
                               ncol=n_categories)
  for(i in 1:nrow(predictions)){
    for(j in 1:n_categories){
      tmp_prob<-1
      for(c in 1:n_coders){
        tmp_aem<-ensemble_learner[[c]]$reliability$Iota2_Object$categorical_level$raw_estimates$assignment_error_matrix
        tmp_prob<-tmp_prob*tmp_aem[j,predictions[i,c]]
      }
      probabilites_pattern[i,j]<-tmp_prob
    }
  }

  for(i in 1:nrow(predictions)){
    for(j in 1:n_categories){
      probabilites[i,j]<-probabilites_pattern[i,j]/sum(probabilites_pattern[i,])
    }
  }
  colnames(probabilites)<-categories

  expected_category<-vector(length = nrow(predictions))
  for(i in 1:nrow(predictions)){
    tmp_max<-max(probabilites[i,])
    tmp_index<-match(x=tmp_max,
                     table=probabilites[i,])
    expected_category[i]<-categories[tmp_index]
  }

  results<-cbind(predictions,probabilites,expected_category)
  return(results)
  }
