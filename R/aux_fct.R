normalize <- function(x) {
  return (0.95*(x - min(x)) / (max(x) - min(x)))+0.01
}

normalize_predict<- function(x,min_x,max_x) {
  return (0.95*(x - min_x) / (max_x - min_x))+0.01
}

re_normalize_predict<-function(x, min_x,max_x){
  y=(x-0.01)*(max_x-min_x)/0.95+min_x
  y=round(y)
  return(y)
}

normalize_train <- function(x) {

  return (0.95*(x - min(x)) / (max(x) - min(x)))+0.01
}

matrix_to_array<-function(matrix,
                          times,
                          features){

    tmp_array<-array(
      data = 0,
      dim=c(nrow(matrix),
            times,
            features)
      )

  for(i in 1:nrow(matrix)){
    tmp_sample<-matrix[i,]
    for(j in 1:8){
      tmp_time<-tmp_sample[j*(1:features)]
      tmp_array[i,j,]<-tmp_time
    }
  }
    return(tmp_array)
}

check_embedding_models<-function(object_list){

  #Check if all object are from the same class---------------------------------
  tmp_class<-NULL
  for(i in 1:length(object_list)){
    tmp_class[i]<-list(class(object_list[[i]]))
    if(i>1){
      if(tmp_class[[i-1]][[1]]!=tmp_class[[i]][[1]]){
        return(FALSE)
      }
    }
  }

  #Check if all object have the same model configuration---------------------------------
  tmp_model_config<-NULL
  for(i in 1:length(object_list)){
    if(methods::is(object_list[[i]],"TextEmbeddingModel")){
      tmp_model_config[i]<-list(object_list[[i]]$get_model_info())
    } else if(methods::is(object_list[[i]],"EmbeddedText")){
      tmp_model_config[i]<-list(object_list[[i]]$get_model_info())
    } else if(methods::is(object_list[[i]],"TextEmbeddingClassifier")){
      tmp_model_config[i]<-list(object_list[[i]]$trained_learner$text_model)
    }
    if(i>1){
      for(j in 1:length(tmp_model_config[[i]])){
        tmp_i_1<-tmp_model_config[[i-1]]
        tmp_i<-tmp_model_config[[i]]
        #print(tmp_i_1)
        if(is.null(tmp_i_1[[j]])==TRUE){
          tmp_i_1[[j]]<-"missing"
        }
        if(is.null(tmp_i[[j]])==TRUE){
          tmp_i[[j]]<-"missing"
        }

        if(is.na(tmp_i_1[[j]])==TRUE){
          tmp_i_1[[j]]<-"missing"
        }
        if(is.na(tmp_i[[j]])==TRUE){
          tmp_i[[j]]<-"missing"
        }
        if(tmp_i_1[[j]]!=tmp_i[[j]]){
          return(FALSE)
        }
      }
    }
  }
return(TRUE)
}
