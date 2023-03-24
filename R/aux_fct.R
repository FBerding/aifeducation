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
    for(j in 1:times){
      tmp_time<-tmp_sample[(1:features)+(j-1)*(features)]
      tmp_array[i,j,]=as.numeric(tmp_time)
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


#------------------------------------------------------------------------------
get_coder_metrics<-function(true_values,predicted_values){
val_res=iotarelr::check_new_rater(true_values = true_values,
                                  assigned_values = predicted_values,
                                  free_aem = TRUE)

  metric_names=c("iota_index",
                 "min_iota2",
                 "avg_iota2",
                 "max_iota2",
                 "min_alpha",
                 "avg_alpha",
                 "avg_alpha",
                 "static_iota_index",
                 "dynamic_iota_index",
                 "kalpha_nominal",
                 "kalpha_ordinal",
                 "kendall",
                 "kappa2",
                 "kappa_fleiss",
                 "kappa_light",
                 "percentage_agreement",
                 "intra_cat_agree",
                 "gwet_ac")
  metric_values=vector(length = length(metric_names))
  names(metric_values)=metric_names

  val_res=iotarelr::check_new_rater(true_values = true_values,
                                    assigned_values = predicted_values,
                                    free_aem = FALSE)
  val_res_free=iotarelr::check_new_rater(true_values = true_values,
                                         assigned_values = predicted_values,
                                         free_aem = TRUE)

  metric_values[1]=val_res$scale_level$iota_index

  metric_values[2]=min(val_res_free$categorical_level$raw_estimates$iota)
  metric_values[3]=mean(val_res_free$categorical_level$raw_estimates$iota)
  metric_values[4]=max(val_res_free$categorical_level$raw_estimates$iota)

  metric_values[5]=min(val_res_free$categorical_level$raw_estimates$alpha_reliability)
  metric_values[6]=mean(val_res_free$categorical_level$raw_estimates$alpha_reliability)
  metric_values[7]=max(val_res_free$categorical_level$raw_estimates$alpha_reliability)

  metric_values[8]=val_res$scale_level$iota_index_d4
  metric_values[9]=val_res$scale_level$iota_index_dyn2

  metric_values[10]=irr::kripp.alpha(x=rbind(true_values,predicted_values),
                                                    method = "nominal")$value
  metric_values[11]=irr::kripp.alpha(x=rbind(true_values,predicted_values),
                                                    method = "ordinal")$value
  metric_values[12]=irr::kendall(ratings=cbind(true_values,predicted_values),
                                                correct=TRUE)$value
  metric_values[13]=irr::kappa2(ratings=cbind(true_values,predicted_values),
                                               weight = "unweighted",
                                               sort.levels = FALSE)$value
  metric_values[14]=irr::kappam.fleiss(ratings=cbind(true_values,predicted_values),
                                                       exact = TRUE,
                                                       detail = FALSE)$value
  metric_values[15]=irr::kappam.light(ratings=cbind(true_values,predicted_values))$value
  metric_values[16]=irr::agree(ratings=cbind(true_values,predicted_values),
                                               tolerance = 0)$value/100
  #metric_values[16]=sum(diag(table(cbind(true_values,factor(predicted_values)))))/sum(table(true_values))
  #val_metric[17]=mean(diag(table(true_values,predicted_values))/table(true_values))
  metric_values[18]=irrCAC::gwet.ac1.raw(ratings=cbind(true_values,predicted_values))$est$coeff.val

  return(metric_values)
}

#------------------------------------------------------------------------------
get_train_test_split<-function(embedding,target,val_size){
  categories=names(table(target))
  val_sampe=NULL
  for(cat in categories){
    tmp=subset(target,target==cat)
    val_sampe[cat]=list(
      sample(names(tmp),size=max(1,length(tmp)*val_size))
    )
  }
  val_data=target[unlist(val_sampe)]
  train_data=target[setdiff(names(target),names(val_data))]

  val_embeddings=embedding$clone(deep=TRUE)
  val_embeddings$embeddings=val_embeddings$embeddings[names(val_data),]
  val_embeddings$embeddings=na.omit(val_embeddings$embeddings)
  train_embeddings=embedding$clone(deep=TRUE)
  train_embeddings$embeddings=train_embeddings$embeddings[names(train_data),]
  train_embeddings$embeddings=na.omit(train_embeddings$embeddings)

  results<-list(target_train=train_data,
                embeddings_train=train_embeddings,
                target_test=val_data,
                embeddings_test=val_embeddings)
  return(results)
}
#-----------------------------------------------------------------------------
get_folds<-function(target,k_folds){
  sample_target=na.omit(target)
  freq_cat=table(sample_target)
  categories=names(freq_cat)
  min_freq=min(freq_cat)

  if(k_folds>(min_freq/2)){
    fin_k_folds=floor(min_freq/2)
    warning(paste("Frequency of the smallest category is less the requested number of
            folds. Adjusting number of folds from ",k_folds,"to",fin_k_folds,"."))
  } else {
    fin_k_folds=k_folds
  }

  val_sample=NULL
  sizes=ceiling(freq_cat/fin_k_folds)
  for(i in 1:fin_k_folds){

    for(cat in categories){
      all_names=names(subset(target,target==cat))

      if(is.null(val_sample)==TRUE){
        possible_names=all_names
      } else {
        possible_names=setdiff(x=all_names,
                               y=unlist(val_sample))
      }
      selected_names=sample(x=possible_names,
                            size=min(length(possible_names),
                                     sizes[cat]))
      val_sample[i]=list(append(x=unlist(val_sample[i]),
                           values = selected_names))
    }
  }
  train_sample=NULL
  for(i in 1:fin_k_folds){
    train_sample[i]=list(setdiff(x=names(sample_target),y=val_sample[[i]]))
  }

  unlabeled_cases=setdiff(x=names(target),y=c(val_sample[[1]],train_sample[[1]]))

  results<-list(val_sample=val_sample,
                train_sample=train_sample,
                n_folds=fin_k_folds,
                unlabeled_cases=unlabeled_cases)
  return(results)
}
#------------------------------------------------------------------------------
split_labeled_unlabeled<-function(embedding,target){
  target_labeled=subset(target,is.na(target)==FALSE)
  embedding_labeled=embedding$embeddings[names(target_labeled),]
  embedding_unlabeled=embedding$embeddings[setdiff(names(target),names(target_labeled)),]

  result<-list(embeddings_labeled=embedding_labeled,
               embeddings_unlabeled=embedding_unlabeled,
               targets_labeled=target_labeled)
  return(result)
}
#------------------------------------------------------------------------------
create_iota2_mean_object<-function(iota2_list,
                                   free_aem=FALSE,
                                   call="aifeducation::te_classifier_neuralnet",
                                   original_cat_labels){

  if(free_aem==TRUE){
    call=paste0(call,"_free_aem")
  }

    mean_aem<-NULL
    mean_categorical_sizes<-NULL
    n_performance_estimation=length(iota2_list)

    for(i in 1:length(iota2_list)){
      if(i==1){
        mean_aem<-iota2_list[[i]]$categorical_level$raw_estimates$assignment_error_matrix

      } else {
        mean_aem<-mean_aem+iota2_list[[i]]$categorical_level$raw_estimates$assignment_error_matrix
      }
    }

    mean_aem<-mean_aem/n_performance_estimation
    mean_categorical_sizes<-iota2_list[[i]]$information$est_true_cat_sizes
    #mean_categorical_sizes<-mean_categorical_sizes/n_performance_estimation

    colnames(mean_aem)<-original_cat_labels
    rownames(mean_aem)<-original_cat_labels

    names(mean_categorical_sizes) <- original_cat_labels
    tmp_iota_2_measures <- iotarelr::get_iota2_measures(
      aem = mean_aem,
      categorical_sizes = mean_categorical_sizes,
      categorical_levels = original_cat_labels)

    Esimtates_Information <- NULL
    Esimtates_Information["log_likelihood"] <- list(NA)
    Esimtates_Information["iteration"] <- list(NA)
    Esimtates_Information["convergence"] <- list(NA)
    Esimtates_Information["est_true_cat_sizes"] <- list(mean_categorical_sizes)
    Esimtates_Information["conformity"] <- list(iotarelr::check_conformity_c(aem = mean_aem))
    #Esimtates_Information["conformity"] <- list(NA)
    Esimtates_Information["boundaries"] <- list(NA)
    Esimtates_Information["p_boundaries"] <- list(NA)
    Esimtates_Information["n_rater"] <- list(1)
    Esimtates_Information["n_cunits"] <- list(iota2_list[[i]]$information$n_cunits)
    Esimtates_Information["call"] <- list(call)
    Esimtates_Information["random_starts"] <- list(NA)
    Esimtates_Information["estimates_list"] <- list(NA)

    iota2_object <- NULL
    iota2_object["categorical_level"] <- list(tmp_iota_2_measures$categorical_level)
    iota2_object["scale_level"] <- list(tmp_iota_2_measures$scale_level)
    iota2_object["information"] <- list(Esimtates_Information)
    class(iota2_object) <- "iotarelr_iota2"

    return(iota2_object)
}

#-----------------------------------------------------------------------------
#------------------------------------------------------------------------------
get_synthetic_cases<-function(embedding,
                              target,
                              method=c("smote"),
                              inc_major=FALSE,
                              max_k=6){
  cat_freq=table(target)
  categories=names(cat_freq)

  index=1
  input=NULL
  for(cat in categories){
    for(m in 1:length(method)){
      if(method[m]!="dbsmote"){
          for (k in 2:max_k){
            input[[index]]<-list(cat=cat,
                                 k=k,
                                 method=method[m])
            index=index+1
          }
        } else {
            input[[index]]<-list(cat=cat,
                                 k=0,
                                 method=method[m])
            index=index+1
        }
      }
    }

      result_list<-foreach(index=1:length(input),.export="create_synthetic_units")%dopar%{
        create_synthetic_units(
          embedding=embedding,
          target=target,
          k=input[[index]]$k,
          max_k = max_k,
          method=input[[index]]$method,
          cat=input[[index]]$cat,
          cat_freq=cat_freq,
          inc_major=inc_major)
      }

      syntetic_embeddings=data.frame()
      syntetic_targets=NULL

  for(i in 1:length(result_list)){
    if(is.null(result_list[[i]]$syntetic_embeddings)==FALSE){
      if(nrow(syntetic_embeddings)>0){
        n_row=nrow(syntetic_embeddings)
        syntetic_embeddings[(n_row+1):(n_row+nrow(result_list[[i]]$syntetic_embeddings)),]<-result_list[[i]]$syntetic_embeddings
        syntetic_targets=append(syntetic_targets,values = result_list[[i]]$syntetic_targets)
      } else {
      syntetic_embeddings=result_list[[i]]$syntetic_embeddings
      syntetic_targets=result_list[[i]]$syntetic_targets
    }
    }
  }

  #syntetic_embeddings=unique(syntetic_embeddings)
  #syntetic_targets=syntetic_targets[rownames(syntetic_embeddings)]

  n_syntetic_units=table(syntetic_targets)

  results=NULL
  results["syntetic_embeddings"]=list(syntetic_embeddings)
  results["syntetic_targets"]=list(syntetic_targets)
  results["n_syntetic_units"]=list(n_syntetic_units)

  return(results)
}
#---------------------------------------------

#'Creates Synthetic Units
#'
#'@export
create_synthetic_units<-function(embedding,
                                 target,
                                 k,
                                 max_k,
                                 method,
                                 cat,
                                 cat_freq,
                                 inc_major){
  if((cat_freq[cat]!=max(cat_freq) | inc_major==TRUE) &
     k<=min(max_k,cat_freq[cat]-1)){

    tmp_target=(target==cat)
    tmp_ration_necessary_cases=sum(tmp_target)/(sum(!tmp_target))

    syn_data=NULL
      if(method=="smote" & tmp_ration_necessary_cases<1){
        syn_data=smotefamily::SMOTE(X=embedding,
                                    target = tmp_target,
                                    K=k,
                                    dup_size = 1)
      } else if(method=="adas" & tmp_ration_necessary_cases<1){
        syn_data=smotefamily::ADAS(X=embedding,
                                   target = tmp_target,
                                   K=k)
      } else if(method=="dbsmote" & tmp_ration_necessary_cases<1){
        syn_data=smotefamily::DBSMOTE(X=embedding,
                                      target = tmp_target,
                                      dupSize = 1,
                                      MinPts = NULL,
                                      eps = NULL)
      }

      if(is.null(syn_data)==FALSE){
        tmp_data=syn_data$syn_data[,-ncol(syn_data$syn_data)]
        #print(paste("m:",method,"cat:",cat,"k:",k,"row:",nrow(tmp_data)))
        rownames(tmp_data)<-paste0(method,"_",cat,"_",k,"_",
                                   seq(from=1,to=nrow(tmp_data),by=1))
        tmp_data<-as.data.frame(tmp_data)
        tmp_target=rep(cat,times=nrow(tmp_data))
        names(tmp_target)=rownames(tmp_data)

        results<-list(syntetic_embeddings=tmp_data,
                      syntetic_targets=tmp_target)
      } else {
        results<-list(syntetic_embeddings=NULL,
                      syntetic_targets=NULL)
      }
    } else {
      results<-list(syntetic_embeddings=NULL,
                    syntetic_targets=NULL)
    }
  return(results)
}
#-------------------------------------------------------------------------------
get_stratified_train_test_split<-function(targets, val_size=0.25){
  test_sample=NULL
  categories=names(table(targets))

  for(cat in categories){
    tmp=names(subset(targets,targets==cat))
    test_sample[cat]=list(
      sample(tmp,size=max(1,length(tmp)*val_size))
    )
  }
  test_sample=unlist(test_sample,use.names = FALSE)
  train_sample=setdiff(names(targets),test_sample)

  results<-list(test_sample=test_sample,
                train_sample=train_sample)
  return(results)
}

