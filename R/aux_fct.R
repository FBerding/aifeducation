#'Check of compatible text embedding models
#'
#'This function checks if different objects are based on the same text
#'embedding model. This is necessary in order to ensure that classifiers are used
#'only with data generated with compatible embedding models.
#'
#'@param object_list \code{list} of object of class \link{EmbeddedText} or
#'\link{TextEmbeddingClassifierNeuralNet}.
#'@param same_class \code{bool} \code{TRUE} if all object must be from the same class.
#'@return Returns \code{TRUE} if all object refer to the same text embedding model.
#'\code{FALSE} in all other cases.
check_embedding_models<-function(object_list,
                                 same_class=FALSE){
  #Check if the class of the object is TextEmbeddingModel, EmbeddedText or
  #TextEmbeddingClassifierNeuralNet
  for(i in 1:length(object_list)){
    if(!(methods::is(object_list[[i]],"TextEmbeddingModel") |
       methods::is(object_list[[i]],"EmbeddedText") |
       methods::is(object_list[[i]],"TextEmbeddingClassifierNeuralNet"))){
      stop("List contains obcects of the wrong class. Objects must be of class
      TextEmbeddingModel, EmbeddedText or TextEmbeddingClassifierNeuralNet")
    }
  }

  #Check if all object are from the same class---------------------------------
  if(same_class==TRUE){
    tmp_class<-NULL
    for(i in 1:length(object_list)){
      tmp_class[i]<-list(class(object_list[[i]]))
      if(i>1){
        if(tmp_class[[i-1]][[1]]!=tmp_class[[i]][[1]]){
          return(FALSE)
        }
      }
    }
  }

  #Check if all object have the same model configuration---------------------------------
  #field to check
  to_check<-c("model_name","model_date","model_method","model_version","model_language",
              "param_seq_length","param_chunks","param_overlap","param_aggregation")
  tmp_model_config<-NULL
  tmp_results<-NULL
  for(i in 1:length(object_list)){
    if(methods::is(object_list[[i]],"TextEmbeddingModel")){
      if(object_list[[i]]$basic_components$method=="bert"){
        tmp_model_config[i]<-list(object_list[[i]]$get_model_info())
        tmp_model_config[[i]]["model_method"]=list(object_list[[i]]$basic_components$method)
        tmp_model_config[[i]]["param_seq_length"]=object_list[[i]]$basic_components$max_length
        tmp_model_config[[i]]["param_chunks"]=object_list[[i]]$bert_components$chunks
        tmp_model_config[[i]]["param_overlap"]=object_list[[i]]$bert_components$overlap
        tmp_model_config[[i]]["param_aggregation"]=object_list[[i]]$bert_components$aggregation
      } else {
        tmp_model_config[i]<-list(object_list[[i]]$get_model_info())
        tmp_model_config[[i]]["model_method"]=list(object_list[[i]]$basic_components$method)
        tmp_model_config[[i]]["param_seq_length"]=object_list[[i]]$basic_components$max_length
        tmp_model_config[[i]]["param_chunks"]=object_list[[i]]$bow_components$chunks
        tmp_model_config[[i]]["param_overlap"]=object_list[[i]]$bow_components$overlap
        tmp_model_config[[i]]["param_aggregation"]=object_list[[i]]$bow_components$aggregation
      }
    } else if(methods::is(object_list[[i]],"EmbeddedText")){
      tmp_model_config[i]<-list(object_list[[i]]$get_model_info())
    } else if(methods::is(object_list[[i]],"TextEmbeddingClassifierNeuralNet")){
      tmp_model_config[i]<-list(object_list[[i]]$trained_learner$text_model)
    }

    if(i>1){
      for(check in to_check){
        tmp_i_1<-tmp_model_config[[i-1]]
        tmp_i<-tmp_model_config[[i]]
        #print(tmp_i_1)
        if(is.null(tmp_i_1[[check]])==TRUE){
          tmp_i_1[[check]]<-"missing"
        }
        if(is.null(tmp_i[[check]])==TRUE){
          tmp_i[[check]]<-"missing"
        }

        if(identical(tmp_i_1[[check]], integer(0))){
          tmp_i_1[[check]]<-"missing"
        }
        if(identical(tmp_i[[check]], integer(0))){
          tmp_i[[check]]<-"missing"
        }

        if(is.na(tmp_i_1[[check]])==TRUE){
          tmp_i_1[[check]]<-"missing"
        }
        if(is.na(tmp_i[[check]])==TRUE){
          tmp_i[[check]]<-"missing"
        }
        if(as.character(tmp_i_1[[check]])!=as.character(tmp_i[[check]])){
          return(FALSE)
          #tmp_results[check]=FALSE
        } else{
          #tmp_results[check]=TRUE
        }

      }
    }
  }
return(TRUE)
}


#------------------------------------------------------------------------------
#'Calculates reliability measures based on content analysis
#'
#'This functions calculates different reliability measures which are based on the
#'empirical research method of content analysis.
#'
#'@param true_values \code{factor} containing the true labels/categories.
#'@param predicted_values \code{factor} containing the predicted labels/categories.
#'@return Returns a \code{vector} with the following reliability measures:
#'#'\itemize{
#'\item{\strong{iota_index: }}{Iota Index from the Iota Reliability Concept Version 2.}
#'\item{\strong{min_iota2: }}{Minimal Iota from Iota Reliability Concept Version 2.}
#'\item{\strong{avg_iota2: }}{Average Iota from Iota Reliability Concept Version 2.}
#'\item{\strong{max_iota2: }}{Maximum Iota from Iota Reliability Concept Version 2.}
#'\item{\strong{min_alpha: }}{Minmal Alpha Reliability from Iota Reliability Concept Version 2.}
#'\item{\strong{avg_alpha: }}{Average Alpha Reliability from Iota Reliability Concept Version 2.}
#'\item{\strong{max_alpha: }}{Maximum Alpha Reliability from Iota Reliability Concept Version 2.}
#'\item{\strong{static_iota_index: }}{Static Iota Index from Iota Reliability Concept Version 2.}
#'\item{\strong{dynamic_iota_index: }}{Dynamic Iota Index Iota Reliability Concept Version 2.}
#'\item{\strong{kalpha_nominal: }}{Krippendorff's Alpha for nominal variables.}
#'\item{\strong{kalpha_ordinal: }}{Krippendorff's Alpha for ordinal variables.}
#'\item{\strong{kendall: }}{Kendall's coefficient of concordance W.}
#'\item{\strong{kappa2: }}{Cohen's Kappa with equal weights.}
#'\item{\strong{kappa_fleiss: }}{Fleiss' Kappa for multiple raters with exact estimation.}
#'\item{\strong{kappa_light: }}{Light's Kappa for multiple raters.}
#'\item{\strong{percentage_agreement: }}{Percentage Agreement.}
#'\item{\strong{gwet_ac: }}{Gwet's AC1/AC2 agreement coefficient.}
#'}
#'@export
get_coder_metrics<-function(true_values,
                            predicted_values){
val_res=iotarelr::check_new_rater(true_values = true_values,
                                  assigned_values = predicted_values,
                                  free_aem = TRUE)

  metric_names=c("iota_index",
                 "min_iota2",
                 "avg_iota2",
                 "max_iota2",
                 "min_alpha",
                 "avg_alpha",
                 "max_alpha",
                 "static_iota_index",
                 "dynamic_iota_index",
                 "kalpha_nominal",
                 "kalpha_ordinal",
                 "kendall",
                 "kappa2",
                 "kappa_fleiss",
                 "kappa_light",
                 "percentage_agreement",
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
                                               weight = "equal",
                                               sort.levels = FALSE)$value
  metric_values[14]=irr::kappam.fleiss(ratings=cbind(true_values,predicted_values),
                                                       exact = TRUE,
                                                       detail = FALSE)$value
  metric_values[15]=irr::kappam.light(ratings=cbind(true_values,predicted_values))$value
  metric_values[16]=irr::agree(ratings=cbind(true_values,predicted_values),
                                               tolerance = 0)$value/100
  metric_values[17]=irrCAC::gwet.ac1.raw(ratings=cbind(true_values,predicted_values))$est$coeff.val

  return(metric_values)
}

#------------------------------------------------------------------------------
#'Function for splitting data into a train and validation sample
#'
#'This function creates a train and validation sample based on stratified random
#'sampling. The relative frequencies of each category in the train and validation sample
#'equal the relative frequencies of the initial data (proportional stratified sampling).
#'
#'@param embedding Object of class \link{EmbeddedText}.
#'@param target Named \code{factor} containing the labels of every case.
#'@param val_size \code{double} Ratio between 0 and 1 indicating the relative
#'frequency of cases which should be used as validation sample.
#'@return Returns a \code{list} with the following components.
#'\itemize{
#'\item{\code{target_train: }}{Named \code{factor} containing the labels of the training sample.}
#'
#'\item{\code{embeddings_train: }}{Object of class \link{EmbeddedText} containing the text embeddings for the training sample}
#'
#'\item{\code{target_test: }}{Named \code{factor} containing the labels of the validation sample.}
#'
#'\item{\code{embeddings_test: }}{Object of class \link{EmbeddedText} containing the text embeddings for the validation sample}
#'}
get_train_test_split<-function(embedding,
                               target,
                               val_size){
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
#'Creates Cross-Validation Samples
#'
#'Functions creates cross-validation samples and ensures that the relative
#'frequency for every category/label within a fold equals the relative frequency of
#'the category/label within the initial data.
#'
#'@param target Named \code{factor} containing the relevant labels/categories. Missing cases
#'should be declared with \code{NA}.
#'@param k_folds \code{int} number of folds.
#'
#'@return Return a \code{list} with the following components:
#'\itemize{
#'\item{\code{val_sample: }}{\code{vector} of \code{strings} containing the names of cases of the validation sample.}
#'
#'\item{\code{train_sample: }}{\code{vector} of \code{strings} containing the names of cases of the train sample.}
#'
#'\item{\code{n_folds: }}{\code{int} Number of realized folds.}
#'
#'\item{\code{unlabeled_cases: }}{\code{vector} of \code{strings} containing the names of the unlabeled cases.}
#'}
#'@note The parameter \code{target} allows cases with missing categories/labels.
#'These should be declared with \code{NA}. All these cases are ignored for creating the
#'different folds. Their names are saved within the component \code{unlabeled_cases}.
#'These cases can be used for Pseudo Labeling.
#'@note the functions checks the absolute frequencies of every category/label. If the
#'absolute frequency is not sufficient to ensure at least four cases in every fold
#'the number of folds is adjusted. In these cases a warning is printed to the console.
#'At least four cases per fold are necessary to ensure that the training of
#'\link{TextEmbeddingClassifierNeuralNet} works well with all options turned on.
get_folds<-function(target,
                    k_folds){
  sample_target=na.omit(target)
  freq_cat=table(sample_target)
  categories=names(freq_cat)
  min_freq=min(freq_cat)

  if(min_freq/k_folds<1){
    fin_k_folds=floor(min_freq/1)
    warning(paste("Frequency of the smallest category/label is not sufficent to ensure
                  at least 1 cases per fold. Adjusting number of folds from ",k_folds,"to",fin_k_folds,"."))
    if(fin_k_folds==0){
      stop("Frequency of the smallest category/label is to low. Please check your data.
           Consider to remove all categories/labels with a very low absolute frequency.")
    }
  } else {
    fin_k_folds=k_folds
  }

  val_sample=NULL
  for(cat in categories){
    all_names=names(subset(target,target==cat))
    used_names=NULL

    for(i in 1:fin_k_folds){
      if(i==1){
        possible_names=all_names
      } else {
        possible_names=setdiff(x=all_names,
                               y=used_names)
      }
      tmp_size=ceiling(length(possible_names)/(fin_k_folds-(i-1)))
      selected_names<-sample(x=possible_names,
                             size=tmp_size,
                             replace=FALSE)
      val_sample[i]=list(append(x=unlist(val_sample[i]),
                           values = selected_names))
      used_names=append(used_names,values = selected_names)
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
#'Splits data into labeled and unlabeled data
#'
#'This functions splits data into labeled and unlabeled data.
#'
#'@param embedding Object of class \link{EmbeddedText}.
#'@param target Named \code{factor} containing all cases with labels and missing
#'labels.
#'@return Returns a \code{list} with the following components
#'\itemize{
#'\item{\code{embeddings_labeled: }}{Object of class \link{EmbeddedText} containing
#'only the cases which have labels.}
#'
#'\item{\code{embeddings_unlabeled: }}{Object of class \link{EmbeddedText} containing
#'only the cases which have no labels.}
#'
#'\item{\code{targets_labeled: }}{Named \code{factor} containing the labels of
#'relevant cases.}
#'}
split_labeled_unlabeled<-function(embedding,
                                  target){
  target_labeled=subset(target,is.na(target)==FALSE)
  embedding_labeled=embedding$embeddings[names(target_labeled),]
  embedding_unlabeled=embedding$embeddings[setdiff(names(target),names(target_labeled)),]

  result<-list(embeddings_labeled=embedding_labeled,
               embeddings_unlabeled=embedding_unlabeled,
               targets_labeled=target_labeled)
  return(result)
}
#------------------------------------------------------------------------------
#'Creates in iota2 object
#'
#'Function creates an object of class \code{iotarelr_iota2} which can be used
#'with the package iotarelr. This function is for internal use only.
#'
#'@param iota2_list \code{list} of objects of class \code{iotarelr_iota2}.
#'@param free_aem \code{bool} \code{TRUE} if the iota2 objects are estimated
#'without forcing the assumption of weak superiority.
#'@param call \code{string} characterizing the source of estimation. That is, the
#'function within the object was estimated.
#'@param original_cat_labels \code{vector} containing the original labels of each
#'category.
#'@return Returns an object of class \code{iotarelr_iota2} which is the mean
#'iota2 object.
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
#'Creates synthetic cases for balancing training data
#'
#'This function creates synthetic cases for balancing the training with an
#'object of class \link{TextEmbeddingClassifierNeuralNet}.
#'
#'@param embedding Named \code{data.frame} containing the text embeddings.
#'In most cases this object is taken from \link[=EmbeddedText]{EmbeddedText$embeddings}.
#'@param target Named \code{factor} containing the labels of the corresponding embeddings.
#'@param method \code{vector} containing strings of the requested methods for generating new cases.
#'Currently "smote","dbsmote", and "adas" from package smotefamily are available.
#'@param max_k \code{int} The maximum number of nearest neighbors during sampling process.
#'@return \code{list} with the following components.
#'\itemize{
#'\item{\code{syntetic_embeddings: }}{Named \code{data.frame} containing the text embeddings of
#'the synthetic cases.}
#'
#'\item{\code{syntetic_targets}}{Named \code{factor} containing the labels of the corresponding
#'synthetic cases.}
#'
#'\item{\code{n_syntetic_units}}{\code{table} showing the number of synthetic cases for every
#'label/category.}
#'}
#'@export
get_synthetic_cases<-function(embedding,
                              target,
                              method=c("smote"),
                              max_k=6){

  min_k=max_k

  cat_freq=table(target)
  categories=names(cat_freq)

  index=1
  input=NULL
  for(cat in categories){
    for(m in 1:length(method)){
      if(method[m]!="dbsmote"){
          for (k in min_k:max_k){

            #If k exceeds the possible range reduce to a viable number
            if(k>=cat_freq[cat]){
              k_final=cat_freq[cat]-1
            } else {
              k_final=k
            }
            input[[index]]<-list(cat=cat,
                                 k=k_final,
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
          cat_freq=cat_freq)
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
#'Function for creating synthetic cases in order to balance the data for
#'training with \link{TextEmbeddingClassifierNeuralNet}. This is a helper
#'function for use with \link{get_synthetic_cases} to allow parallel
#'computations.
#'
#'@param embedding Named \code{data.frame} containing the text embeddings.
#'In most cases this object is taken from \link[=EmbeddedText]{EmbeddedText$embeddings}.
#'@param target Named \code{factor} containing the labels/categories of the corresponding cases.
#'@param k \code{int} The number of nearest neighbors during sampling process.
#'@param max_k \code{int} The maximum number of nearest neighbors during sampling process.
#'@param method \code{vector} containing strings of the requested methods for generating new cases.
#'Currently "smote","dbsmote", and "adas" from package smotefamily are available.
#'@param cat \code{string} The category for which new cases should be created.
#'@param cat_freq Object of class \code{"table"} containing the absolute frequencies
#'of every category/label.
#'@returns Returns a \code{list} which contains the text embeddings of the
#'new synthetic cases as a named \code{data.frame} and their labels as a named
#'\code{factor}.
#'@export
create_synthetic_units<-function(embedding,
                                 target,
                                 k,
                                 max_k,
                                 method,
                                 cat,
                                 cat_freq){

  if((cat_freq[cat]!=max(cat_freq)) & (k<=min(max_k,cat_freq[cat]-1))){

    tmp_target=(target==cat)
    n_minor=sum(tmp_target)
    n_major=max(cat_freq)

    tmp_ration_necessary_cases=n_minor/n_major

    syn_data=NULL
      if(method=="smote" & tmp_ration_necessary_cases<1){
        syn_data=smotefamily::SMOTE(X=embedding,
                                    target = tmp_target,
                                    K=k,
                                    dup_size = n_major/n_minor)
      } else if(method=="adas" & tmp_ration_necessary_cases<1){
        syn_data=smotefamily::ADAS(X=embedding,
                                   target = tmp_target,
                                   K=k)
      } else if(method=="dbsmote" & tmp_ration_necessary_cases<1){
        syn_data=smotefamily::DBSMOTE(X=embedding,
                                      target = tmp_target,
                                      dupSize = 2*n_major/n_minor,
                                      MinPts = NULL,
                                      eps = NULL)
      }

      if(is.null(syn_data)==FALSE){
        tmp_data=syn_data$syn_data[,-ncol(syn_data$syn_data)]
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
#'Creates a stratified random sample
#'
#'This function creates a stratified random sample.The difference to
#'\link{get_train_test_split} is that this function does not require text
#'embeddings and does not split the text embeddings into a train and validation
#'sample.
#'
#'@param targets Named \code{vector} containing the labels/categories for each case.
#'@param val_size \code{double} Value between 0 and 1 indicating how many cases of
#'each label/category should be part of the validation sample.
#'@return \code{list} which containing the names of cases belonging to the train
#'sample and to the validation sample.
get_stratified_train_test_split<-function(targets, val_size=0.25){
  test_sample=NULL
  categories=names(table(targets))

  for(cat in categories){
    condition=(targets==cat)
    tmp=names(subset(x = targets,
                     subset = condition))
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

