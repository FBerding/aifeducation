#' @title Text Embedding Classifier
#'
#'@export
te_classifier<-R6::R6Class(
  classname = "TextEmbeddingClassifier",
  public = list(
    trained_learner=list(),
    initialize=function(classifier_title,
                        classifier_version,
                        classifier_algorithm,
                        text_embeddings,
                        target_data,
                        category_name,
                        category_label=NULL,
                        additional_data=NULL,
                        learner_name,
                        use_smote=TRUE,
                        #smote_rep=1,
                        autotuning=FALSE,
                        smote_K=NULL,
                        #smote_k_max=6,
                        smote_dup=NULL,
                        #smote_dup_max=5,
                        normalize_input=FALSE,
                        normalize_output=FALSE,
                        n_performance_estimation=30,
                        ratio_performance_estimation=0.75,
                        tuning_method=mlr3tuning::tnr("random_search"),
                        tune_inner_sampling=mlr3::rsmp("subsampling", ratio= .66, repeats=1),
                        max_n_tuning,
                        cr_optim,
                        filter_method="jmim",
                        filter_ratio=1.0,
                        verbose=FALSE,
                        logger_bbotk="warn",
                        logger_mlr3="warn",
                        na.rm=TRUE){

      private$classifier_title<-classifier_title
      private$classifier_version<-classifier_version
      private$classifier_algorithm<-classifier_algorithm

      times=text_embeddings$get_model_info()$param_chunks
      features=ncol(text_embeddings$embeddings)/times

      freq_table<-table(target_data)
      freq_table<-sort(freq_table)
      minor_freq<-freq_table[1:(length(freq_table)-1)]
      major_freq<-freq_table[length(freq_table)]
      min_freq<-min(freq_table,na.rm=TRUE)

      #Check minimal frequencies-----------------------------------------------
      while(min_freq<4){
        if(min_freq<4){
          warning("Frequency of the smalest category is less 4. This category cannot
          be used in training ai. Units with this category are set to NA.")
          target_data<-replace(x=target_data,
                               list=(target_data==names(freq_table[1])),
                               values = NA)
          smote_K<-smote_K[-1]
          smote_dup<-smote_dup[-1]
        }

      freq_table<-table(target_data)
      freq_table<-sort(freq_table)
      minor_freq<-freq_table[1:(length(freq_table)-1)]
      major_freq<-freq_table[length(freq_table)]
      min_freq<-min(freq_table,na.rm=TRUE)
    }

      #Check Smote Configuration-----------------------------------------------
      #for subsampling
      if(tune_inner_sampling$id=="subsampling"){
        #if(min_freq<4){
        #  warning("Frequency of the smalest category is less 4. Smote is not
        #          applicable. Changing use_smote to FALSE.")
        #  use_smote=FALSE
        #} else {
          test_freq<-floor((1-ratio_performance_estimation)*min_freq)
          if(test_freq<1){
            ratio_performance_estimation=1-1/min_freq
            warning(paste("Ratio of parameter ratio_performance_estimation does not
                    ensure that at least one coding unit is part of the test
                    data. Changing parameter to",ratio_performance_estimation))
          }
          tmp_min_cat_size=floor(ratio_performance_estimation*(min_freq))-1
          test_freq_inner<-(1-tune_inner_sampling$param_set$values$ratio)*tmp_min_cat_size
          if(test_freq_inner<1){
            tmp_new_ratio<-1-1/tmp_min_cat_size
            tune_inner_sampling$param_set$values$ratio=tmp_new_ratio
            warning(paste("Ratio of inner sampling does not
                    ensure that at least one coding unit is part of the test
                    data. Changing parameter to",tmp_new_ratio))
          }
        }
        smote_k_limit=floor(0.125*tune_inner_sampling$param_set$values$ratio*ratio_performance_estimation*minor_freq)
        for(i in 1:length(smote_k_limit)){
          if(smote_k_limit[i]<1){
            smote_k_limit[i]=1
          }
        }

        if(sum(smote_K>smote_k_limit)>0)
        {
          smote_K=smote_k_limit
        }
        #print(smote_k_limit)
      #}

        if(verbose==TRUE){
          print(date())
          print(smote_K)
          print(date())
          print(smote_k_limit)
        }

      #Create Learner----------------------------------------------------------
      learner<-get_configurated_learner(
        learner_name=learner_name,
        use_smote = use_smote,
        smote_rep=length(freq_table)-1,
        autotuning = autotuning,
        smote_K = smote_K,
        smote_k_max = smote_k_limit,
        smote_dup = smote_dup,
        smote_dup_max = floor(major_freq/minor_freq),
        times = times,
        features = features)

      #Train Learner-----------------------------------------------------------
      trained_learner<-ai_train(
        text_embeddings=text_embeddings,
        target_data=target_data,
        category_name = category_name,
        category_label=category_label,
        additional_data=additional_data,
        learner=learner,
        normalize_input=normalize_input,
        normalize_output=normalize_output,
        n_performance_estimation=n_performance_estimation,
        ratio_performance_estimation=ratio_performance_estimation,
        tuning_method=tuning_method,
        tune_inner_sampling=tune_inner_sampling,
        max_n_tuning=max_n_tuning,
        cr_optim=cr_optim,
        filter_method=filter_method,
        filter_ratio=filter_ratio,
        verbose=verbose,
        logger_bbotk=logger_bbotk,
        logger_mlr3=logger_mlr3,
        na.rm=na.rm)

      self$trained_learner=trained_learner
    },
    predict=function(text_embeddings,
                     additional_data=NULL,
                     na.rm=TRUE,
                     verbose=FALSE){
      predictions<-ai_predict(
        text_embeddings,
        additional_data=NULL,
        trained_learner=self$trained_learner,
        na.rm=TRUE,
        verbose=FALSE)
      return(predictions)
    },
    set_publication_info=function(autors,
                                  citation,
                                  url){
      private$publication_info$autors=autors
      private$publication_info$citation=citation
      private$publication_info$url=url
    },
    get_publication_info=function(){
      return(private$publication_info)
    },
    set_model_description=function(training,
                                   additional_data){
      private$model_description$training=training
      private$model_description$training=additional_data
    },
    get_model_description=function(){
      return(private$model_description)
    },
    set_license=function(license){
      private$model_license=license
    },
    get_license=function(){
      return(private$model_license)
    },
    get_classifier_info=function(){
      return(list(
        classifier_algorithm=private$classifier_algorithm,
        classifier_title=private$classifier_title,
        classifier_version=private$classifier_version
      ))
    }
  ),
  private=list(
    publication_info=list(
      autors=NULL,
      citation=NULL,
      url=NULL
    ),
    classifier_title=NULL,
    classifier_version=NULL,
    classifier_algorithm=NULL,
    model_license=NULL,
    model_description=list(
      training=NULL,
      additional_data=NULL
    )
  )
)


#' @title Text Embedding Classifier Ensemble
#'
#'@export
te_classifier_ensemble<-R6::R6Class(
  classname = "TextEmbeddingClassifierEnsemble",
  public = list(
    embedding_model=list(),
    te_classifiers=list(),
  #----------------------------------------------------------------------------
  initialize=function(te_classifier_list){
    check_list<-check_embedding_models(te_classifier_list)
    if(check_list==FALSE){
      stop("Text embedding models of the classifiers are not identical.")
    } else {
      for(i in 1:length(te_classifier_list)){
        self$te_classifiers[te_classifier_list[[i]]$get_classifier_info()$classifier_title]<-list(te_classifier_list[[i]])
      }
      self$embedding_model<-te_classifier_list[[1]]$trained_learner$text_model
    }
  },
  #----------------------------------------------------------------------------
  predict=function(text_embeddings,
                   additional_data=NULL,
                   na.rm=TRUE,
                   verbose=FALSE){
    predictions<-NULL
    n_coders<-length(self$te_classifiers)
    categories<-colnames(self$te_classifiers[[1]]$trained_learner$reliability$Iota2_Object$categorical_level$raw_estimates$assignment_error_matrix)
    for(i in 1:n_coders){
      tmp_predictions<-self$te_classifiers[[i]]$predict(
        text_embeddings = text_embeddings,
        additional_data=additional_data,
        na.rm=na.rm,
        verbose=verbose)
      predictions<-cbind(predictions,tmp_predictions[,2])
    }
    colnames(predictions)<-names(self$te_classifiers)
    n_categories<-length(self$te_classifiers[[1]]$trained_learner$category_summary$categories)
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
          tmp_aem<-self$te_classifiers[[c]]$trained_learner$reliability$Iota2_Object$categorical_level$raw_estimates$assignment_error_matrix
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
  )
)
