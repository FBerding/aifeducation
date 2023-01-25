#' Training of Artificial Intelligence
#'
#' Function for training an AI with several configurations and options
#' provided by several packages (e.g. mlr3, text2vec).
#'
#' @importFrom varhandle check.numeric
#' @importFrom mlr3filters flt
#' @importFrom lgr get_logger
#' @importFrom stats na.omit
#' @importFrom mlr3 rsmp
#' @importFrom mlr3tuning tnr
#' @importFrom mlr3tuning tune
#' @importFrom irr kripp.alpha
#' @importFrom irr kendall
#' @importFrom irr kappa2
#' @importFrom irr kappam.fleiss
#' @importFrom irr kappam.light
#' @importFrom irr agree
#' @importFrom irr kripp.alpha
#' @importFrom stats sd
#' @importFrom stats quantile
#' @importFrom iotarelr get_iota2_measures
#' @importFrom iotarelr check_conformity_c
#' @importFrom stats qnorm
#' @importFrom mlr3misc insert_named
#' @importFrom mlr3 TaskClassif
#' @export
ai_train<-function(text_embeddings,
                   use_dim_reduction,
                   dim_red_n,
                   dim_red_method,
                   target_data,
                   category_name,
                   category_label=NULL,
                   additional_data,
                   learner,
                   normalize_input=FALSE,
                   normalize_output=FALSE,
                   n_performance_estimation=30,
                   ratio_performance_estimation=0.75,
                   tuning_method=mlr3tuning::tnr("random_search"),
                   tune_inner_sampling,
                   max_n_tuning,
                   cr_optim,
                   filter_method="jmim",
                   filter_ratio=1.0,
                   verbose=FALSE,
                   logger_bbotk="warn",
                   logger_mlr3="warn",
                   na.rm=TRUE){

  lgr::get_logger("mlr3")$set_threshold(logger_bbotk)
  lgr::get_logger("bbotk")$set_threshold(logger_mlr3)

  if(verbose==TRUE){
    print(paste(date(),"Beginning Training of AI for",category_name))
  }

  tmp_param_set<-learner$param_set$search_space()
  if(tmp_param_set$is_empty==TRUE){
    tune=FALSE
  } else {
    tune=TRUE
  }

  ##Data Preparation-----------------------------------------------------------
  data_analysis_start<-text_embeddings$embeddings

  if(is.null(additional_data)==FALSE){
    data_analysis_start<-cbind(data_analysis_start,additional_data,target_data)
  } else {
    data_analysis_start<-cbind(data_analysis_start,target_data)
  }
  data_analysis_start<-as.data.frame(data_analysis_start)

  #Removing cases with NA------------------------------------------------------
  if(na.rm==TRUE){
    if(verbose==TRUE){
      print(paste(date(),"Removing Cases with NA"))
    }
    data_analysis_start<-stats::na.omit(data_analysis_start)
    if(verbose==TRUE){
      print(paste(date(),"Cases Reduced from",nrow(text_embeddings$embeddings),
                  "to",nrow(data_analysis_start)))
    }
  }
  data_analysis<-data_analysis_start[,1:(ncol(data_analysis_start)-1)]

  ##Apply Dimensionreduction---------------------------------------------------
  if(use_dim_reduction==TRUE){
    if(verbose==TRUE){
      print(paste(date(),"Reducing Dimensions with",dim_red_method,
                  "from",ncol(data_analysis),
                  "to",dim_red_n))
    }
    if(dim_red_method=="PCA"){
      res_pca<-prcomp(x=data_analysis,center = TRUE)
      pca_loadings<-res_pca$rotation
      columns_means<-colMeans(data_analysis)
      data_analysis=scale(as.matrix(data_analysis),
                          center=TRUE,
                          scale=FALSE)%*%as.matrix(pca_loadings[,1:dim_red_n])
      dim_red_model=NULL
      dim_red_model["loadings"]<-list(pca_loadings[,1:dim_red_n])
      dim_red_model["columns_means"]<-list(columns_means)
    }
    data_analysis<-unname(data_analysis)
    colnames(data_analysis)<-colnames(data_analysis,prefix = "rd_",do.NULL=FALSE)
    data_analysis<-as.data.frame(data_analysis)
  }

  data_analysis$target_data<-data_analysis_start[,ncol(data_analysis_start)]

  #target_data<-basic_text_rep$dtm@docvars[,category_name]

  target_max=max(target_data,na.rm = TRUE)
  target_min=min(target_data,na.rm = TRUE)
  original_cat_labels<-names(table(target_data))
  #print(original_cat_labels)

  datamatrix_non_normalized<-data_analysis
  if(normalize_input==TRUE)
  {
    if(verbose==TRUE){
      print(paste(date(),"Normalizing Input Data"))
    }
    for(i in 1:(ncol(data_analysis)-1)){
      if(min(data_analysis[[i]])!=max(data_analysis[[i]])){
        data_analysis[i]<-normalize_train(data_analysis[[i]])
      }
    }
  }
  if(normalize_output==TRUE){
    if(verbose==TRUE){
      print(paste(date(),"Normalizing Output Data"))
    }
      data_analysis[ncol(data_analysis)]<-normalize_train(data_analysis[[ncol(data_analysis)]])
  }
  data_analysis$target_data<-as.factor(data_analysis$target_data)

  #----------------------------------------------------------------------------
  n_sampling<-nrow(data_analysis)
  n_p_test<-floor(nrow(data_analysis)*(1-ratio_performance_estimation))
  n_p_train<-n_sampling-n_p_test
  n_iteration<-n_performance_estimation

  max_features=ncol(data_analysis)-1
  n_features<-floor(filter_ratio*max_features)
  filter=mlr3filters::flt(filter_method)
  results_filtervalues<-matrix(data=NA,
                               nrow=n_performance_estimation,
                               ncol=(ncol(data_analysis)-1))
  colnames(results_filtervalues)<-colnames(data_analysis)[1:(ncol(data_analysis)-1)]


  task_training<-mlr3::TaskClassif$new(id="training",
                                       backend = data_analysis,
                                       target="target_data")
  task_training$col_roles$stratum="target_data"

  outer_sampling<-mlr3::rsmp("subsampling",
                             ratio = ratio_performance_estimation,
                             repeats = n_performance_estimation)
  train_set_split<-outer_sampling$instantiate(task_training)




  ##Hyperparametertuning-----------------------------------------------------
  results_performance<-matrix(data=NA,
                              nrow = 10+1,
                              ncol=n_performance_estimation)
  rownames(results_performance)<-c(
    "iteration",
    "iota_index",
    "iota_index_d4",
    "iota_index_dyn2",
    "kripp.alpha.nominal",
    "kripp.alpha.ordinal",
    "kendall_tau",
    "cohen_kappa",
    "fleiss_kappa",
    "light_kappa",
    "percentage_agreement"
  )
  iota2_list<-NULL
  hyperparameter_all<-NULL
  hyperparameter_best<-NULL

  if(verbose==TRUE){
    print(paste(date(),"Starting Performance Estimation"))
  }

  for(i in 1:n_performance_estimation)
  {
    #print(i)
    train_set = train_set_split$train_set(i)
    test_set = train_set_split$test_set(i)
    #print(i)
    if(filter_ratio<1){
      task_filter<-mlr3::TaskClassif$new(id="filter_estimation",
                                         backend = data_analysis[train_set,],
                                         target="target_data")
      task_filter$col_roles$stratum="target_data"
      #print(i)
      filter$calculate(task_filter)
      #abc<-filter
      Liste_Features<-as.vector(rownames(as.matrix(filter$scores[1:floor(filter_ratio*max_features)])))
      for (j in 1:(ncol(data_analysis)-1)){
        results_filtervalues[i,names(filter$scores)[j]]<-filter$scores[names(filter$scores)[j]]
      }
    } else {
      Liste_Features<-colnames(data_analysis)[1:(ncol(data_analysis)-1)]
    }

    #print(i)
    task_performance<-mlr3::TaskClassif$new(id="performance_estimation",
                                            backend = data_analysis[c(Liste_Features,"target_data")],
                                            target="target_data")
    task_performance$col_roles$stratum="target_data"
    #print(i)
    tmp_learner<-learner$clone(deep = TRUE)
    if(tune==TRUE){
      tune_results<-mlr3tuning::tune(
        method = tuning_method,
        task = task_performance,
        learner = tmp_learner,
        resampling = tune_inner_sampling,
        measures = cr_optim,
        term_evals = max_n_tuning
      )
      tmp_learner$param_set$values = tune_results$result_learner_param_vals
    }
    tmp_learner$train(task_performance,row_ids=train_set)
    #invisible(tmp_learner$train(task_performance,row_ids=train_set))
    prediction<-tmp_learner$predict(task_performance,row_ids = test_set)

    tmp_iota2<-iotarelr::check_new_rater(
      true_values = as.character(prediction$truth),
      assigned_values = as.character(prediction$response),
      con_trace = FALSE)

    iota2_list[paste0("iter",i)]<-list(tmp_iota2)

    results_performance[1,i]<-i
    results_performance[2,i]<-iota2_list[[paste0("iter",i)]]$scale_level$iota_index
    results_performance[3,i]<-iota2_list[[paste0("iter",i)]]$scale_level$iota_index_d4
    results_performance[4,i]<-iota2_list[[paste0("iter",i)]]$scale_level$iota_index_dyn2
    results_performance[5,i]<-irr::kripp.alpha(x=t(cbind(as.character(prediction$truth),
                                                       as.character(prediction$response))),
                                               method = "nominal")$value
    results_performance[6,i]<-irr::kripp.alpha(x=t(cbind(as.character(prediction$truth),
                                                         as.character(prediction$response))),
                                               method = "ordinal")$value
    results_performance[7,i]<-irr::kendall(ratings=cbind(as.character(prediction$truth),
                                                         as.character(prediction$response)),
                                            correct=TRUE)$value
    results_performance[8,i]<-irr::kappa2(ratings=cbind(as.character(prediction$truth),
                                                         as.character(prediction$response)),
                                           weight = "unweighted",
                                          sort.levels = FALSE)$value
    results_performance[9,i]<-irr::kappam.fleiss(ratings=cbind(as.character(prediction$truth),
                                                        as.character(prediction$response)),
                                                exact = TRUE,
                                                detail = FALSE)$value
    results_performance[10,i]<-irr::kappam.light(ratings=cbind(as.character(prediction$truth),
                                                               as.character(prediction$response)))$value
    results_performance[11,i]<-irr::agree(ratings=cbind(as.character(prediction$truth),
                                                              as.character(prediction$response)),
                                         tolerance = 0)$value/100
    if(tune==TRUE){
      #hyperparameter_best[paste0("iter",i)]<-list(
      hyperparameter_best<-cbind(hyperparameter_best,
        matrix(
          c("iter"=i,
            results_performance[2,i],
            results_performance[3,i],
            results_performance[4,i],
            results_performance[5,i],
            results_performance[6,i],
            results_performance[7,i],
            results_performance[8,i],
            results_performance[9,i],
            results_performance[10,i],
            results_performance[11,i],
            prediction$score(cr_optim),
            #unlist(x=tune_results$result_y),
            unlist(x=as.character(tune_results$result_learner_param_vals)))
        )
      )
      rownames(hyperparameter_best)<-c("iter",
                                       rownames(results_performance)[2:11],
                                       names(prediction$score(cr_optim)),
                                       names(tune_results$result_learner_param_vals))
      hyperparameter_all<-rbind(
        hyperparameter_all,
        cbind(
          rep(i,
              times=nrow(tune_results$archive$data)),
          tune_results$archive$data)
        )
    }

        if(verbose==TRUE){
      print(paste(date(),"Performance estimation:",i,"of",n_performance_estimation,
                  cr_optim$id,round(prediction$score(cr_optim),digits = 3)))
    }
  }

  if(tune==TRUE){
    hyperparameter_best<-t(as.matrix(hyperparameter_best))
    hyperparameter_best<-as.data.frame(hyperparameter_best)
    hyperparameter_all<-as.data.frame(hyperparameter_all)
  }


  #Training mit allen Daten----
  #task_filter<-mlr3::TaskClassif$new(id="filter_estimation", backend = data_analysis, target="target_data")
  #task_filter$col_roles$stratum="target_data"

  #filter$calculate(task_filter)
  #Liste_Features<-as.vector(rownames(as.matrix(filter$scores[1:floor(filter_ratio*max_features)])))

  finale_learner<-learner$clone(deep = TRUE)

  if(filter_ratio<1){
    final_features<-colSums(results_filtervalues)
    final_features<-final_features[names(final_features)[order(final_features,decreasing = TRUE)]]
    final_features<-names(final_features)[1:floor(filter_ratio*max_features)]
  }
  else{
    final_features<-colnames(data_analysis)[1:(ncol(data_analysis)-1)]
  }

  task_training_all<-mlr3::TaskClassif$new(id="training_complete",
                                           backend = data_analysis[c(final_features,"target_data")],
                                           target="target_data")
  task_training_all$col_roles$stratum="target_data"

  #Wenn Autotuning, Ermittlung der besten Werte fuer das finale Training
  hyperparameter_final<-NULL
  if(tune==TRUE){
    if(verbose==TRUE){
      print(paste(date(),"Estimating best values of hyperparameters"))
    }
    for(i in 13:ncol(hyperparameter_best)){
      tmp_name=colnames(hyperparameter_best)[i]
      if(is.logical(hyperparameter_best[1,i])){
        tmp_count<-table(hyperparameter_best[i])
        index_max<-match(x=max(tmp_count),table=tmp_count)
        tmp_value<-names(tmp_count)[index_max]
        hyperparameter_final[tmp_name]<-list(tmp_value)

      } else if(varhandle::check.numeric(hyperparameter_best[1,i])==FALSE){
        tmp_count<-table(hyperparameter_best[i])
        if(length(tmp_count)>0){
          index_max<-match(x=max(tmp_count),table=tmp_count)
          tmp_value<-names(tmp_count)[index_max]
          if(tmp_value=="FALSE"){
            tmp_value<-FALSE
          } else if(tmp_value=="TRUE") {
            tmp_value<-TRUE
          } else {
            tmp_value<-tmp_value
          }
          hyperparameter_final[tmp_name]<-list(tmp_value)
        }
      } else {
        if(stats::sd(as.numeric(hyperparameter_best[,i]))>0){
          hyperparameter_final[tmp_name]<-list(stats::quantile(x=as.numeric(hyperparameter_best[,i]),
                                              probs=0.5))
          if(learner$param_set$params[[tmp_name]]$class=="ParamInt"){
            hyperparameter_final[tmp_name]<-list(as.integer(round(as.numeric(hyperparameter_final[tmp_name]),digits = 0)))
          } else {
            hyperparameter_final[tmp_name]<-list(as.numeric(hyperparameter_final[tmp_name]))
          }
        } else {
          hyperparameter_final[tmp_name]<-list(as.numeric(hyperparameter_best[1,i]))
        }
      }
      #print(i)
      #print(hyperparameter_final)
    }
    #names(hyperparameter_final)=colnames(hyperparameter_best)[5:ncol(hyperparameter_best)]
    #learner<-learner$base_learner()
    if(verbose==TRUE){
      print(paste(date(),"Setting best values of hyperparameters"))
    }
    #learner$param_set$add(learner$instance_args$learner$param_set)
    finale_learner$param_set$values = mlr3misc::insert_named(finale_learner$param_set$values,
                                                             as.list(hyperparameter_final))
    #print(finale_learner$param_set$values)
    #print(as.list(hyperparameter_final))
    #finale_learner$param_set$params = as.list(hyperparameter_final)
  }

  if(verbose==TRUE){
    print(paste(date(),"Final training of the learner"))
  }

  finale_learner$train(task_training_all)

  datamatrix_min_max<-datamatrix_non_normalized[c(final_features)]

  normalization_matrix<-matrix(data=NA,nrow=4,ncol =n_features)
  for (i in 1:n_features)
  {
    normalization_matrix[1,i]<-i
    normalization_matrix[2,i]<-final_features[i]
    #Min-Werte for Normalisierung
    normalization_matrix[3,i]<-min(datamatrix_min_max[,i])
    #Max-Werte fuer Normalisierung
    normalization_matrix[4,i]<-max(datamatrix_min_max[,i])
  }

  #Creating object of class iotarelr_iota2
  mean_aem<-0
  mean_categorical_sizes<-0
  for(i in 1:length(iota2_list)){
    mean_aem<-mean_aem+iota2_list[[paste0("iter",i)]]$categorical_level$raw_estimates$assignment_error_matrix
  }
  mean_aem<-mean_aem/n_performance_estimation
  mean_categorical_sizes<-iota2_list[[paste0("iter",1)]]$information$est_true_cat_sizes
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
  Esimtates_Information["n_cunits"] <- list(length(prediction$truth))
  Esimtates_Information["call"] <- list("latools::fct_train")
  Esimtates_Information["random_starts"] <- list(NA)
  Esimtates_Information["estimates_list"] <- list(NA)

  iota2_object <- NULL
  iota2_object["categorical_level"] <- list(tmp_iota_2_measures$categorical_level)
  iota2_object["scale_level"] <- list(tmp_iota_2_measures$scale_level)
  iota2_object["information"] <- list(Esimtates_Information)
  class(iota2_object) <- "iotarelr_iota2"

  reliability<-list(
    "Iota2_Object"=iota2_object,
    "iota_index"=mean(results_performance[2,]),
    "static_iota_index"=mean(results_performance[3,]),
    "dynamic_iota_index"=mean(results_performance[4,]),
    "kripp_alpha_nominal"=mean(results_performance[5,]),
    "kripp_alpha_ordinal"=mean(results_performance[6,]),
    "kendall_tau"=mean(results_performance[7,]),
    "cohen_kappa"=mean(results_performance[8,]),
    "fleiss_kappa"=mean(results_performance[9,]),
    "light_kappa"=mean(results_performance[10,]),
    "percentage_agreement"=mean(results_performance[11,]),

    "n_sample"=nrow(data_analysis),
    "n_p_test"=n_p_test,
    "n_p_train"=n_p_train,
    "n_iteration"=n_iteration
  )

  #Summary of Filter Values
  filtervalues<-rbind(
    colMeans(results_filtervalues)-stats::qnorm(0.975)*apply(X=results_filtervalues,
                                                             MARGIN = 2,
                                                             FUN = stats::sd)/sqrt(nrow(results_filtervalues)),
    colMeans(results_filtervalues),
    colMeans(results_filtervalues)+stats::qnorm(0.975)*apply(X=results_filtervalues,
                                                             MARGIN = 2,
                                                             FUN = stats::sd)/sqrt(nrow(results_filtervalues))
  )
  filtervalues<-filtervalues[,order(filtervalues[2,],decreasing = TRUE)]
  filtervalues<-rbind(filtervalues,
                      seq(from=1,to=ncol(filtervalues)))
  rownames(filtervalues)<-c("LCI95","Mean","UCI95","Rank")
  filtervalues<-as.data.frame(t(filtervalues))

  filter_summary<-list(
    "Filter"=filter$id,
    "Inclusion_Percentage"=filter_ratio,
    "Scores"=filtervalues
  )

  #Summary of category related information
  category_summary<-NULL
  category_summary["label"]<-list(category_label)
  category_summary["category_name"]<-list(category_name)
  category_summary["categories"]<-list(original_cat_labels)

  #Summarizing the results regarding hyperparameter tuning
  res_hyber_params<-NULL
  if(tune==TRUE){
    res_hyber_params["all"]<-list(hyperparameter_all)
    res_hyber_params["best"]<-list(hyperparameter_best)
    res_hyber_params["final"]<-list(hyperparameter_final)
  }

  #Summary of transformation related information
  res_transformation<-NULL
  res_transformation["normalization_input"]=list(normalize_input)
  res_transformation["normalization_output"]=list(normalize_output)
  res_transformation["target_max"]=list(target_max)
  res_transformation["target_min"]=list(target_min)
  res_transformation["n_input_variables"]=list(n_features)
  res_transformation["normalization_matrix"]=list(normalization_matrix)

  #Summary of dimension_reduction
  res_dim_reduction<-NULL
  if(use_dim_reduction==TRUE){
    res_dim_reduction["method"]<-list(dim_red_method)
    res_dim_reduction["n_dim"]<-list(dim_red_n)
    res_dim_reduction["applied"]<-list(use_dim_reduction)
    res_dim_reduction["model"]<-list(dim_red_model)
  }

  #Summarizing the results
  trained_learner<-list(
    "category_summary"=category_summary,
    "learner"=finale_learner,
    "transformation"=res_transformation,
    "dim_reduction"=res_dim_reduction,
    "reliability"=reliability,
    "filter_summary"=filter_summary,
    "hyperparameter_summary"=res_hyber_params,
    "text_model"=text_embeddings$get_model_info(),
    "date"=date()
  )

  if(verbose==TRUE){
    print(paste(date(),"Done"))
  }

  return(trained_learner)
}


