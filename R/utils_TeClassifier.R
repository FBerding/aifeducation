#------------------------------------------------------------------------------
#'Calculate reliability measures based on content analysis
#'
#'This function calculates different reliability measures which are based on the
#'empirical research method of content analysis.
#'
#'@param true_values \code{factor} containing the true labels/categories.
#'@param predicted_values \code{factor} containing the predicted labels/categories.
#'@param return_names_only \code{bool} If \code{TRUE} returns only the names
#'of the resulting vector. Use \code{FALSE} to request computation of the values.
#'@return If \code{return_names_only=FALSE} returns a \code{vector} with the following reliability measures:
#'#'\itemize{
#'\item{\strong{iota_index: }Iota Index from the Iota Reliability Concept Version 2.}
#'\item{\strong{min_iota2: }Minimal Iota from Iota Reliability Concept Version 2.}
#'\item{\strong{avg_iota2: }Average Iota from Iota Reliability Concept Version 2.}
#'\item{\strong{max_iota2: }Maximum Iota from Iota Reliability Concept Version 2.}
#'\item{\strong{min_alpha: }Minmal Alpha Reliability from Iota Reliability Concept Version 2.}
#'\item{\strong{avg_alpha: }Average Alpha Reliability from Iota Reliability Concept Version 2.}
#'\item{\strong{max_alpha: }Maximum Alpha Reliability from Iota Reliability Concept Version 2.}
#'\item{\strong{static_iota_index: }Static Iota Index from Iota Reliability Concept Version 2.}
#'\item{\strong{dynamic_iota_index: }Dynamic Iota Index Iota Reliability Concept Version 2.}
#'\item{\strong{kalpha_nominal: }Krippendorff's Alpha for nominal variables.}
#'\item{\strong{kalpha_ordinal: }Krippendorff's Alpha for ordinal variables.}
#'\item{\strong{kendall: }Kendall's coefficient of concordance W.}
#'\item{\strong{kappa2_unweighted: }Cohen's Kappa unweighted.}
#'\item{\strong{kappa2_equal_weighted: }Weighted Cohen's Kappa with equal weights.}
#'\item{\strong{kappa2_squared_weighted: }Weighted Cohen's Kappa with squared weights.}
#'\item{\strong{kappa_fleiss: }Fleiss' Kappa for multiple raters without exact estimation.}
#'\item{\strong{percentage_agreement: }Percentage Agreement.}
#'\item{\strong{balanced_accuracy: }Average accuracy within each class.}
#'\item{\strong{gwet_ac: }Gwet's AC1/AC2 agreement coefficient.}
#'}
#'
#'@return If \code{return_names_only=TRUE} returns only the names of the vector elements.
#'
#'@family Auxiliary Functions
#'
#'@export
get_coder_metrics<-function(true_values=NULL,
                            predicted_values=NULL,
                            return_names_only=FALSE){

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
                 "kappa2_unweighted",
                 "kappa2_equal_weighted",
                 "kappa2_squared_weighted",
                 "kappa_fleiss",
                 "percentage_agreement",
                 "balanced_accuracy",
                 "gwet_ac",
                 "avg_precision",
                 "avg_recall",
                 "avg_f1")
  metric_values=vector(length = length(metric_names))
  names(metric_values)=metric_names

  if(return_names_only==TRUE){
    return(metric_names)
  } else {

    val_res=iotarelr::check_new_rater(true_values = true_values,
                                      assigned_values = predicted_values,
                                      free_aem = FALSE)
    val_res_free=iotarelr::check_new_rater(true_values = true_values,
                                           assigned_values = predicted_values,
                                           free_aem = TRUE)

    metric_values["iota_index"]=val_res$scale_level$iota_index

    metric_values["min_iota2"]=min(val_res_free$categorical_level$raw_estimates$iota)
    metric_values["avg_iota2"]=mean(val_res_free$categorical_level$raw_estimates$iota)
    metric_values["max_iota2"]=max(val_res_free$categorical_level$raw_estimates$iota)

    metric_values["min_alpha"]=min(val_res_free$categorical_level$raw_estimates$alpha_reliability)
    metric_values["avg_alpha"]=mean(val_res_free$categorical_level$raw_estimates$alpha_reliability)
    metric_values["max_alpha"]=max(val_res_free$categorical_level$raw_estimates$alpha_reliability)

    metric_values["static_iota_index"]=val_res$scale_level$iota_index_d4
    metric_values["dynamic_iota_index"]=val_res$scale_level$iota_index_dyn2

    metric_values["kalpha_nominal"]=irr::kripp.alpha(x=rbind(true_values,predicted_values),
                                                     method = "nominal")$value
    metric_values["kalpha_ordinal"]=irr::kripp.alpha(x=rbind(true_values,predicted_values),
                                                     method = "ordinal")$value

    metric_values["kendall"]=irr::kendall(ratings=cbind(true_values,predicted_values),
                                          correct=TRUE)$value

    if(length(table(predicted_values))>1){
      metric_values["kappa2_unweighted"]=irr::kappa2(ratings=cbind(true_values,predicted_values),
                                                     weight = "unweighted",
                                                     sort.levels = FALSE)$value
      metric_values["kappa2_equal_weighted"]=irr::kappa2(ratings=cbind(true_values,predicted_values),
                                                         weight = "equal",
                                                         sort.levels = FALSE)$value
      metric_values["kappa2_squared_weighted"]=irr::kappa2(ratings=cbind(true_values,predicted_values),
                                                           weight = "squared",
                                                           sort.levels = FALSE)$value
    } else {
      metric_values["kappa2_unweighted"]=NA
      metric_values["kappa2_equal_weighted"]=NA
      metric_values["kappa2_squared_weighted"]=NA
    }


    metric_values["kappa_fleiss"]=irr::kappam.fleiss(ratings=cbind(true_values,predicted_values),
                                                     exact = FALSE,
                                                     detail = FALSE)$value

    metric_values["percentage_agreement"]=irr::agree(ratings=cbind(true_values,predicted_values),
                                                     tolerance = 0)$value/100

    metric_values["balanced_accuracy"]=sum(diag(val_res_free$categorical_level$raw_estimates$assignment_error_matrix))/
      ncol(val_res_free$categorical_level$raw_estimates$assignment_error_matrix)

    metric_values["gwet_ac"]=irrCAC::gwet.ac1.raw(ratings=cbind(true_values,predicted_values))$est$coeff.val

    standard_measures<-calc_standard_classification_measures(true_values=true_values,
                                                             predicted_values=predicted_values)
    metric_values["avg_precision"]<-mean(standard_measures[,"precision"])
    metric_values["avg_recall"]<-mean(standard_measures[,"recall"])
    metric_values["avg_f1"]<-mean(standard_measures[,"f1"])

    return(metric_values)
  }
}

#------------------------------------------------------------------------------
#'Create an iota2 object
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
#'@family Auxiliary Functions
#'@keywords internal
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

#'Calculate standard classification measures
#'
#'Function for calculating recall, precision, and f1.
#'
#'@param true_values \code{factor} containing the true labels/categories.
#'@param predicted_values \code{factor} containing the predicted labels/categories.
#'@return Returns a matrix which contains the cases categories in the rows and
#'the measures (precision, recall, f1) in the columns.
#'
#'@family Auxiliary Functions
#'
#'@export
calc_standard_classification_measures<-function(true_values,predicted_values){
  categories=levels(true_values)
  results<-matrix(nrow = length(categories),
                  ncol = 3)
  colnames(results)=c("precision","recall","f1")
  rownames(results)<-categories

  for(i in 1:length(categories)){
    bin_true_values=(true_values==categories[i])
    bin_true_values=factor(as.character(bin_true_values),levels = c("TRUE","FALSE"))

    bin_pred_values=(predicted_values==categories[i])
    bin_pred_values=factor(as.character(bin_pred_values),levels = c("TRUE","FALSE"))

    conf_matrix=table(bin_true_values,bin_pred_values)
    conf_matrix=conf_matrix[c("TRUE","FALSE"),c("TRUE","FALSE")]

    TP_FN=(sum(conf_matrix[1,]))
    if(TP_FN==0){
      recall=NA
    } else {
      recall=conf_matrix[1,1]/TP_FN
    }

    TP_FP=sum(conf_matrix[,1])
    if(TP_FP==0){
      precision=NA
    } else {
      precision=conf_matrix[1,1]/TP_FP
    }

    if(is.na(recall)|is.na(precision)){
      f1=NA
    } else {
      f1=2*precision*recall/(precision+recall)
    }

    results[categories[i],1]=precision
    results[categories[i],2]=recall
    results[categories[i],3]=f1
  }

  return(results)
}
