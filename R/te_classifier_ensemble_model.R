#'@title Text Embedding Classifier Ensemble
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
