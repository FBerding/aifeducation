#===================DataSets===================================================
DataSetsIndex=list()

#===================Tokenizer==================================================
TokenizerIndex=list()

#===================BaseModels=================================================
BaseModelsIndex=list()

#===================Classifiers================================================

#' @title Class names of all classifier models based on text embeddings
#' @description `vector` containing all class names as a string.
#' @family Parameter Dictionary
#' @keywords internal
#' @noRd
TEClassifiers_class_names <- c()

#' @title Get names of classifiers
#' @description Function returns the names of all classifiers which
#' are child classes of a specific super class.
#' @param super_class `string` Name of the super class the classifiers should
#' be child of. To request the names of all classifiers set this argument to `NULL`.
#' @return Returns a vector containing the names of the classifiers.
#' @family Parameter Dictionary
#' @export
get_TEClassifiers_class_names <- function(super_class = NULL) {
  if (is.null(super_class)) {
    return(TEClassifiers_class_names)
  } else {
    class_names <- NULL
    for (class in TEClassifiers_class_names) {
      object <- create_object(class)
      if (super_class %in% class(object)) {
        class_names <- append(
          x = class_names,
          values = class
        )
      }
    }
    return(class_names)
  }
}

#===================TextEmbedding===============================================
TextEmbeddingObjectsIndex=list()


#' @title Names of all deprecated objects
#'
#' @description `vector` containing all class names as a string.
#'
#' @family Parameter Dictionary
#' @keywords internal
#' @noRd
DeprecatedObjects <- c("TEClassifierProtoNet", "TEClassifierRegular")


#' @title Get names of deprecated objects
#' @description Function returns the names of all objects that are deprecated.
#' @return Returns a `vector` containing the names.
#' @family Parameter Dictionary
#' @export
get_depr_obj_names <- function() {
  return(DeprecatedObjects)
}


#' @title Create object
#'
#' @description  Support function for creating objects.
#' @param class `string` Name of the class to be created. The following
#' strings are possible:
#' `unique(DataSetsIndex,names(DataSetsIndex))`
#' `unique(TokenizerIndex,names(TokenizerIndex))`
#' `unique(BaseModelsIndex,names(BaseModelsIndex))`
#' `unique(TextEmbeddingObjectsIndex,names(TextEmbeddingObjectsIndex))`
#' `unique(TEClassifiers_class_names,names(TEClassifiers_class_names))`
#' @return Returns an object of the requested class.
#' @family Utils Developers
#' @export
create_object=function(class){

  #Create list of all objects on user level
  object_list=c(
    DataSetsIndex,
    TokenizerIndex,
    BaseModelsIndex,
    TextEmbeddingObjectsIndex,
    TEClassifiers_class_names
  )

  if(class=="TEClassifierRegular"){
    return(suppressMessages(TEClassifierRegular$new()))
  } else if(class=="TEClassifierProtoNet"){
    return(suppressMessages(TEClassifierProtoNet$new()))
  } else if(class%in%object_list){
    object=eval(str2expression(class))
    return(object$new())
    } else if(class%in%names(object_list)){
      object=eval(str2expression(object_list[[class]]))
      return(object$new())
  } else {
    stop(paste0("Object ",class," is not implemented in this function."))
  }
}



