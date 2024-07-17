#'Clean pytorch log of transformers
#'
#'Function for preparing and cleaning the log created by an object of class Trainer
#'from the python library 'transformer's
#'
#'@param log \code{data.frame} containing the log.
#'
#'@return Returns a \code{data.frame} containing epochs, loss, and val_loss.
#'
#'@family Auxiliary Functions
#'@keywords internal
#'
#'@export
clean_pytorch_log_transformers<-function(log){
  max_epochs<-max(log$epoch)

  cols=c("epoch","loss","val_loss")

  cleaned_log<-matrix(data = NA,
                      nrow = max_epochs,
                      ncol = length(cols))
  colnames(cleaned_log)=cols
  for(i in 1:max_epochs){
    cleaned_log[i,"epoch"]=i

    tmp_loss=subset(log,log$epoch==i & is.na(log$loss)==FALSE)
    tmp_loss=tmp_loss[1,"loss"]
    cleaned_log[i,"loss"]=tmp_loss

    tmp_val_loss=subset(log,log$epoch==i & is.na(log$eval_loss)==FALSE)
    tmp_val_loss=tmp_val_loss[1,"eval_loss"]
    cleaned_log[i,"val_loss"]=tmp_val_loss

  }
  return(as.data.frame(cleaned_log))
}

#'Check if NULL or NA
#'
#'Function for checking if an object is \code{NULL} or \code{NA}
#'
#'@param object An object to test.
#'
#'@return Returns \code{FALSE} if the object is not \code{NULL} and not \code{NA}.
#'Returns \code{TRUE} in all other cases.
#'
#'@family Auxiliary Functions
#'@keywords internal
#'
#'@export
is.null_or_na<-function(object){
  if(is.null(object)==FALSE){
    if(anyNA(object)==FALSE){
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(TRUE)
  }
}

#------------------------------------------------------------------------------
#'Generate ID suffix for objects
#'
#'Function for generating an ID suffix for objects of class
#'\link{TextEmbeddingModel} and \link{TextEmbeddingClassifierNeuralNet}.
#'
#'@param length \code{int} determining the length of the id suffix.
#'@return Returns a \code{string} of the requested length
#'@family Auxiliary Functions
#'@keywords internal
generate_id<-function(length=16){
  id_suffix=NULL
  sample_values=c(
    "a","A",
    "b","B",
    "c","C",
    "d","D",
    "e","E",
    "f","F",
    "g","G",
    "h","H",
    "i","I",
    "j","J",
    "k","K",
    "l","L",
    "m","M",
    "n","N",
    "o","O",
    "p","P",
    "q","Q",
    "r","R",
    "s","S",
    "t","T",
    "u","U",
    "v","V",
    "w","W",
    "x","X",
    "y","Y",
    "z","Z",
    seq(from=0,to=9,by=1)
  )


  id_suffix=sample(
    x=sample_values,
    size = length,
    replace = TRUE)
  id_suffix=paste(id_suffix,collapse = "")
  return(id_suffix)
}
