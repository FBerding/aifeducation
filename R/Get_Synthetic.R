#-----------------------------------------------------------------------------
#'Create synthetic cases for balancing training data
#'
#'This function creates synthetic cases for balancing the training with an
#'object of the class \link{TextEmbeddingClassifierNeuralNet}.
#'
#'@param embedding Named \code{data.frame} containing the text embeddings.
#'In most cases, this object is taken from \link{EmbeddedText}$embeddings.
#'@param target Named \code{factor} containing the labels of the corresponding embeddings.
#'@param times \code{int} for the number of sequences/times.
#'@param features \code{int} for the number of features within each sequence.
#'@param method \code{vector} containing strings of the requested methods for generating new cases.
#'Currently "smote","dbsmote", and "adas" from the package smotefamily are available.
#'@param max_k \code{int} The maximum number of nearest neighbors during sampling process.
#'@return \code{list} with the following components.
#'\itemize{
#'\item{\code{syntetic_embeddings: }Named \code{data.frame} containing the text embeddings of
#'the synthetic cases.}
#'
#'\item{\code{syntetic_targets }Named \code{factor} containing the labels of the corresponding
#'synthetic cases.}
#'
#'\item{\code{n_syntetic_units }\code{table} showing the number of synthetic cases for every
#'label/category.}
#'}
#'
#'@family Auxiliary Functions
#'
#'@export
#'@import foreach
#'@import doParallel
get_synthetic_cases_from_matrix<-function(matrix_form,
                                          times,
                                          features,
                                          target,
                                          sequence_length,
                                          method=c("smote"),
                                          min_k=1,
                                          max_k=6){


  #get possible seq lengthes in order to group the cases by sequence length
  seq_length_categories=as.numeric(names(table(sequence_length)))

  index=1
  input=NULL

  #Create tasks for every group of sequence lengths
  for(current_seq_length in seq_length_categories){
    condition=(sequence_length==current_seq_length)
    idx=which(condition)
    cat_freq=table(target[idx])
    categories=names(cat_freq)
    max_freq=max(cat_freq)

    for(cat in categories){

      #Check k and adjust if necessary
      n_neighbors=cat_freq[cat]-1

      if(n_neighbors<=max_k){
        max_k_final=n_neighbors
        if(min_k>max_k_final){
          min_k_final=max_k_final
        } else {
          min_k_final=min_k
        }
      } else {
        max_k_final=max_k
        min_k_final=min_k
      }

      #Check k and adjust according to the difference to the major category
      required_cases=max_freq-cat_freq[cat]
      n_k=length(min_k_final:max_k_final)
      if(required_cases<n_k){
        difference=n_k-required_cases
        min_k_final=min_k_final+difference
      }

      if(cat_freq[cat]<max_freq & min_k>0 & cat_freq[cat]>3){
        for(m in 1:length(method)){
          if(method[m]!="dbsmote"){
            for (k in min_k_final:max_k_final){
              input[[index]]<-list(cat=cat,
                                   required_cases=required_cases,
                                   k=k,
                                   method=method[m],
                                   selected_cases=idx,
                                   chunks=current_seq_length,
                                   max_freq=max_freq,
                                   k_s=length(min_k_final:max_k_final),
                                   max_k=max_k_final)
              index=index+1
            }
          } else {
            input[[index]]<-list(cat=cat,
                                 required_cases=required_cases,
                                 k=0,
                                 method=method[m],
                                 selected_cases=idx,
                                 chunks=current_seq_length,
                                 max_freq=max_freq,
                                 k_s=1,
                                 max_k=0)
            index=index+1
          }
        }
      }
    }
  }


  result_list<-foreach::foreach(index=1:length(input),.export="create_synthetic_units_from_matrix")%dopar%{
  #index=1
    create_synthetic_units_from_matrix(
      matrix_form=matrix_form[input[[index]]$selected_cases,
                              c(1:(input[[index]]$chunks*features))],
      target=target[input[[index]]$selected_cases],
      required_cases=input[[index]]$required_cases,
      k=input[[index]]$k,
      method=input[[index]]$method,
      cat=input[[index]]$cat,
      max_freq=input[[index]]$max_freq,
      k_s=input[[index]]$k_s,
      max_k=input[[index]]$max_k)
  }

  #get number of synthetic cases
  n_syn_cases=0
  for(i in 1:length(result_list)){
    if(is.null(result_list[[i]]$syntetic_embeddings)==FALSE){
      n_syn_cases=n_syn_cases+nrow(result_list[[i]]$syntetic_embeddings)
    }
  }

  syntetic_embeddings<-matrix(data = 0,
                              nrow = n_syn_cases,
                              ncol = times*features)
  colnames(syntetic_embeddings)=colnames(matrix_form)
  syntetic_embeddings=as.data.frame(syntetic_embeddings)
  syntetic_targets=NULL

  n_row=0
  names_vector=NULL
  for(i in 1:length(result_list)){
    if(is.null(result_list[[i]]$syntetic_embeddings)==FALSE){
      syntetic_embeddings[(n_row+1):(n_row+nrow(result_list[[i]]$syntetic_embeddings)),
                          c(1:ncol(result_list[[i]]$syntetic_embeddings))]<-result_list[[i]]$syntetic_embeddings[,c(1:ncol(result_list[[i]]$syntetic_embeddings))]
      syntetic_targets=append(syntetic_targets,values = result_list[[i]]$syntetic_targets)
      n_row=n_row+nrow(result_list[[i]]$syntetic_embeddings)
      names_vector=append(x=names_vector,
                          values = rownames(result_list[[i]]$syntetic_embeddings))
    }
  }

  #Transform matrix back to array
  syntetic_embeddings<-matrix_to_array_c(
    matrix=as.matrix(syntetic_embeddings),
    times = times,
    features = features)
  rownames(syntetic_embeddings)=names_vector
  #dimnames(syntetic_embeddings)[3]<-feature_names

  n_syntetic_units=table(syntetic_targets)

  results=NULL
  results["syntetic_embeddings"]=list(syntetic_embeddings)
  results["syntetic_targets"]=list(syntetic_targets)
  results["n_syntetic_units"]=list(n_syntetic_units)

  return(results)
}

#---------------------------------------------
#'Create synthetic units
#'
#'Function for creating synthetic cases in order to balance the data for
#'training with \link{TextEmbeddingClassifierNeuralNet}. This is an auxiliary
#'function for use with \link{get_synthetic_cases} to allow parallel
#'computations.
#'
#'@param embedding Named \code{data.frame} containing the text embeddings.
#'In most cases this object is taken from \link{EmbeddedText}$embeddings.
#'@param target Named \code{factor} containing the labels/categories of the corresponding cases.
#'@param k \code{int} The number of nearest neighbors during sampling process.
#'@param max_k \code{int} The maximum number of nearest neighbors during sampling process.
#'@param method \code{vector} containing strings of the requested methods for generating new cases.
#'Currently "smote","dbsmote", and "adas" from the package smotefamily are available.
#'@param cat \code{string} The category for which new cases should be created.
#'@param cat_freq Object of class \code{"table"} containing the absolute frequencies
#'of every category/label.
#'@return Returns a \code{list} which contains the text embeddings of the
#'new synthetic cases as a named \code{data.frame} and their labels as a named
#'\code{factor}.
#'
#'@family Auxiliary Functions
#'
#'@export
create_synthetic_units_from_matrix<-function(matrix_form,
                                 target,
                                 required_cases,
                                 k,
                                 method,
                                 cat,
                                 max_freq,
                                 k_s,
                                 max_k){

    #Transform to a binary problem
    tmp_target=(target==cat)

    n_minor=sum(tmp_target)

    #Calculate n of all other cases
    n_major=sum(!tmp_target)

    #Adept the number of cases for every k in the loop
    min_k=max_k-k_s+1

    if(k<(min_k+max_k)/2){
      requested_number_cases=max(1,floor(required_cases/k_s))
    } else {
      requested_number_cases=ceiling(required_cases/k_s)
    }

    dup_size=n_major/n_minor

    syn_data=NULL
    if(method=="smote"){
      syn_data=smotefamily::SMOTE(X=as.data.frame(matrix_form),
                                  target = tmp_target,
                                  K=k,
                                  dup_size = dup_size)
    } else if(method=="adas"){
      syn_data=smotefamily::ADAS(X=as.data.frame(matrix_form),
                                 target = tmp_target,
                                 K=k)
    } else if(method=="dbsmote"){
      syn_data=smotefamily::DBSMOTE(X=as.data.frame(matrix_form),
                                    target = tmp_target,
                                    dupSize = dup_size,
                                    MinPts = NULL,
                                    eps = NULL)
    }

    if(is.null(syn_data)==FALSE|nrow(syn_data$syn_data)>0){
      n_cols_embedding=ncol(matrix_form)
      tmp_data=syn_data$syn_data[1:requested_number_cases,-ncol(syn_data$syn_data)]
      rownames(tmp_data)<-paste0(method,"_",cat,"_",k,"_",n_cols_embedding,"_",
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

  return(results)
}
