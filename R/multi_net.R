#' Train Multi-Layer Neural Net
#'
#' Function for training a multi-layer neural net
#'
#' @export
multi_net<-function(input,
                    output,
                    n_hidden=3,
                    n_hidden_size=3,
                    learning_rate = 0.001,
                    act_fct="relu",
                    act_fct_last="softmax",
                    err_msr="iota2",
                    freq_recalc_iota2=1,
                    max_iter = 1000,
                    cr_rel_change = 1e-6,
                    cr_abs_error=1e-4,
                    validation_split=0.2,
                    split_method="strata",
                    monitor="val_loss",
                    patience=1,
                    return_best=TRUE,
                    trace=FALSE){
  #Evaluating input and output data----------------------------------------------
  if(is.factor(output)==FALSE){
    stop("Object for output mus be a factor.")
  }
  if(is.data.frame(input)==FALSE){
    stop("Object for input must be a data.frame.")
  }
  if(sum(sapply(input, is.numeric)==TRUE)==0){
    stop("Data.frame for input must be numeric.")
  }
  if(sum((abs(input)<1)==FALSE)>0){
    warning("Some values in data.frame for input are not
            in the range between greater -1 and lesser +1.")
  }

  #Creating training and test split--------------------------------------------
  names(output)<-seq(from=1,to=length(output),by=1)

  test_samples<-NULL
  if(validation_split>0){
    for(i in levels(output)){
      tmp_subset<-subset(output,output==i)
      if(split_method=="strata"){
        tmp_size=ceiling(validation_split*length(tmp_subset))
      } else if(split_method=="equal"){
        tmp_size=ceiling(validation_split*min(table(output)))
      }

      test_samples[i]<-list(names(sample(x = tmp_subset,
                                         size = tmp_size,
                                         replace = FALSE)))
    }
    test_sample<-unlist(test_samples)
    train_sample<-setdiff(names(output),test_sample)

    train_input<-input[as.numeric(train_sample),]
    train_target<-output[train_sample]

    test_input<-input[as.numeric(test_sample),]
    test_target<-output[test_sample]
  } else {
    train_input<-input
    train_target<-output

    test_input<-NULL
    test_target<-NULL
  }

  hidden=vector(length = n_hidden)
  hidden[]=n_hidden_size

  #hidden=c(20,10,8,7)

  #Training Net-----------------------------------------------------------------
  trained_wts<-multi_net_train(input = as.matrix(train_input),
                               output = as.character(train_target),
                               test_input = as.matrix(test_input),
                               test_output = as.character(test_target),
                               output_levels = levels(output),
                               hidden = hidden,
                               learningrate = learning_rate,
                               act_fct=act_fct,
                               act_fct_last=act_fct_last,
                               err_msr_last=err_msr,
                               freq_recalc_iota2=freq_recalc_iota2,
                               max_iter = max_iter,
                               cr_rel_change = cr_rel_change,
                               cr_abs_error=cr_abs_error,
                               monitor=monitor,
                               return_best=return_best,
                               patience=patience,
                               trace=trace)
  #Summarizing results----------------------------------------------------------
  resulting_model<-NULL
  resulting_model["wts"]<-list(trained_wts$wts)
  resulting_model["act_fct"]<-list(act_fct)
  resulting_model["act_fct_last"]<-list(act_fct_last)
  resulting_model["err_msr"]<-list(err_msr)
  resulting_model["cat_char"]<-list(levels(output))
  resulting_model["features_order"]<-list(colnames(train_input))
  resulting_model["history"]<-list(trained_wts$history)

  return(resulting_model)

}


#' Predict with Multi-Layer Neural Net
#'
#' Function for predicting with a multi-layer neural net
#'
#' @export
multi_net_predict<-function(model,newdata){
  expected_cat=multi_net_predict_c(wts_list=model$wts,
                                   newdata=as.matrix(newdata),
                                   act_fct=model$act_fct,
                                   act_fct_last=model$act_fct_last)

  for(i in 1:length(model$cat_char)){
    expected_cat<-replace(
      x=expected_cat,
      list=(as.character(expected_cat)==as.character(i-1)),
      values = model$cat_char[i]
    )
  }
  return(expected_cat)
}
