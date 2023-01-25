#' Training of a Simple Keras Neural Net
#'
#'INSERT Description
#'
#'@import keras
#'@importFrom methods isClass
#'@export
keras_seq_net<-function(
    input,
    target,
    n_hidden,
    n_hidden_size,
    optimizer=keras::optimizer_rmsprop(),
    act_fct="relu",
    act_fct_last="softmax",
    err_fct="categorical_crossentropy",
    epochs = 30L,
    batch_size = 5L,
    rel_tolerance=1e-8,
    validation_split=0.2,
    monitor="val_loss",
    view_metrics = FALSE){

  #Validate Input and target Data
  if(methods::isClass(where=input,"data.frame")==FALSE){
    stop("input must be a data frame")
  }
  if(methods::isClass(where=target,"factor")==FALSE){
    stop("target must be a factor")
  }

  #Save names and order of input variables and target levels
  variable_name_order<-colnames(input)
  target_levels_order<-levels(target)

  #Transforming target for the use in keras. That is switching characters to
  #numeric
  target_transformed<-as.numeric(target)-1

  #Defining basic keras model
  model<-keras::keras_model_sequential()

  for(i in 1:n_hidden){
    model %>% keras::layer_dense(units = as.integer(n_hidden_size),
                                 activation = act_fct)
  }
  model %>% keras::layer_dense(units = length(levels(target)),
                               activation = act_fct_last)
  model %>% keras::compile(
    loss = err_fct,
    optimizer = optimizer,
    metric="accuracy"
  )

  data<-cbind(input,target_transformed)
  data<-na.omit(data)
  data<-as.data.frame(data)

  train_results<- model %>% keras::fit(
    as.matrix(data[,1:(ncol(data)-1)]),
    keras::to_categorical(data[,ncol(data)],length(levels(target))),
    epochs = epochs,
    batch_size = batch_size,
    callback = keras::callback_early_stopping(monitor = monitor,
                                              min_delta = rel_tolerance,
                                              patience = 8,
                                              restore_best_weights = TRUE),
    view_metrics=view_metrics,
    validation_split=validation_split
  )

  final_model<-NULL
  final_model["keras_model"]<-list(model)
  final_model["input_variables"]<-list(variable_name_order)
  final_model["target_levels"]<-list(target_levels_order)
  final_model["history"]<-list(train_results)

  return(final_model)

}

#' Prediction for a Simple Keras Neural Net
#'
#'INSERT Description
#'
#'@import keras
#'@importFrom methods isClass
#'@importFrom stats predict
#'@export
keras_seq_net_predict<-function(model,newdata){

  #Checking input data
  if(methods::isClass(where=newdata,"data.frame")==FALSE){
    stop("newdata mus be a data frame")
  }
  #Ensuring the correct order of the variables for prediction
  newdata<-newdata[model$input_variables]

  #Predicting target variable
  predictions<-model$keras_model %>% stats::predict((as.matrix(newdata))) %>% keras::k_argmax()

  #Transforming predictions to target levels
  predictions<-as.character(as.vector(predictions))
  for(i in 0:(length(model$target_levels)-1)){
    predictions<-replace(x=predictions,
                        predictions==as.character(i),
                        values=model$target_levels[i+1])
  }

  return(predictions)
}

#' Training of a Simple Keras Recursive Neural Net
#'
#'INSERT Description
#'
#'@import keras
#'@importFrom methods isClass
#'@export
#keras_net_gru------------------------------------------------------------------
keras_net_gru<-function(
    input,
    times,
    features,
    target,
    n_hidden,
    n_hidden_size,
    n_gru,
    n_gru_size,
    dropout=0,
    recurrent_dropout=0,
    optimizer=keras::optimizer_rmsprop(),
    act_fct="relu",
    act_fct_last="softmax",
    err_fct="categorical_crossentropy",
    epochs = 30L,
    batch_size = 5L,
    rel_tolerance=1e-8,
    validation_split=0.2,
    monitor="val_loss",
    view_metrics = FALSE){

  #Validate Input and target Data
  if(methods::isClass(where=input,"data.frame")==FALSE){
    stop("input must be a data frame")
  }
  if(methods::isClass(where=target,"factor")==FALSE){
    stop("target must be a factor")
  }

  #Save names and order of input variables and target levels
  variable_name_order<-colnames(input)
  target_levels_order<-levels(target)

  #Transforming target for the use in keras.
  #That is switching characters to numeric
  target_transformed<-as.numeric(target)-1

  #Defining basic keras model
  model<-keras::keras_model_sequential()

  #Adding gru layer
  for(i in 1:n_gru){
    if(i<n_gru){
      model %>% keras::layer_gru(units=n_gru_size,
                                 input_shape=list(times,features),
                                 return_sequences = TRUE,
                                 dropout = dropout,
                                 recurrent_dropout = recurrent_dropout)
    } else {
      model %>% keras::layer_gru(units=n_gru_size,
                                 input_shape=list(times,features),
                                 return_sequences = FALSE,
                                 dropout = dropout,
                                 recurrent_dropout = recurrent_dropout)
    }
  }

  #Adding standard layer
  for(i in 1:n_hidden){
    model %>% keras::layer_dense(units = as.integer(n_hidden_size),
                                 activation = act_fct)
  }

  #Adding final Layer
  model %>% keras::layer_dense(units = length(levels(target)),
                               activation = act_fct_last)

  model %>% keras::compile(
    loss = err_fct,
    optimizer = optimizer,
    metric="accuracy"
  )

  data<-cbind(input,target_transformed)
  data<-na.omit(data)
  data<-as.data.frame(data)

  #Convert Input data to sequential data
  input_sequential<-matrix_to_array(
    matrix = as.matrix(data[,1:(ncol(data)-1)]),
    times = times,
    features = features)

  train_results<- model %>% keras::fit(
    input_sequential,
    keras::to_categorical(data[,ncol(data)],length(levels(target))),
    epochs = epochs,
    batch_size = batch_size,
    callback = keras::callback_early_stopping(monitor = monitor,
                                              min_delta = rel_tolerance,
                                              patience = 8,
                                              restore_best_weights = TRUE,
                                              mode = "auto"),
    view_metrics=view_metrics,
    validation_split=validation_split)

  final_model<-NULL
  final_model["keras_model"]<-list(model)
  final_model["input_variables"]<-list(variable_name_order)
  final_model["target_levels"]<-list(target_levels_order)
  final_model["history"]<-list(train_results)
  final_model["times"]<-list(times)
  final_model["features"]<-list(features)

  return(final_model)

}

#' Prediction for a Simple Keras Recursive Neural Net
#'
#'INSERT Description
#'
#'@import keras
#'@importFrom methods isClass
#'@importFrom stats predict
#'@export
keras_net_gru_predict<-function(model,newdata,times,features){

  #Checking input data
  if(methods::isClass(where=newdata,"data.frame")==FALSE){
    stop("newdata mus be a data frame")
  }

  #Ensuring the correct order of the variables for prediction
  newdata<-newdata[model$input_variables]
  newdata<-matrix_to_array(
    matrix = as.matrix(newdata),
    times = model$times,
    features = model$features)

  #Predicting target variable
  predictions<-model$keras_model %>% stats::predict(newdata) %>% keras::k_argmax()

  #Transforming predictions to target levels
  predictions<-as.character(as.vector(predictions))
  for(i in 0:(length(model$target_levels)-1)){
    predictions<-replace(x=predictions,
                         predictions==as.character(i),
                         values=model$target_levels[i+1])
  }

  return(predictions)
}
