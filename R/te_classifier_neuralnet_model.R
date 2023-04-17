#'@title Text Embedding Classifier with a Neural Net
#'
#'@description Abstract class for neural nets with keras and
#'tensorflow
#'
#'@export
TextEmbeddingClassifierNeuralNet<-R6::R6Class(
  classname = "TextEmbeddingClassifierNeuralNet",
  public = list(
    #'@field name ('character()')\cr
    #'Name of the classifier.
    name=NULL,

    #'@field label ('character()')\cr
    #'Label of the classifier used as the individual title.
    label=NULL,

    #'@field date ('date()')\cr
    #'Date of the first creation of the classifier.
    date=NULL,

    #'@field text_embedding_model ('list()')\cr
    #'List for storing information about the underlying text embedding model.
    #'This information ensures that a classifier is only used with data from
    #'the correct text embedding model and ensures the correct handling of the
    #'embeddings.
    text_embedding_model=list(
      model=list(),
      times=NA,
      features=NA
    ),

    #'@field bundeled_model ('bundle object')\cr
    #'Object for storing the keras model of the neural net. Saved as bundled
    #'object with help of the package \link[bundle]{bundle} for serialization.
    bundeled_model=NULL,

    #'@field model_config ('list()')\cr
    #'List for storing information about the configuration of the model. This
    #'information is used for predicting new data.
    #'\itemize{
    #'\item{\code{model_config$n_gru: }}{Number of gru layers.}
    #'\item{\code{model_config$n_hidden: }}{Number of dense layers.}
    #'\item{\code{model_config$target_levels: }}{Levels of the target variable. Do not change this manually.}
    #'\item{\code{model_config$input_variables: }}{Order and name of the input variables. Do not change this manually.}
    #'\item{\code{model_config$init_config: }}{List storing all parameters passed to method new().}
    #'}
    model_config=list(
      n_gru=NULL,
      n_hidden=NULL,
      target_levels=NULL,
      input_variables=NULL,
      init_config=list()
    ),

    #'@field last_training ('list()')\cr
    #'List for storing the history and the results of the last training. This
    #'information will be overwritten if a new training is started.
    #'\itemize{
    #'\item{\code{last_training$learning_time: }}{Duration of the training process.}
    #'\item{\code{config$history: }}{History of the last training.}
    #'\item{\code{config$data: }}{Object of class table storing the initial frequencies of the passed data.}
    #'\item{\code{config$data_pb:l }}{Matrix storing the number of additional cases (test and training) added
    #'during balanced pseudo labeling. The rows refer to folds and final training.
    #'The columns refer to the steps during pseudo labeling.}
    #'\item{\code{config$data_bsc_test: }}{Matrix storing the number of cases for each category used for testing
    #'during the phase of balanced synthetic units. Please note that the
    #'frequencies include original and synthetic cases. In terms that the number
    #'of original and synthetic cases exceeds the limit for the majority classes
    #'the frequency represents the number of cases created by cluster analysis.}
    #'\item{\code{config$date: }}{Time when the last training finished.}
    #'\item{\code{config$config: }}{List storing which kind of estimation was requested during last training.
    #'\itemize{
    #'\item{\code{config$config$use_bsc:  }}{\code{TRUE} if  balanced synthetic cases was requested. \code{FALSE}
    #'if not.}
    #'\item{\code{config$config$use_baseline: }}{\code{TRUE} if baseline estimation was requested. \code{FALSE}
    #'if not.}
    #'\item{\code{ config$config$use_bpl: }}{\code{TRUE} if  balanced pseudo labeling cases was requested. \code{FALSE}
    #'if not.}
    #'}}
    #'}
    last_training=list(
      learning_time=NULL,
      history=NULL,
      data=NULL,
      data_pbl=NULL,
      data_bsc_train=NULL,
      data_bsc_test=NULL,
      date=NULL,
      n_samples=NULL,
      config=list(
        use_bsc=NULL,
        use_baseline=NULL,
        use_bpl=NULL
      )
    ),

    #'@field reliability ('list()')\cr
    #'List for storing central reliability measures of the last training.
    #'\itemize{
    #'\item{\code{reliability$val_metric: }}{Array containing the reliability measures for the validation data for
    #'every fold, method, and step (in case of pseudo labeling).}
    #'\item{\code{reliability$val_metric_mean: }}{Array containing the reliability measures for the validation data for
    #'every method and step (in case of pseudo labeling). The values represent
    #'the mean values for every fold.}
    #'\item{\code{reliability$raw_iota_objects: }}{List containing all iota_object generated with the package \link[iotarelr]{iotarelr}
    #'for every fold at the start and the end of the last training.
    #'\itemize{
    #'\item{\code{reliability$raw_iota_objects$iota_objects_start: }}{List of objects with class \code{iotarelr_iota2} containing the
    #'estimated iota reliability of the second generation for the baseline model
    #'for every fold.
    #'If the estimation of the baseline model is not requested the list is
    #'set to \code{NULL}.}
    #'\item{\code{reliability$raw_iota_objects$iota_objects_end: }}{List of objects with class \code{iotarelr_iota2} containing the
    #'estimated iota reliability of the second generation for the final model
    #'for every fold. Depending of the requested training method these values
    #'refer to the baseline model, a trained model on the basis of balanced
    #'synthetic cases, balanced pseudo labeling or a combination of balanced
    #'synthetic cases with pseudo labeling.}
    #'\item{\code{reliability$raw_iota_objects$iota_objects_start_free: }}{List of objects with class \code{iotarelr_iota2} containing the
    #'estimated iota reliability of the second generation for the baseline model
    #'for every fold.
    #'If the estimation of the baseline model is not requested the list is
    #'set to \code{NULL}.Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'\item{\code{reliability$raw_iota_objects$iota_objects_end_free: }}{List of objects with class \code{iotarelr_iota2} containing the
    #'estimated iota reliability of the second generation for the final model
    #'for every fold. Depending of the requested training method these values
    #'refer to the baseline model, a trained model on the basis of balanced
    #'synthetic cases, balanced pseudo labeling or a combination of balanced
    #'synthetic cases with pseudo labeling.
    #'Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'}
    #'}
    #'\item{\code{reliability$iota_object_start: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold. If the estimation of the baseline model is not requested the list is
    #'set to \code{NULL}.}
    #'\item{\code{ reliability$iota_object_start_free: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold. If the estimation of the baseline model is not requested the list is
    #'set to \code{NULL}.
    #'Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'\item{\code{reliability$iota_object_end: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold.
    #'Depending of the requested training method this object
    #'refers to the baseline model, a trained model on the basis of balanced
    #'synthetic cases, balanced pseudo labeling or a combination of balanced
    #'synthetic cases with pseudo labeling.}
    #'\item{\code{reliability$iota_object_end_free: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold.
    #'Depending of the requested training method this object
    #'refers to the baseline model, a trained model on the basis of balanced
    #'synthetic cases, balanced pseudo labeling or a combination of balanced
    #'synthetic cases with pseudo labeling.
    #'Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'}
    reliability=list(
      val_metric=NULL,
      val_metric_mean=NULL,
      raw_iota_objects=list(
        iota_objects_start=NULL,
        iota_objects_end=NULL,
        iota_objects_start_free=NULL,
        iota_objects_end_free=NULL),
      iota_object_start=NULL,
      iota_object_start_free=NULL,
      iota_object_end=NULL,
      iota_object_end_free=NULL
    ),
    #New-----------------------------------------------------------------------
    #'@description Creating a new instance of this class.
    #'@param name \code{Character} Name of the new classifier. Please refer to
    #'common name conventions. Free text can be used with parameter \code{label}.
    #'@param label \code{Character} Label for the new classifier. Here you can use
    #'free text.
    #'@param text_embeddings An object of class\code{TextEmbeddingModel}.
    #'@param targets \code{factor} containing the target values of the classifier.
    #'@param config \code{list} containing the configuration of the neural net.
    #'@param config$hidden \code{vector} containing the number of neurons for each dense layer.
    #'The length of the vector determines the number of dense layers. If you want no dense layer
    #'set this parameter to \code{NULL}.
    #'@param config$gru \code{vector} containing the number of neurons for each gru layer.
    #'The length of the vector determines the number of dense layers. If you want no dense layer
    #'set this parameter to \code{NULL}.
    #'@param config$dropout \code{double} ranging between 0 and lower 1 determining the
    #'dropout for each gru layer.
    #'@param config$recurrent_dropout \code{double} ranging between 0 and lower 1 determining the
    #'recurrent dropout for each gru layer.
    #'@param l2_regularizer \code{double} determining the l2 regularizer for every dense layer.
    #'@param optimizer Object of class \code{keras.optimizers}.
    #'@param act_fct \code{character} naming the activation function for all dense layers.
    #'@param act_fct_last \code{character} naming the activation function for the last dense layers.
    #'@param err_fct \code{character} naming the loss/error function for the neural net.
    initialize=function(name=NULL,
                        label=NULL,
                        text_embeddings=NULL,
                        targets=NULL,
                        config=list(
                          hidden=c(128),
                          gru=c(128),
                          dropout=0.2,
                          recurrent_dropout=0.4,
                          l2_regularizer=0.001,
                          optimizer="adam",
                          act_fct="gelu",
                          act_fct_last="softmax",
                          err_fct="categorical_crossentropy")
    ){
      #Setting Label and Name
      self$name=name
      self$label=label

      #Basic Information of Input and Target Data
      variable_name_order<-colnames(text_embeddings$embeddings)
      target_levels_order<-levels(targets)

      model_info=text_embeddings$get_model_info()
      times=model_info$param_chunks
      features=ncol(text_embeddings$embeddings)/times

      self$text_embedding_model["model"]=list(model_info)
      self$text_embedding_model["times"]=times
      self$text_embedding_model["features"]=features

      #Defining basic keras model
      layer_list=NULL
      #Adding Input Layer
      n_gru=length(config$gru)
      n_hidden=length(config$hidden)
      if(n_gru>0){
        model_input<-keras::layer_input(shape=list(times,features),
                                        name="input_embeddings")
      } else {
        model_input<-keras::layer_input(shape=ncol(text_embeddings$embeddings),
                                        name="input_embeddings")
      }
      layer_list[1]<-list(model_input)

      #Adding a Mask-Layer
      if(n_gru>0){
        masking_layer<-keras::layer_masking(object=model_input,
                             mask_value = 0.0,
                             name="masking_layer",
                             input_shape=c(times,features),
                             trainable=FALSE
                             )
        layer_list[length(layer_list)+1]<-list(masking_layer)
      }

      #Adding a Normalization Layer
      if(n_gru>0){
        #norm_layer<-keras::layer_layer_normalization(
        #  object=layer_list[[length(layer_list)]],
        #  name = "normalizaion_layer")
        #layer_list[length(layer_list)+1]<-list(norm_layer)
      } else {
        norm_layer<-keras::layer_batch_normalization(
          object=layer_list[[length(layer_list)]],
          name = "normalizaion_layer")
        layer_list[length(layer_list)+1]<-list(norm_layer)
      }


      #Adding gru layer
      if(n_gru>0){
        for(i in 1:n_gru){
          if(i<n_gru){
            layer_list[length(layer_list)+1]<-list(
              keras::bidirectional(object = layer_list[[length(layer_list)]],
                                   layer=keras::layer_gru(
                               units=config$gru[i],
                               input_shape=list(times,features),
                               return_sequences = TRUE,
                               dropout = config$dropout,
                               recurrent_dropout = config$recurrent_dropout,
                               name=paste0("gru",i))))
          } else {
            layer_list[length(layer_list)+1]<-list(
              keras::bidirectional(object = layer_list[[length(layer_list)]],
                                   layer=keras::layer_gru(
                               units=config$gru[i],
                               input_shape=list(times,features),
                               return_sequences = FALSE,
                               dropout = config$dropout,
                               recurrent_dropout = config$recurrent_dropout,
                               name=paste0("gru",i))))
          }
        }
      }

      #Adding standard layer
      if(n_hidden>0){
        for(i in 1:n_hidden){
          layer_list[length(layer_list)+1]<-list(
            keras::layer_dense(object= layer_list[[length(layer_list)]],
                               units = as.integer(config$hidden[i]),
                               activation = config$act_fct,
                               kernel_regularizer = keras::regularizer_l2(l=config$l2_regularizer),
                               name=paste0("dense",i)))
        }
      }

      #Adding final Layer
      layer_list[length(layer_list)+1]<-list(
        keras::layer_dense(object = layer_list[[length(layer_list)]],
                           units = length(levels(targets)),
                           activation = config$act_fct_last,
                           name="output_categories"))

      #Creating Model
      model<-keras::keras_model(
        inputs = model_input,
        outputs = layer_list[length(layer_list)],
        name = name)

      tf<-reticulate::import("tensorflow")

      if(config$optimizer=="adam"){
        model %>% keras::compile(
          loss = config$err_fct,
          optimizer=tf$keras$optimizers$legacy$Adam(),
          metric="categorical_accuracy")
      }

      self$bundeled_model=bundle::bundle(model)
      private$init_weights=model$get_weights()
      self$model_config$n_gru=length(config$gru)
      self$model_config$n_hidden=length(config$hidden)
      self$model_config$target_levels=target_levels_order
      self$model_config$input_variables=variable_name_order
      self$model_config$init_config=config
      self$date=date()
    },
    #-------------------------------------------------------------------------
    #'@description Method for training a neural net with keras and
    #'tensorflow.
    #'@param data_embeddings Object of class \code{TextEmbeddingModel}.
    #'@param data_targets \code{Factor} containing the labels for cases
    #'stored in \code{data_embeddings}. Factor must be named and has to use the
    #'same names used in \code{data_embeddings}.
    #'@param data_n_valid_samples \code{int} determining the number of cross-fold
    #'samples.
    #'@param use_baseline \code{bool} \code{TRUE} if the calculation of a baseline
    #'model is requested. This option is only relevant for \code{use_bsc=TRUE} or
    #'\code{use_pbl=TRUE}. If both are \code{FALSE} a baseline model is calculated.
    #'@param bsl_val_size \code{double} between 0 and 1 indicating the proportion of cases of each label
    #'which should be used for the validation sample during the estimation of the baseline model.
    #'The remaining cases are part of the training data.
    #'@param use_bsc \code{bool} \code{TRUE} if the estimation should integrate
    #'balanced synthetic cases. \code{FALSE} if not.
    #'@param bsc_methods \code{vector} containing the methods for generating
    #'synthetic cases via \link[smotefamily]{smotefamily}. Multiple methods can
    #'be passed. Currently \code{bsc_methods=c("adas")}, \code{bsc_methods=c("smote")}
    #'and \code{bsc_methods=c("dbsmote")} are possible.
    #'@param bsc_max_k \code{int} determining the maximal number of k which is used
    #'for creating synthetic units.
    #'@param use_bpl \code{bool} \code{TRUE} if the estimation should integrate
    #'balanced pseudo labeling. \code{FALSE} if not.
    #'@param bpl_max_steps \code{int} determining the maximal number of steps for
    #'every application of balanced pseudo labeling.
    #'@param bpl_inc_ratio \code{double} ratio between 0 and 1 indicating the
    #'proportion of new cases which should used for further training. See notes
    #'for more details.
    #'@param bpl_anchor \code{double} between 0 and 1 indicating the reference
    #'point for sorting the new cases of every label. See notes for more details.
    #'@param bpl_valid_size \code{double} ratio between 0 and 1 determining the proportion
    #'of new cases for every label which should be added to the validation sample during training.
    #'The remaining cases are added to the training sample.
    #'@param opt_model_reset \code{bool} \code{TRUE} if the model should be
    #'reseted before training.
    #'@param epochs \code{int} Number of training epochs.
    #'@param batch_size \code{int} Size of batches.
    #'@param dir_checkpoint \code{string} Path to the directory where
    #'the checkpoint during training should be saved. If the directory does not
    #'exists it is created.
    #'@param trace \code{bool} \code{TRUE} if information about the estimation
    #'phase should be printed to the console.
    #'@param keras_trace \code{int} \code{keras_trace=0} does not print any
    #'information about the training process from keras on the console.
    #'\code{keras_trace=1} print a progress bar. \code{keras_trace=2} prints
    #'one line of information for every epoch.
    #'@param view_metrics \code{bool} \code{TRUE} if metrics should be printed
    #'in the RStudie IDE.
    #'@param n_cores \code{int} Number of cores used for creating synthetic units.
    #'@details \itemize{
    #'
    #'\item{bsc_max_k: }{All values from 2 up to bsc_max_k are successively used. If
    #'the number of bsc_max_k is to high the value is reduced to a number that
    #'allows the calculating of synthetic units.}
    #'
    #'\item{bpl_inc_ratio: }{The ratio is applied to the label with the smallest number
    #'of new cases. The resulting value is used for every label to ensure the balance
    #'of all labels.}
    #'
    #'\item{bpl_anchor: }{With the help of this value the new cases are sorted. For
    #'this aim the distance from the anchor is calculated and all cases are arranged
    #'into an increasing order.
    #'}
    #'}
    train=function(data_embeddings,
                   data_targets,
                   data_n_valid_samples=10,
                   use_baseline=TRUE,
                   bsl_val_size=0.25,
                   use_bsc=TRUE,
                   bsc_methods=c("dbsmote"),
                   bsc_max_k=10,
                   use_bpl=TRUE,
                   bpl_max_steps=10,
                   bpl_inc_ratio=0.25,
                   bpl_anchor=0.75,
                   bpl_valid_size=0.33,
                   opt_model_reset=TRUE,
                   epochs=100,
                   batch_size=32,
                   dir_checkpoint,
                   trace=TRUE,
                   view_metrics=FALSE,
                   keras_trace=2,
                   n_cores=2){

      start_time=Sys.time()

      #Set Up Parallel Execution
      if(use_bsc==TRUE){
        cl <- parallel::makeCluster(n_cores)
        doParallel::registerDoParallel(cl)
      }

      #Checking Prerequisites
      if(trace==TRUE){
        print(paste(date(),
                    "Start"))
      }
      if(trace==TRUE){
        print(paste(date(),
                    "Matching Input and Target Data"))
      }
      data_embeddings=data_embeddings$clone(deep=TRUE)
      viable_cases=base::intersect(x=rownames(data_embeddings$embeddings),
                                   names(data_targets))
      data_embeddings$embeddings=data_embeddings$embeddings[viable_cases,]
      data_targets=data_targets[viable_cases]

      #Reducing to unique cases
      if(trace==TRUE){
        print(paste(date(),
                    "Checking Uniqueness of Data"))
      }
      n_init_cases=nrow(data_embeddings$embeddings)
      data_embeddings$embeddings=unique(data_embeddings$embeddings)
      n_final_cases=nrow(data_embeddings$embeddings)
      viable_cases=base::intersect(x=rownames(data_embeddings$embeddings),
                                   names(data_targets))
      data_embeddings$embeddings=data_embeddings$embeddings[viable_cases,]
      data_targets=data_targets[viable_cases]
      if(trace==TRUE){
        print(paste(date(),
                    "Total Cases:",n_init_cases,
                    "Unique Cases:",n_final_cases))
      }


      if(trace==TRUE){
        print(paste(date(),
                    "Checking Minimal Frequencies."))
      }
      freq_check<-table(data_targets)
      freq_check_eval<-freq_check<4
      if(sum(freq_check_eval)>0){
        cat<-subset(names(freq_check),
                    subset = freq_check_eval)
        stop(paste("Categories",cat,"have absolute frequencies below 4.",
             "These categories are not suitable for training.
             Please remove the corresponding categories/labels from the data
             and create a new classifier with the reduced data set."))
        }


      #Split data into k folds
      folds=get_folds(target=data_targets,k_folds=data_n_valid_samples)

      #Create a Vector with names of categories
      categories<-names(table(data_targets))

      #Saving Training Information
      n_unlabeled_data=length(data_targets)-length(na.omit(data_targets))
      names(n_unlabeled_data)="unlabeled"
      n_labeled_data=as.vector(table(data_targets))
      names(n_labeled_data)=names(table(data_targets))
      self$last_training$data=append(n_labeled_data,n_unlabeled_data)

      #Init Object for Saving pbl_labeling
      data_pbl=matrix(data=0,
                      nrow=folds$n_folds,
                      ncol=bpl_max_steps)
      dimnames(data_pbl)=list(folds=NULL,
                              bpl_steps=NULL)

      data_bsc_train=matrix(data = 0,
                      nrow = folds$n_folds+1,
                      ncol = length(categories))
      rownames(data_bsc_train)=c(paste0("fold_",seq(from=1,to=folds$n_folds,by=1)),
                                 "final")

      data_bsc_test=data_bsc_train

      #Initalazing Objects for Saving Performance
      val_metric=array(dim=c(folds$n_folds,
                             bpl_max_steps+4,
                             17),
                       dimnames = list(iterations=NULL,
                                       steps=c("Baseline",
                                               "BSC",
                                               paste0("BPL_Step",seq(from=1,to=bpl_max_steps,by=1)),
                                               "BPL",
                                               "Final"),
                                       metrics=c("iota_index",
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
                                                 "gwet_ac")))
      iota_objects_start=NULL
      iota_objects_end=NULL
      iota_objects_start_free=NULL
      iota_objects_end_free=NULL


      for(iter in 1:folds$n_folds){
        #---------------------------------------------


        #Create a Train and Validation Sample
        targets_val=data_targets[folds$val_sample[[iter]]]
        targets_train=data_targets[c(folds$train_sample[[iter]],
                                     folds$unlabeled_cases)]

        embeddings_val=data_embeddings$clone(deep=TRUE)
        embeddings_val$embeddings=embeddings_val$embeddings[folds$val_sample[[iter]],]
        embeddings_val$embeddings=na.omit(embeddings_val$embeddings)

        embeddings_train=data_embeddings$clone(deep=TRUE)
        embeddings_train$embeddings=embeddings_train$embeddings[names(targets_train),]
        #embeddings_train$embeddings=na.omit(embeddings_train$embeddings)

        #Train baseline model or normal training--------------------------------
        if(use_baseline==TRUE |
           (use_bsc==FALSE & use_bpl==FALSE) |
           (use_bsc==FALSE & use_bpl==TRUE)){
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Training Baseline Model"))
          }

          #omit cases with na
          current_train_targets=na.omit(targets_train)
          current_train_embedding=embeddings_train$embeddings[names(current_train_targets),]

          #Get Train and Test Sample
          baseline_sample<-get_stratified_train_test_split(
            targets = current_train_targets,
            val_size = bsl_val_size
          )

          #Train model
          private$basic_train(embedding_train=current_train_embedding[baseline_sample$train_sample,],
                              target_train=current_train_targets[baseline_sample$train_sample],
                              embedding_test=current_train_embedding[baseline_sample$test_sample,],
                              target_test=current_train_targets[baseline_sample$test_sample],
                              epochs=epochs,
                              batch_size=batch_size,
                              rel_tolerance=rel_tolerance,
                              patience=patience,
                              trace=trace,
                              keras_trace=keras_trace,
                              view_metrics=view_metrics,
                              reset_model=TRUE,
                              dir_checkpoint=dir_checkpoint)
          #Predict val targets
          val_predictions=self$predict(newdata=embeddings_val)
          #compare prediction and true values
          #print(targets_val)
          #print(val_predictions$expected_category)
          #print(cbind(targets_val,val_predictions$expected_category))

          val_res=get_coder_metrics(true_values = targets_val,
                                    predicted_values = val_predictions$expected_category)
          #Save results for baseline model
          val_metric[iter,1,]<-val_res

          if(use_bsc==TRUE | use_bpl==TRUE){

          iota_objects_start[iter]=list(iotarelr::check_new_rater(true_values = targets_val,
                                                                  assigned_values = val_predictions$expected_category,
                                                                  free_aem = FALSE))
          iota_objects_start_free[iter]=list(iotarelr::check_new_rater(true_values = targets_val,
                                                                       assigned_values = val_predictions$expected_category,
                                                                       free_aem = TRUE))
          }
        }

        #Create and Train with synthetic cases----------------------------------
        if(use_bsc==TRUE){
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Applying Augmention with Balanced Synthetic Cases"))
          }
          #Split data in labeled and unlabeled cases
          train_data_labeled_unlabeled=split_labeled_unlabeled(embedding = embeddings_train,
                                                               target = targets_train)

          embeddings_train_unlabeled=train_data_labeled_unlabeled$embeddings_unlabeled
          embeddings_train_labeled=train_data_labeled_unlabeled$embeddings_labeled
          targets_train_labeled=train_data_labeled_unlabeled$targets_labeled

          #Generating Data For Training
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Generating Synthetic Cases"))
          }
          #save(embeddings_train_labeled,targets_train_labeled,bsc_methods,bsc_max_k,
          #     file="debug.RData")
          syn_cases<-get_synthetic_cases(embedding=embeddings_train_labeled,
                                         target=targets_train_labeled,
                                         method=bsc_methods,
                                         max_k=bsc_max_k)
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Generating Synthetic Cases Done"))
          }

          #Combining original labeled data and synthetic data
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Selecting Synthetic Cases"))
          }
          #Checking frequencies of categories and adding syn_cases
          cat_freq=table(targets_train_labeled)
          cat_max=max(cat_freq)
          cat_delta=cat_max-cat_freq

          cat_freq_syn=table(syn_cases$syntetic_targets)

          syn_cases_selected=NULL
          for(cat in categories){
            if(cat_delta[cat]>0){
              condition=(syn_cases$syntetic_targets==cat)
              tmp_subset=subset(x = syn_cases$syntetic_targets,
                                subset = condition)
              syn_cases_selected[cat]=list(
                sample(x=names(tmp_subset),
                       size = min(cat_delta[cat],length(tmp_subset)),
                       replace = FALSE)
              )
            }
          }
          syn_cases_selected=unlist(syn_cases_selected)

          #Combining original labeled data and synthetic data
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Combining Original and Synthetic Data"))
          }
          embeddings_labeled_syn=rbind(embeddings_train_labeled,
                                       syn_cases$syntetic_embeddings[syn_cases_selected,])
          targets_labeled_syn=c(targets_train_labeled,as.factor(syn_cases$syntetic_targets[syn_cases_selected]))

          #Ensuring that only unique cases are part of the data
          #if(trace==TRUE){
          #  print(paste(date(),
          #              "Iter:",iter,"from",folds$n_folds,
          #              "Reducing Data to Unique Cases"))
          #}
          #embeddings_labeled_syn=dplyr::distinct(embeddings_labeled_syn)
          targets_labeled_syn=targets_labeled_syn[rownames(embeddings_labeled_syn)]
          #if(trace==TRUE){
          #  print(paste(date(),
          #              "Iter:",iter,"from",folds$n_folds,
          #              "Reducing Data to Unique Cases Done"))
          #}

          #Creating training and test sample
          bsc_train_test_split<-get_stratified_train_test_split(
            targets = targets_labeled_syn,
            val_size=0.25)
          val_sampel_bsc=bsc_train_test_split$test_sample
          train_sample_bsc=bsc_train_test_split$train_sample

          #Creating the final dataset for training. Please note that units
          #with NA in target are included for pseudo labeling if requested
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Creating Train Dataset"))
          }
          embeddings_train_train=rbind(
            embeddings_labeled_syn[train_sample_bsc,],
            embeddings_train_unlabeled)
          targets_train_train=c(
            targets_labeled_syn[train_sample_bsc],
            targets_train[rownames(embeddings_train_unlabeled)]
          )

          #Creating the final test dataset for training
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Creating Test Dataset"))
          }
          embeddings_train_test=embeddings_labeled_syn[val_sampel_bsc,]
          targets_train_test=as.factor(targets_labeled_syn[val_sampel_bsc])

          #omit cases with na
          current_train_targets=na.omit(targets_train_train)
          current_train_embedding=embeddings_train_train[names(current_train_targets),]

          #Save freq of every labeled original and synthetic case
          data_bsc_train[iter,]<-table(current_train_targets)
          data_bsc_test[iter,]<-table(targets_train_test)

          #Train model
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Start Training"))
          }
          private$basic_train(embedding_train=current_train_embedding,
                              target_train=current_train_targets,
                              embedding_test=embeddings_train_test,
                              target_test=targets_train_test,
                              epochs=epochs,
                              batch_size=batch_size,
                              rel_tolerance=rel_tolerance,
                              patience=patience,
                              trace=trace,
                              keras_trace=keras_trace,
                              view_metrics=view_metrics,
                              reset_model=opt_model_reset,
                              dir_checkpoint=dir_checkpoint)
          #Predict val targets
          val_predictions=self$predict(newdata=embeddings_val)
          #compare prediction and true values
          val_res=get_coder_metrics(true_values = targets_val,
                                    predicted_values = val_predictions$expected_category)
          #Save results for baseline model
          val_metric[iter,2,]<-val_res
        }

        #Applying Pseudo Labeling-----------------------------------------------
        if(use_bpl==TRUE){
          categories<-names(table(data_targets))
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Applying Balanced Pseudo Labeling"))
          }
          added_cases_train=100
          step=1

          if(use_bsc==TRUE){
            targets_augmented=targets_train_train
            current_test_embedding=embeddings_train_test
            current_test_targets=targets_train_test
          } else {
            targets_augmented=targets_train
            current_test_embedding=embeddings_val
            current_test_targets=targets_val
          }

          while(step <=bpl_max_steps & added_cases_train>0){
            #Augmenting Data

            #Select targets with no label
            condition=is.na(targets_augmented)
            remaining_targets=subset(targets_augmented,
                                     subset=condition)

            #Select the corresponding embeddings
            remaining_embeddings<-embeddings_train$clone(deep=TRUE)
            remaining_embeddings$embeddings<-remaining_embeddings$embeddings[names(remaining_targets),]

            #Estimate the labels for the remaining data
            est_remaining_data=self$predict(newdata=remaining_embeddings)

            #Create Matrix for saving the results
            new_categories<-matrix(nrow= nrow(est_remaining_data),
                                   ncol=2)
            rownames(new_categories)=rownames(est_remaining_data)
            colnames(new_categories)=c("cat","prob")

            #Gather information for every case. That is the category with the
            #highest probaiblits and save both
            for(i in 1:nrow(est_remaining_data)){
              tmp_est_prob=est_remaining_data[i,1:(ncol(est_remaining_data)-1)]
              new_categories[i,1]=categories[which.max(tmp_est_prob)]
              new_categories[i,2]=max(tmp_est_prob)
            }
            new_categories<-as.data.frame(new_categories)

            #check if all possible categories are part of the estimates.
            #if not stop process here since it would influence the balance
            new_cat_freq=table(new_categories$cat)
            min_new_freq=max(floor(min(new_cat_freq)*bpl_inc_ratio),1)

            if(length(new_cat_freq)==length(categories)){
              #Transforming the probabilities to a information index
              new_categories[,2]=abs(bpl_anchor-(as.numeric(new_categories[,2])-1/1/length(categories))/(1-1/length(categories)))
              new_categories=as.data.frame(new_categories)

              #Order cases with increasing distance from maximal information
              final_new_categories=NULL
              for(cat in categories){
                condition=new_categories[,1]==cat
                tmp=subset(x=new_categories,
                           subset=condition)
                tmp=tmp[order(tmp$prob,decreasing = FALSE),]
                #Chose always the same number of new cases to ensure the balance
                #of all categories
                final_new_categories=append(x=final_new_categories,
                                            values=rownames(tmp)[1:min_new_freq])
              }

              #select final categories
              new_categories=new_categories[unique(final_new_categories),]
            } else {
              new_categories=data.frame()
            }

            #Splitting augmented data into train and test set
            if(min_new_freq>=2 & nrow(new_categories)>0){
              tmp_targets=factor(new_categories[,1],
                                 levels = categories)
              names(tmp_targets)=rownames(new_categories)
              bpl_train_test_split<-get_stratified_train_test_split(
                targets = tmp_targets,
                val_size = max(1/min_new_freq,bpl_valid_size))
              targets_augmented_train=bpl_train_test_split$train_sample
              targets_augmented_test=bpl_train_test_split$test_sample
            } else {
              targets_augmented_train=rownames(new_categories)
              targets_augmented_test=NULL
            }

            #Augmenting Train Data with new cases
            targets_augmented[targets_augmented_train]=as.character(new_categories[targets_augmented_train,1])
            #Removing cases that are assigned to test data
            targets_augmented=targets_augmented[!names(targets_augmented)%in%targets_augmented_test]
            #Creating a factor
            targets_augmented=as.factor(targets_augmented)

            #Augmenting Test Data
            if(use_bsc==TRUE){
              current_test_embedding=rbind(
                current_test_embedding,
                embeddings_train$embeddings[targets_augmented_test,])
            } else {
              current_test_embedding$embeddings=rbind(
                current_test_embedding$embeddings,
                embeddings_train$embeddings[targets_augmented_test,])
            }
            current_test_targets=c(current_test_targets,
                                   factor(as.character(new_categories[targets_augmented_test,1]),
                                          levels = categories))
            current_test_targets=as.factor(current_test_targets)

            #Counting new cases
            added_cases_train=length(targets_augmented_train)
            added_cases_test=length(targets_augmented_test)
            data_pbl[iter,step]=added_cases_train+added_cases_test
            n_cases_total=length(subset(targets_augmented,is.na(targets_augmented)==FALSE))

            if(trace==TRUE){
              print(paste(date(),
                          "Iter:",iter,"from",folds$n_folds,
                          "Step:",step,"/",bpl_max_steps,
                          "Added Cases Test:",added_cases_test,
                          "Added Cases Train:",added_cases_train,
                          "Cases Total Train:",n_cases_total))
            }

            #Selecting the correct data for new training
            current_train_targets=na.omit(targets_augmented)

            if(use_bsc==TRUE){
              current_train_embedding=embeddings_train_train[names(current_train_targets),]
            } else {
              current_train_embedding=embeddings_train$embeddings[names(current_train_targets),]
            }

            if(added_cases_train>0){
              #Train model
              private$basic_train(embedding_train=current_train_embedding,
                                  target_train=current_train_targets,
                                  embedding_test=current_test_embedding,
                                  target_test=current_test_targets,
                                  epochs=epochs,
                                  batch_size=batch_size,
                                  rel_tolerance=rel_tolerance,
                                  patience=patience,
                                  trace=trace,
                                  keras_trace=keras_trace,
                                  view_metrics=view_metrics,
                                  reset_model=opt_model_reset,
                                  dir_checkpoint=dir_checkpoint)

              #Predict val targets
              val_predictions=self$predict(newdata=embeddings_val)

              #compare prediction and true values
              val_res=get_coder_metrics(true_values = targets_val,
                                        predicted_values = val_predictions$expected_category)
              #Save results for baseline model
              val_metric[iter,2+step,]<-val_res
            }

            #increase step
            step=step+1
          }
          val_metric[iter,"BPL",]<-val_res
        }
        val_metric[iter,"Final",]<-val_res

        iota_objects_end[iter]=list(iotarelr::check_new_rater(true_values = targets_val,
                                                                assigned_values = val_predictions$expected_category,
                                                                free_aem = FALSE))
        iota_objects_end_free[iter]=list(iotarelr::check_new_rater(true_values = targets_val,
                                                                     assigned_values = val_predictions$expected_category,
                                                                     free_aem = TRUE))



        #----------------------------------------------
      }

      #Insert Final Training here and Savings here
      self$reliability$val_metric=val_metric
      self$reliability$raw_iota_objects$iota_objects_start=iota_objects_start
      self$reliability$raw_iota_objects$iota_objects_end=iota_objects_end
      self$reliability$raw_iota_objects$iota_objects_start_free=iota_objects_start_free
      self$reliability$raw_iota_objects$iota_objects_end_free=iota_objects_end_free

      if(is.null(iota_objects_start)==FALSE){
        self$reliability$iota_object_start=create_iota2_mean_object(
          iota2_list = iota_objects_start,
          original_cat_labels = categories,
          free_aem=FALSE,
          call="aifeducation::te_classifier_neuralnet")
      } else {
        self$reliability$iota_object_start=NULL
      }

      if(is.null(iota_objects_end)==FALSE){
        self$reliability$iota_object_end=create_iota2_mean_object(
          iota2_list = iota_objects_end,
          original_cat_labels = categories,
          free_aem=FALSE,
          call="aifeducation::te_classifier_neuralnet")
      } else {
        self$reliability$iota_objects_end=NULL
      }

      if(is.null(iota_objects_start_free)==FALSE){
        self$reliability$iota_object_start_free=create_iota2_mean_object(
          iota2_list = iota_objects_start_free,
          original_cat_labels = categories,
          free_aem=TRUE,
          call="aifeducation::te_classifier_neuralnet")
      } else {
        self$reliability$iota_objects_start_free=NULL
      }

      if(is.null(iota_objects_end_free)==FALSE){
        self$reliability$iota_object_end_free=create_iota2_mean_object(
          iota2_list = iota_objects_end_free,
          original_cat_labels = categories,
          free_aem=TRUE,
          call="aifeducation::te_classifier_neuralnet")
      } else {
        self$reliability$iota_objects_end_free=NULL
      }

      #Final Training----------------------------------------------------------
      if(use_bsc==FALSE & use_bpl==FALSE){
        embeddings_train=data_embeddings
        targets_train=data_targets
        if(trace==TRUE){
          print(paste(date(),
                      "Iter:","Final Training",
                      "Final Training of Baseline Model"))
        }

        #omit cases with na
        current_train_targets=na.omit(targets_train)
        current_train_embedding=embeddings_train$embeddings[names(current_train_targets),]

        #Get Train and Test Sample
        baseline_sample<-get_stratified_train_test_split(
          targets = current_train_targets,
          val_size = bsl_val_size
        )

        #Train model
        private$basic_train(embedding_train=current_train_embedding[baseline_sample$train_sample,],
                            target_train=current_train_targets[baseline_sample$train_sample],
                            embedding_test=current_train_embedding[baseline_sample$test_sample,],
                            target_test=current_train_targets[baseline_sample$test_sample],
                            epochs=epochs,
                            batch_size=batch_size,
                            rel_tolerance=rel_tolerance,
                            patience=patience,
                            trace=trace,
                            keras_trace=keras_trace,
                            view_metrics=view_metrics,
                            reset_model=TRUE,
                            dir_checkpoint=dir_checkpoint)


      }

      #Final Training with BSC--------------------------------------------------

      if(use_bsc==TRUE){
        #Setting embedding train to the complete data set
        embeddings_train=data_embeddings
        targets_train=data_targets

        if(trace==TRUE){
          print(paste(date(),
                      "Iter:","Final Training",
                      "Applying Augmention with Balanced Synthetic Cases for Final Training"))
        }
        #Split data in labeled and unlabeled cases
        train_data_labeled_unlabeled=split_labeled_unlabeled(embedding = embeddings_train,
                                                             target = targets_train)

        embeddings_train_unlabeled=train_data_labeled_unlabeled$embeddings_unlabeled
        embeddings_train_labeled=train_data_labeled_unlabeled$embeddings_labeled
        targets_train_labeled=train_data_labeled_unlabeled$targets_labeled

        #Generating Data For Training
        if(trace==TRUE){
          print(paste(date(),
                      "Iter:","Final Training",
                      "Generating Synthetic Cases"))
        }
        syn_cases<-get_synthetic_cases(embedding=embeddings_train_labeled,
                                       target=targets_train_labeled,
                                       method=bsc_methods,
                                       max_k=bsc_max_k)
        if(trace==TRUE){
          print(paste(date(),"Generating Synthetic Cases Done"))
        }

        #Combining original labeled data and synthetic data
        if(trace==TRUE){
          print(paste(date(),
                      "Iter:","Final Training",
                      "Selecting Synthetic Cases"))
        }
        #Checking frequencies of categories and adding syn_cases
        cat_freq=table(targets_train_labeled)
        cat_max=max(cat_freq)
        cat_delta=cat_max-cat_freq

        cat_freq_syn=table(syn_cases$syntetic_targets)

        syn_cases_selected=NULL
        for(cat in categories){
          if(cat_delta[cat]>0){
            condition=(syn_cases$syntetic_targets==cat)
            tmp_subset=subset(x = syn_cases$syntetic_targets,
                              subset = condition)
            syn_cases_selected[cat]=list(
              sample(x=names(tmp_subset),
                     size = min(cat_delta[cat],length(tmp_subset)),
                     replace = FALSE)
            )
          }
        }
        syn_cases_selected=unlist(syn_cases_selected)

        #Combining original labeled data and synthetic data
        if(trace==TRUE){
          print(paste(date(),
                      "Iter:","Final Training",
                      "Combining Original and Synthetic Data"))
        }
        embeddings_labeled_syn=rbind(embeddings_train_labeled,
                                     syn_cases$syntetic_embeddings[syn_cases_selected,])
        targets_labeled_syn=c(targets_train_labeled,as.factor(syn_cases$syntetic_targets[syn_cases_selected]))

        #Ensuring that only unique cases are part of the data
        #if(trace==TRUE){
        #  print(paste(date(),
        #              "Iter:",iter,"from",folds$n_folds,
        #              "Reducing Data to Unique Cases"))
        #}
        #embeddings_labeled_syn=dplyr::distinct(embeddings_labeled_syn)
        targets_labeled_syn=targets_labeled_syn[rownames(embeddings_labeled_syn)]
        #if(trace==TRUE){
        #  print(paste(date(),
        #              "Iter:",iter,"from",folds$n_folds,
        #              "Reducing Data to Unique Cases Done"))
        #}

        #Creating training and test sample
        bsc_train_test_split<-get_stratified_train_test_split(
          targets = targets_labeled_syn,
          val_size=0.25)
        val_sampel_bsc=bsc_train_test_split$test_sample
        train_sample_bsc=bsc_train_test_split$train_sample

        #Creating the final dataset for training. Please note that units
        #with NA in target are included for pseudo labeling if requested
        embeddings_train_train=rbind(
          embeddings_labeled_syn[train_sample_bsc,],
          embeddings_train_unlabeled)
        targets_train_train=c(
          targets_labeled_syn[train_sample_bsc],
          targets_train[rownames(embeddings_train_unlabeled)]
        )

        #Creating the final test dataset for training
        embeddings_train_test=embeddings_labeled_syn[val_sampel_bsc,]
        targets_train_test=as.factor(targets_labeled_syn[val_sampel_bsc])

        #omit cases with na
        current_train_targets=na.omit(targets_train_train)
        current_train_embedding=embeddings_train_train[names(current_train_targets),]

        data_bsc_train[folds$n_folds+1,]<-table(current_train_targets)
        data_bsc_test[folds$n_folds+1,]<-table(targets_train_test)

        #Train model
        private$basic_train(embedding_train=current_train_embedding,
                            target_train=current_train_targets,
                            embedding_test=embeddings_train_test,
                            target_test=targets_train_test,
                            epochs=epochs,
                            batch_size=batch_size,
                            rel_tolerance=rel_tolerance,
                            patience=patience,
                            trace=trace,
                            view_metrics=view_metrics,
                            keras_trace=keras_trace,
                            reset_model=opt_model_reset,
                            dir_checkpoint=dir_checkpoint)
      }

      if(use_bpl==TRUE){
        #Applying Pseudo Labeling-----------------------------------------------
        if(use_bpl==TRUE){
          categories<-names(table(data_targets))
          if(trace==TRUE){
            print(paste(date(),
                        "Iter:","Final Training",
                        "Applying Balanced Pseudo Labeling for Final Training"))
          }

          added_cases_train=100
          step=1

          if(use_bsc==TRUE){
            targets_augmented=targets_train_train
            current_test_embedding=embeddings_train_test
            current_test_targets=targets_train_test
          } else {
            targets_augmented=data_targets
            current_test_embedding=data_embeddings$embeddings
            current_test_targets=targets_val
          }

          while(step <=bpl_max_steps & added_cases_train>0){
            #Augmenting Data

            #Select targets with no label
            remaining_targets=subset(targets_augmented,is.na(targets_augmented)==TRUE)

            #Select the corresponding embeddings
            remaining_embeddings<-embeddings_train$clone(deep=TRUE)
            remaining_embeddings$embeddings<-remaining_embeddings$embeddings[names(remaining_targets),]

            #Estimate the labels for the remaining data
            est_remaining_data=self$predict(newdata=remaining_embeddings)
            #Create Matrix for saving the results
            new_categories<-matrix(nrow= nrow(est_remaining_data),
                                   ncol=2)
            rownames(new_categories)=rownames(est_remaining_data)
            colnames(new_categories)=c("cat","prob")

            #Gather information for every case. That is the category with the
            #highest probaiblits and save both
            for(i in 1:nrow(est_remaining_data)){
              tmp_est_prob=est_remaining_data[i,1:(ncol(est_remaining_data)-1)]
              new_categories[i,1]=categories[which.max(tmp_est_prob)]
              new_categories[i,2]=max(tmp_est_prob)
            }
            new_categories<-as.data.frame(new_categories)

            #check if all possible categories are part of the estimates.
            #if not stop process here since it would influence the balance
            new_cat_freq=table(new_categories$cat)
            min_new_freq=max(floor(min(new_cat_freq)*bpl_inc_ratio),1)

            if(length(new_cat_freq)==length(categories)){
              #Transforming the probabilities to a information index
              new_categories[,2]=abs(bpl_anchor-(as.numeric(new_categories[,2])-1/1/length(categories))/(1-1/length(categories)))
              new_categories=as.data.frame(new_categories)

              #Order cases with increasing distance from maximal information
              final_new_categories=NULL
              for(cat in categories){
                condition=new_categories[,1]==cat
                tmp=subset(x=new_categories,
                           subset=condition)
                tmp=tmp[order(tmp$prob,decreasing = FALSE),]
                #Chose always the same number of new cases to ensure the balance
                #of all categories
                final_new_categories=append(x=final_new_categories,
                                            values=rownames(tmp)[1:min_new_freq])
              }

              #select final categories
              new_categories=new_categories[unique(final_new_categories),]
            } else {
              new_categories=data.frame()
            }

            #Splitting augmented data into train and test set
            if(min_new_freq>=2 & nrow(new_categories)>0){
              tmp_targets=factor(new_categories[,1],
                                 levels = categories)
              names(tmp_targets)=rownames(new_categories)
              bpl_train_test_split<-get_stratified_train_test_split(
                targets = tmp_targets,
                val_size = max(1/min_new_freq,bpl_valid_size))
              targets_augmented_train=bpl_train_test_split$train_sample
              targets_augmented_test=bpl_train_test_split$test_sample
            } else {
              targets_augmented_train=rownames(new_categories)
              targets_augmented_test=NULL
            }

            #Augmenting Train Data with new cases
            targets_augmented[targets_augmented_train]=as.character(new_categories[targets_augmented_train,1])
            #Removing cases that are assigned to test data
            targets_augmented=targets_augmented[!names(targets_augmented)%in%targets_augmented_test]
            #Creating a factor
            targets_augmented=as.factor(targets_augmented)

            #Augmenting Test Data
            if(use_bsc==TRUE){
              current_test_embedding=rbind(
                current_test_embedding,
                embeddings_train$embeddings[targets_augmented_test,])
            } else {
              current_test_embedding$embeddings=rbind(
                current_test_embedding$embeddings,
                embeddings_train$embeddings[targets_augmented_test,])
            }
            current_test_targets=c(current_test_targets,
                                   factor(as.character(new_categories[targets_augmented_test,1]),
                                          levels = categories))
            current_test_targets=as.factor(current_test_targets)

            #Counting new cases
            added_cases_train=length(targets_augmented_train)
            added_cases_test=length(targets_augmented_test)
            data_pbl[iter,step]=added_cases_train+added_cases_test
            n_cases_total=length(subset(targets_augmented,is.na(targets_augmented)==FALSE))

            if(trace==TRUE){
              print(paste(date(),
                          "Iter:","Final Training",
                          "Step:",step,"/",bpl_max_steps,
                          "Added Cases Test:",added_cases_test,
                          "Added Cases Train:",added_cases_train,
                          "Cases Total Train:",n_cases_total))
            }

            #Selecting the correct data for new training
            current_train_targets=na.omit(targets_augmented)
            if(added_cases_train>0){
              if(use_bsc==TRUE){
                current_train_embedding=embeddings_train_train[names(current_train_targets),]
              } else {
                current_train_embedding=embeddings_train$embeddings[names(current_train_targets),]
              }

              #Train model
              private$basic_train(embedding_train=current_train_embedding,
                                  target_train=current_train_targets,
                                  embedding_test=current_test_embedding,
                                  target_test=current_test_targets,
                                  epochs=epochs,
                                  batch_size=batch_size,
                                  rel_tolerance=rel_tolerance,
                                  patience=patience,
                                  trace=trace,
                                  keras_trace=keras_trace,
                                  view_metrics=view_metrics,
                                  reset_model=opt_model_reset,
                                  dir_checkpoint=dir_checkpoint)
            }
            #increase step
            step=step+1
          }
        }
      }
      #Save Final Information
      self$last_training$date=date()
      self$last_training$config$use_bsc=use_bsc
      self$last_training$config$use_baseline=use_baseline
      self$last_training$config$use_bpl=use_bpl
      self$last_training$n_samples=folds$n_folds

      self$last_training$data_bsc_train=data_bsc_train
      self$last_training$data_bsc_test=data_bsc_test

      if(use_bpl==TRUE){
        self$last_training$data_pbl=data_pbl
      } else {
        self$last_training$data_pbl=NULL
      }

      val_metric_mean=matrix(data=0,
                             nrow = nrow(val_metric[1,,]),
                             ncol = ncol(val_metric[1,,]))
      rownames(val_metric_mean)=rownames(val_metric[1,,])
      colnames(val_metric_mean)=colnames(val_metric[1,,])

      n_mean=vector(length = nrow(val_metric[1,,]))
      n_mean[]=folds$n_folds

      for(i in 1:folds$n_folds){
          tmp_val_metric=val_metric[i,,]
          for(j in 1:nrow(tmp_val_metric)){
            if(sum(is.na(tmp_val_metric[j,]))!=length(tmp_val_metric[j,])){
              val_metric_mean[j,]=val_metric_mean[j,]+tmp_val_metric[j,]
            } else {
              n_mean[j]=n_mean[j]-1
            }
          }
        }

      val_metric_mean=val_metric_mean/n_mean
      val_metric_mean[is.nan(val_metric_mean)]=NA
      self$reliability$val_metric_mean=val_metric_mean

      self$last_training$learning_time=as.numeric(difftime(Sys.time(),start_time,
                                                           units="mins"))

      #Unload Cluster for Parallel Execution
      if(use_bsc==TRUE){
        parallel::stopCluster(cl)
      }

      if(trace==TRUE){
        print(paste(date(),
                    "Training Complete"))
      }
    },
    #-------------------------------------------------------------------------
    #'@description Method for prediciting new data with a trained neural net.
    #'@param newdata Object of class \code{TextEmbeddingModel} or
    #'\code{data.frame} for which predictions should be made.
    #'@return Returns a \code{data.frame} containing the predictions and
    #'the probabilities of the different labels for each case.
    predict=function(newdata){
      #Checking input data
      #if(methods::isClass(where=newdata,"data.frame")==FALSE){
      #  stop("newdata mus be a data frame")
      #}
      if("EmbeddedText" %in%class(newdata)){
        real_newdata=newdata$embeddings
      } else {
        real_newdata=newdata
      }

      #Ensuring the correct order of the variables for prediction
      real_newdata<-real_newdata[self$model_config$input_variables]
      current_row_names=rownames(real_newdata)
      if(self$model_config$n_gru>0){
        real_newdata<-matrix_to_array_c(
          matrix = as.matrix(real_newdata),
          times = self$text_embedding_model$times,
          features = self$text_embedding_model$features)
      } else {
        real_newdata=as.matrix(real_newdata)
      }

      #Predicting target variable
      model<-bundle::unbundle(self$bundeled_model)
      predictions_prob<-model %>% stats::predict(real_newdata)
      predictions<-predictions_prob %>% keras::k_argmax()

      #Transforming predictions to target levels
      predictions<-as.character(as.vector(predictions))
      for(i in 0:(length(self$model_config$target_levels)-1)){
        predictions<-replace(x=predictions,
                             predictions==as.character(i),
                             values=self$model_config$target_levels[i+1])
      }

      #Transforming to a factor
      predictions=factor(predictions,levels = self$model_config$target_levels)

      colnames(predictions_prob)=self$model_config$target_levels
      predictions_prob<-as.data.frame(predictions_prob)
      predictions_prob$expected_category=predictions
      rownames(predictions_prob)=current_row_names

      return(predictions_prob)
    },
    #General Information set and get--------------------------------------------
    #'@description Method for setting publication information of the classifier
    #'@param type \code{string} for choosing the type of information that should
    #'be added. \code{type="developer"} for information about the developer of the
    #'classifier and \code{type="trainer"} for information about persons who trained
    #'the classifier. If you modify an already existing classifier please use
    #'\code{type="modifier"}.
    #'@param autors Person list of authors.
    #'@param citation Free text citation.
    #'@param url URL of a corresponding homepage.
    set_publication_info=function(type,
                                  autors,
                                  citation,
                                  url=NULL){
      if(type=="developer"){
        private$publication_info$developed_by$authors<-autors
        private$publication_info$developed_by$citation<-citation
        private$publication_info$developed_by$url<-url
      } else if(type=="trainer"){
        private$publication_info$trained_by$authors<-autors
        private$publication_info$trained_by$citation<-citation
        private$publication_info$trained_by$url<-url
      } else if(type=="modifier"){
        private$publication_info$modifided_by$authors<-autors
        private$publication_info$modifided_by$citation<-citation
        private$publication_info$modifided_by$url<-url
      }
    },
    #--------------------------------------------------------------------------
    #'@description Method for requesting the publication information of the classifier.
    #'@return \code{list} with all saved publication information.
    get_publication_info=function(){
      return(private$publication_info)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting the license of the classifier.
    #'@param license \code{string} containing the abbreviation of the license or
    #'the license text.
    set_license=function(license){
      private$model_info$model_license<-license
    },
    #'@description Method for requesting the license of the classifier.
    #'@return \code{string} License of the classifier.
    get_license=function(){
      return(private$model_info$model_license)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting a description of the classifier.
    #'@param eng \code{string} A text describing the training of the learner,
    #'its theoretical and empirical background, and the different output labels
    #'in English.
    #'@param native \code{string} A text describing the training of the learner,
    #'its theoretical and empirical background, and the different output labels
    #'in the native language of the classifier.
    set_model_description=function(eng=NULL,native=NULL){
      if(!is.null(description)){
        private$model_description$eng=description
      }
      if(!is.null(description_native)){
        private$model_description$native=description
      }
    },
    #'@description Method for requesting the model description.
    #'@return \code{list} with the description of the classifier in English
    #'and native language.
    get_model_description=function(){
      return(private$model_description)
    },
    #-------------------------------------------------------------------------
    #'@description Method for exporting a model to tensorflow SavedModel format.
    #'@param dir_path \code{string()} Path of the directory where the model should be
    #'saved.
     export_model=function(dir_path){
      tmp_model=bundle::unbundle(self$bundeled_model)
      tmp_model$save(dir_path)
    },
    #'@description Method for importing a model from tensorflow SavedModel format.
    #'@param dir_path \code{string()} Path of the directory where the model is
    #'saved.
    import_model=function(dir_path){
      tf=reticulate::import("tensorflow")
      tmp_model<-tf$keras$models$load_model(dir_path)
      self$bundeled_model=bundle::bundle(tmp_model)
    }
  ),
  private = list(
    #General Information-------------------------------------------------------
    publication_info=list(
      developed_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      ),
      trained_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      ),
      modifided_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      )
    ),
    model_description=list(
      eng=NULL,
      native=NULL
    ),
    #Training Process----------------------------------------------------------
    init_weights=NULL,
    #--------------------------------------------------------------------------
    basic_train=function(embedding_train,
                         target_train,
                         embedding_test,
                         target_test,
                         reset_model=FALSE,
                         epochs=100,
                         batch_size=32,
                         rel_tolerance=1e-4,
                         patience=3,
                         trace=TRUE,
                         view_metrics=FALSE,
                         keras_trace=2,
                         dir_checkpoint){

      if("EmbeddedText" %in% class(embedding_train)){
        data_embedding_train=embedding_train$embeddings
      } else {
        data_embedding_train=embedding_train
      }
      if("EmbeddedText" %in% class(embedding_test)){
        data_embedding_test=embedding_test$embeddings
      } else {
        data_embedding_test=embedding_test
      }

      #load names and order of input variables and train_target levels
      variable_name_order<-self$model_config$input_variables
      target_levels_order<-self$model_config$target_levels

      #Ensuring the same encoding
      target_train<-factor(as.character(target_train),
                           levels = target_levels_order)
      target_test<-factor(as.character(target_test),
                          levels = target_levels_order)

      data_embedding_train<-data_embedding_train[,variable_name_order]
      data_embedding_test<-data_embedding_test[,variable_name_order]

      #variable_name_order<-colnames(data_embedding_train)
      #target_levels_order<-levels(target_train)

      #Transforming train_target for the use in keras.
      #That is switching characters to numeric
      target_train_transformed<-as.numeric(target_train)-1
      target_test_transformed<-as.numeric(target_test)-1

      #Estimating Class Weights
      #freq_categories=table(target_train)
      #freq_max=max(freq_categories)
      #weights_categories=as.list(freq_max/freq_categories)

      if(self$model_config$n_gru>0){
        #Convert Input data to sequential data
        input_embeddings_train<-matrix_to_array_c(
          matrix = as.matrix(data_embedding_train),
          times = self$text_embedding_model$times,
          features = self$text_embedding_model$features)
        input_embeddings_test<-matrix_to_array_c(
          matrix = as.matrix(data_embedding_test),
          times = self$text_embedding_model$times,
          features = self$text_embedding_model$features)
      } else {
        input_embeddings_train= as.matrix(data_embedding_train)
        input_embeddings_test=as.matrix(data_embedding_test)
      }

      n_categories=as.integer(length(levels(target_train)))

      output_categories_train=keras::to_categorical(y=target_train_transformed,
                                                    num_classes=n_categories)
      output_categories_test=keras::to_categorical(y=target_test_transformed,
                                                   num_classes=n_categories)

      model<-bundle::unbundle(self$bundeled_model)

      if(reset_model==TRUE){
        model$set_weights(private$init_weights)
        print("Model reseted")
      }

      tf<-reticulate::import("tensorflow")

      if(self$model_config$init_config$optimizer=="adam"){
        model %>% keras::compile(
          loss = self$model_config$init_config$err_fct,
          optimizer=tf$keras$optimizers$legacy$Adam(),
          metric="categorical_accuracy")
      }

      if(dir.exists(paste0(dir_checkpoint,"/checkpoints"))==FALSE){
        print(paste(date(),"Creating Checkpoint Directory"))
        dir.create(paste0(dir_checkpoint,"/checkpoints"))
      }

      train_results<- model %>% keras::fit(
        verbose=as.integer(keras_trace),
        x=input_embeddings_train,
        y=output_categories_train,
        validation_data=list(x_val=input_embeddings_test,
                             y_val=output_categories_test),
        epochs = epochs,
        batch_size = batch_size,
        #callback = keras::callback_early_stopping(monitor = "val_categorical_accuracy",
        #                                          min_delta = rel_tolerance,
        #                                          patience = patience,
        #                                          restore_best_weights = TRUE,
        #                                          verbose=as.integer(trace),
        #                                          mode = "auto"),
        callback = keras::callback_model_checkpoint(
          filepath = paste0(dir_checkpoint,"/checkpoints"),
          monitor = "val_categorical_accuracy",
          verbose = as.integer(min(keras_trace,1)),
          mode = "auto",
          save_best_only = TRUE,
          save_weights_only = TRUE),
        view_metrics=view_metrics
        #class_weight=weights_categories
      )

      print(paste(date(),"Load Weights From Best Checkpoint"))
      model$load_weights(paste0(dir_checkpoint,"/checkpoints"))

      self$bundeled_model=bundle::bundle(model)
      self$model_config$input_variables<-variable_name_order
      self$model_config$target_levels<-target_levels_order
      self$last_training$history<-train_results
    }
  )
)
