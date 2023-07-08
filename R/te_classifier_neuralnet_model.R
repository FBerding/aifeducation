#'@title Text embedding classifier with a neural net
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

    #'@field name_root ('character()')\cr
    #'Root part of the name of the classifier.
    name_root=NULL,

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
    #'Object for storing the keras model of the neural net. Saved as a bundled
    #'object with help of the package \link[bundle]{bundle} for serialization.
    bundeled_model=NULL,

    #'@field model_config ('list()')\cr
    #'List for storing information about the configuration of the model. This
    #'information is used to predict new data.
    #'\itemize{
    #'\item{\code{model_config$n_req: }}{Number of recurrent layers.}
    #'\item{\code{model_config$n_hidden: }}{Number of dense layers.}
    #'\item{\code{model_config$target_levels: }}{Levels of the target variable. Do not change this manually.}
    #'\item{\code{model_config$input_variables: }}{Order and name of the input variables. Do not change this manually.}
    #'\item{\code{model_config$init_config: }}{List storing all parameters passed to method new().}
    #'}
    model_config=list(
      n_req=NULL,
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
    #'during balanced pseudo-labeling. The rows refer to folds and final training.
    #'The columns refer to the steps during pseudo-labeling.}
    #'\item{\code{config$data_bsc_test: }}{Matrix storing the number of cases for each category used for testing
    #'during the phase of balanced synthetic units. Please note that the
    #'frequencies include original and synthetic cases. In case the number
    #'of original and synthetic cases exceeds the limit for the majority classes,
    #'the frequency represents the number of cases created by cluster analysis.}
    #'\item{\code{config$date: }}{Time when the last training finished.}
    #'\item{\code{config$config: }}{List storing which kind of estimation was requested during the last training.
    #'\itemize{
    #'\item{\code{config$config$use_bsc:  }}{\code{TRUE} if  balanced synthetic cases were requested. \code{FALSE}
    #'if not.}
    #'\item{\code{config$config$use_baseline: }}{\code{TRUE} if baseline estimation were requested. \code{FALSE}
    #'if not.}
    #'\item{\code{ config$config$use_bpl: }}{\code{TRUE} if  balanced, pseudo-labeling cases were requested. \code{FALSE}
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
    #'\item{\code{reliability$test_metric: }}{Array containing the reliability measures for the validation data for
    #'every fold, method, and step (in case of pseudo-labeling).}
    #'\item{\code{reliability$test_metric_mean: }}{Array containing the reliability measures for the validation data for
    #'every method and step (in case of pseudo-labeling). The values represent
    #'the mean values for every fold.}
    #'\item{\code{reliability$raw_iota_objects: }}{List containing all iota_object generated with the package [iotarelr]{iotarelr}
    #'for every fold at the start and the end of the last training.
    #'\itemize{
    #'\item{\code{reliability$raw_iota_objects$iota_objects_start: }}{List of objects with class \code{iotarelr_iota2} containing the
    #'estimated iota reliability of the second generation for the baseline model
    #'for every fold.
    #'If the estimation of the baseline model is not requested, the list is
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
    #'If the estimation of the baseline model is not requested, the list is
    #'set to \code{NULL}.Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'\item{\code{reliability$raw_iota_objects$iota_objects_end_free: }}{List of objects with class \code{iotarelr_iota2} containing the
    #'estimated iota reliability of the second generation for the final model
    #'for every fold. Depending of the requested training method, these values
    #'refer to the baseline model, a trained model on the basis of balanced
    #'synthetic cases, balanced pseudo-labeling or a combination of balanced
    #'synthetic cases and pseudo-labeling.
    #'Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'}
    #'}
    #'\item{\code{reliability$iota_object_start: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold. If the estimation of the baseline model is not requested, the list is
    #'set to \code{NULL}.}
    #'\item{\code{ reliability$iota_object_start_free: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold. If the estimation of the baseline model is not requested, the list is
    #'set to \code{NULL}.
    #'Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'\item{\code{reliability$iota_object_end: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold.
    #'Depending on the requested training method, this object
    #'refers to the baseline model, a trained model on the basis of balanced
    #'synthetic cases, balanced pseudo-labeling or a combination of balanced
    #'synthetic cases and pseudo-labeling.}
    #'\item{\code{reliability$iota_object_end_free: }}{Object of class \code{iotarelr_iota2} as a mean of the individual objects
    #'for every fold.
    #'Depending on the requested training method, this object
    #'refers to the baseline model, a trained model on the basis of balanced
    #'synthetic cases, balanced pseudo-labeling or a combination of balanced
    #'synthetic cases and pseudo-labeling.
    #'Please note that the model is estimated without
    #'forcing the Assignment Error Matrix to be in line with the assumption of weak superiority.}
    #'}
    reliability=list(
      test_metric=NULL,
      test_metric_mean=NULL,
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
    #'@param hidden \code{vector} containing the number of neurons for each dense layer.
    #'The length of the vector determines the number of dense layers. If you want no dense layer,
    #'set this parameter to \code{NULL}.
    #'@param rec \code{vector} containing the number of neurons for each recurrent layer.
    #'The length of the vector determines the number of dense layers. If you want no dense layer,
    #'set this parameter to \code{NULL}.
    #'@param self_attention_heads \code{integer} determining the number of attention heads
    #'for a self-attention layer. If this value is greater 0, a self-attention layer is added
    #'between the recurrent and dense layers together with a normalization and a
    #'recurrent layer. If set to 0, none of these layers are added.
    #'@param dropout \code{double} ranging between 0 and lower 1, determining the
    #'dropout for each recurrent layer.
    #'@param recurrent_dropout \code{double} ranging between 0 and lower 1, determining the
    #'recurrent dropout for each recurrent layer.
    #'@param l2_regularizer \code{double} determining the l2 regularizer for every dense layer.
    #'@param optimizer Object of class \code{keras.optimizers}.
    #'@param act_fct \code{character} naming the activation function for all dense layers.
    #'@param rec_act_fct \code{character} naming the activation function for all recurrent layers.
    #'@import bundle
    #'@import keras
    initialize=function(name=NULL,
                        label=NULL,
                        text_embeddings=NULL,
                        targets=NULL,
                        hidden=c(128),
                        rec=c(128),
                        self_attention_heads=0,
                        dropout=0.4,
                        recurrent_dropout=0.4,
                        l2_regularizer=0.01,
                        optimizer="adam",
                        act_fct="gelu",
                        rec_act_fct="tanh"
    ){
      #Checking of parameters--------------------------------------------------
      if(is.null(name)){
        stop("name is NULL but must be a character.")
      }
      if(is.null(label)){
        stop("label is NULL but must be a character.")
      }
      if(!("EmbeddedText" %in% class(text_embeddings))){
        stop("text_embeddings must be of class EmbeddedText.")
      }
      if(is.factor(targets)==FALSE){
        stop("targets must be of class factor.")
      }

      if(!(is.numeric(hidden)==TRUE | is.null(hidden)==TRUE)){
        stop("hidden must be a vector of integer or NULL.")
      }
      if(!(is.numeric(rec)==TRUE | is.null(rec)==TRUE)){
        stop("rec must be a vector of integer or NULL.")
      }
      if(is.integer(as.integer(self_attention_heads))==FALSE){
        stop("self_attention_heads must be an integer.")
      }

      if(optimizer %in% c("adam","rmsprop")==FALSE){
        stop("Optimzier must be 'adam' oder 'rmsprop'.")
      }

      #------------------------------------------------------------------------

      #Setting Label and Name
      self$name_root=name
      self$name=paste0(self$name_root,"_id_",generate_id(16))
      self$label=label

      #Basic Information of Input and Target Data
      variable_name_order<-dimnames(text_embeddings$embeddings)[[3]]
      target_levels_order<-levels(targets)

      model_info=text_embeddings$get_model_info()
      times=model_info$param_chunks
      features=dim(text_embeddings$embeddings)[3]

      self$text_embedding_model["model"]=list(model_info)
      self$text_embedding_model["times"]=times
      self$text_embedding_model["features"]=features

      if(is.null(rec) & self_attention_heads>0){
        if(features %% 2 !=0){
          stop("The number of features of the TextEmbeddingmodel is
               not a multiple of 2.")
        }
      }

      #Saving Configuration
      config=list(
        hidden=hidden,
        rec=rec,
        dropout=dropout,
        recurrent_dropout=recurrent_dropout,
        l2_regularizer=l2_regularizer,
        optimizer=optimizer,
        act_fct=act_fct,
        rec_act_fct=rec_act_fct,
        self_attention_heads=self_attention_heads)

      if(length(target_levels_order)>2){
        #Multi Class
        config["act_fct_last"]="softmax"
        config["err_fct"]="categorical_crossentropy"
        config["metric"]="categorical_accuracy"
      } else {
        #Binary Classification
        config["act_fct_last"]="sigmoid"
        config["err_fct"]="binary_crossentropy"
        config["metric"]="binary_accuracy"
      }

      #Defining basic keras model
      layer_list=NULL
      #Adding Input Layer
      n_req=length(config$rec)
      n_hidden=length(config$hidden)
      if(n_req>0 |
         self_attention_heads>0){
        model_input<-keras::layer_input(shape=list(as.integer(times),as.integer(features)),
                                        name="input_embeddings")
      } else {
        model_input<-keras::layer_input(shape=times*features,
                                        name="input_embeddings")
      }
      layer_list[1]<-list(model_input)

      #Adding a Mask-Layer
      if(n_req>0 |
         self_attention_heads>0){
        masking_layer<-keras::layer_masking(object=model_input,
                             mask_value = 0.0,
                             name="masking_layer",
                             input_shape=c(times,features),
                             trainable=FALSE)
        layer_list[length(layer_list)+1]<-list(masking_layer)
      }

      #Adding a Normalization Layer
      if(n_req>0|
         self_attention_heads>0){
        norm_layer<-keras::layer_layer_normalization(
          object=layer_list[[length(layer_list)]],
          name = "normalizaion_layer")
        layer_list[length(layer_list)+1]<-list(norm_layer)
      } else {
        norm_layer<-keras::layer_batch_normalization(
          object=layer_list[[length(layer_list)]],
          name = "normalizaion_layer")
        layer_list[length(layer_list)+1]<-list(norm_layer)
      }

      #Adding rec layer
      if(n_req>0){
        for(i in 1:n_req){
          if(i<n_req){
            layer_list[length(layer_list)+1]<-list(
              keras::bidirectional(
                object = layer_list[[length(layer_list)]],
                layer=keras::layer_gru(
                   units=config$rec[i],
                   input_shape=list(times,features),
                   return_sequences = TRUE,
                   dropout = config$dropout,
                   recurrent_dropout = config$recurrent_dropout,
                   activation = config$rec_act_fct,
                   name=paste0("gru_",i)),
                name=paste0("bidirectional_",i)))
          } else {
            if(config$self_attention_heads>0){
              return_sequence=TRUE
            } else {
              return_sequence=FALSE
            }
            layer_list[length(layer_list)+1]<-list(
              keras::bidirectional(
                object = layer_list[[length(layer_list)]],
                layer=keras::layer_gru(
                   units=config$rec[i],
                   input_shape=list(times,features),
                   return_sequences = return_sequence,
                   dropout = config$dropout,
                   recurrent_dropout = config$recurrent_dropout,
                   activation = config$rec_act_fct,
                   name=paste0("gru",i)),
                name=paste0("bidirectional_",i)))
          }
        }
      }

      if(n_req>0){
        i=i
      } else {
        i=0
      }

      if(config$self_attention_heads>0){
        if(length(config$rec)>0){
          self_attention_units=config$rec[length(config$rec)]
        } else {
          self_attention_units=features/2
        }

        self_attention_layer<-keras::layer_multi_head_attention(
          num_heads = as.integer(self_attention_heads),
          key_dim = as.integer(self_attention_units),
          name="self_attention")

        layer_list[length(layer_list)+1]<-list(
          self_attention_layer(layer_list[[length(layer_list)]],
                               layer_list[[length(layer_list)]],
                               layer_list[[length(layer_list)]]))

        addition_layer_attention<-keras::layer_add(
          inputs=list(layer_list[[length(layer_list)]],
                      layer_list[[length(layer_list)-1]]))
        layer_list[length(layer_list)+1]<-list(addition_layer_attention)

        norm_layer_attention<-keras::layer_layer_normalization(
          object=layer_list[[length(layer_list)]],
          name = "normalizaion_layer_attention")
        layer_list[length(layer_list)+1]<-list(norm_layer_attention)

        proj_dense_1<-keras::layer_dense(
          object=layer_list[[length(layer_list)]],
          units = as.integer(self_attention_heads*2*self_attention_units),
          activation = config$act_fct,
          kernel_regularizer = keras::regularizer_l2(l=config$l2_regularizer),
          name="dense_projection_1")
        layer_list[length(layer_list)+1]<-list(proj_dense_1)

        proj_dense_2<-keras::layer_dense(
          object=layer_list[[length(layer_list)]],
          units = as.integer(2*self_attention_units),
          activation = config$act_fct,
          kernel_regularizer = keras::regularizer_l2(l=config$l2_regularizer),
          name="dense_projection_2")
        layer_list[length(layer_list)+1]<-list(proj_dense_2)

        addition_layer_projection<-keras::layer_add(
          inputs=list(layer_list[[length(layer_list)]],
                      layer_list[[length(layer_list)-2]]))
        layer_list[length(layer_list)+1]<-list(addition_layer_projection)

        norm_layer_projection<-keras::layer_layer_normalization(
          object=layer_list[[length(layer_list)]],
          name = "normalizaion_layer_projection")
        layer_list[length(layer_list)+1]<-list(norm_layer_projection)

        layer_list[length(layer_list)+1]<-list(
          keras::bidirectional(
            object = layer_list[[length(layer_list)]],
            layer=keras::layer_gru(
              units=self_attention_units,
              input_shape=list(times,features),
              return_sequences = FALSE,
              dropout = config$dropout,
              recurrent_dropout = config$recurrent_dropout,
              activation = config$rec_act_fct,
              name=paste0("gru",i+1)),
            name=paste0("bidirectional_",i+1)))
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

          if(i==(n_hidden-1) & n_hidden>=2){
            #Add Dropout_Layer
            layer_list[length(layer_list)+1]<-list(
              keras::layer_dropout(object = layer_list[[length(layer_list)]],
                                   rate = config$dropout,
                                   name="dropout"))
          }
        }
      }

      #Adding final Layer
      if(length(target_levels_order)>2){
        #Multi Class
        layer_list[length(layer_list)+1]<-list(
          keras::layer_dense(object = layer_list[[length(layer_list)]],
                             units = length(levels(targets)),
                             activation = config$act_fct_last,
                             name="output_categories"))
      } else {
        #Binary Class
        layer_list[length(layer_list)+1]<-list(
          keras::layer_dense(object = layer_list[[length(layer_list)]],
                             units = as.integer(1),
                             activation = config$act_fct_last,
                             name="output_categories"))
      }

      #Creating Model
      model<-keras::keras_model(
        inputs = model_input,
        outputs = layer_list[length(layer_list)],
        name = name)

      if(config$optimizer=="adam"){
        model$compile(
          loss = config$err_fct,
          optimizer=tf$keras$optimizers$legacy$Adam(),
          metrics=config$metric)

      } else if(config$optimizer=="rmsprop"){
         model$compile(
            loss = self$model_config$init_config$err_fct,
            optimizer=tf$keras$optimizers$legacy$RMSprop(),
            metric=self$model_config$init_config$metric)
      }

      self$bundeled_model=bundle::bundle(model)
      private$init_weights=model$get_weights()
      self$model_config$n_req=length(config$rec)
      self$model_config$n_hidden=length(config$hidden)
      self$model_config$n_self_attention_heads=config$self_attention_heads
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
    #'@param data_n_test_samples \code{int} determining the number of cross-fold
    #'samples.
    #'@param use_baseline \code{bool} \code{TRUE} if the calculation of a baseline
    #'model is requested. This option is only relevant for \code{use_bsc=TRUE} or
    #'\code{use_pbl=TRUE}. If both are \code{FALSE}, a baseline model is calculated.
    #'@param bsl_val_size \code{double} between 0 and 1, indicating the proportion of cases of each class
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
    #'@param bsc_val_size \code{double} between 0 and 1, indicating the proportion of cases of each class
    #'which should be used for the validation sample during the estimation with synthetic cases.
    #'@param bsc_add_all \code{bool} If \code{FALSE} only synthetic cases necessary to fill
    #'the gab between the class and the major class are added to the data. If \code{TRUE} all
    #'generated synthetic cases are added to the data.
    #'@param use_bpl \code{bool} \code{TRUE} if the estimation should integrate
    #'balanced pseudo-labeling. \code{FALSE} if not.
    #'@param bpl_max_steps \code{int} determining the maximum number of steps during
    #'pseudo-labeling.
    #'@param bpl_epochs_per_step \code{int} Number of training epochs within every step.
    #'@param bpl_model_reset \code{bool} If \code{TRUE}, model is re-initialized at every
    #'step.
    #'@param bpl_dynamic_inc \code{bool} If \code{TRUE}, only a specific percentage
    #'of cases is included during each step. The percentage is determined by
    #'\eqn{step/bpl_max_steps}. If \code{FALSE}, all cases are used.
    #'@param bpl_balance \code{bool} If \code{TRUE}, the same number of cases for
    #'every category/class of the pseudo-labeled data are used with training. That
    #'is, the number of cases is determined by the minor class/category.
    #'@param bpl_anchor \code{double} between 0 and 1 indicating the reference
    #'point for sorting the new cases of every label. See notes for more details.
    #'@param bpl_max \code{double} between 0 and 1, setting the maximal level of
    #'confidence for considering a case for pseudo-labeling.
    #'@param bpl_min \code{double} between 0 and 1, setting the minimal level of
    #'confidence for considering a case for pseudo-labeling.
    #'@param bpl_weight_inc \code{double} value how much the sample weights
    #'should be increased for the cases with pseudo-labels in every step.
    #'@param bpl_weight_start \code{dobule} Starting value for the weights of the
    #'unlabeled cases.
    #'@param epochs \code{int} Number of training epochs.
    #'@param batch_size \code{int} Size of batches.
    #'@param dir_checkpoint \code{string} Path to the directory where
    #'the checkpoint during training should be saved. If the directory does not
    #'exist, it is created.
    #'@param trace \code{bool} \code{TRUE}, if information about the estimation
    #'phase should be printed to the console.
    #'@param keras_trace \code{int} \code{keras_trace=0} does not cat any
    #'information about the training process from keras on the console.
    #'\code{keras_trace=1} cat a progress bar. \code{keras_trace=2} prints
    #'one line of information for every epoch.
    #'@param view_metrics \code{bool} \code{TRUE}, if metrics should be printed
    #'in the RStudio IDE.
    #'@param n_cores \code{int} Number of cores used for creating synthetic units.
    #'@details \itemize{
    #'
    #'\item{bsc_max_k: }{All values from 2 up to bsc_max_k are successively used. If
    #'the number of bsc_max_k is too high, the value is reduced to a number that
    #'allows the calculating of synthetic units.}
    #'
    #'\item{bpl_anchor: }{With the help of this value, the new cases are sorted. For
    #'this aim, the distance from the anchor is calculated and all cases are arranged
    #'into an ascending order.
    #'}
    #'}
    #'@import bundle
    #'@import keras
    #'@importFrom abind abind
    train=function(data_embeddings,
                   data_targets,
                   data_n_test_samples=5,
                   use_baseline=TRUE,
                   bsl_val_size=0.25,
                   use_bsc=TRUE,
                   bsc_methods=c("dbsmote"),
                   bsc_max_k=10,
                   bsc_val_size=0.25,
                   bsc_add_all=FALSE,
                   use_bpl=TRUE,
                   bpl_max_steps=3,
                   bpl_epochs_per_step=1,
                   bpl_dynamic_inc=FALSE,
                   bpl_balance=TRUE,
                   bpl_max=1.00,
                   bpl_anchor=1.00,
                   bpl_min=0.00,
                   bpl_weight_inc=0.02,
                   bpl_weight_start=0.00,
                   bpl_model_reset=FALSE,
                   epochs=40,
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
        cat(paste(date(),
                    "Start","\n"))
      }

      if(!("EmbeddedText" %in% class(data_embeddings))){
        stop("data_embeddings must be an object of class EmbeddedText")
      }

      embedding_model_config<-data_embeddings$get_model_info()
      for(check in names(embedding_model_config)){
        if(embedding_model_config[[check]]!=self$text_embedding_model$model[[check]]){
          stop("The TextEmbeddingModel that generated the data_embeddings is not
               the same as the TextEmbeddingModel when generating the classifier.")
        }
      }

      if(is.factor(data_targets)==FALSE){
        stop("data_targets must be a factor.")
      }
      if(is.null(names(data_targets))){
        stop("data_targets must be a named factor.")
      }

      if(bpl_anchor<bpl_min){
        stop("bpl_anchor must be at least bpl_min.")
      }
      if(bpl_anchor>bpl_max){
        stop("bpl_anchor must be lower or equal to bpl_max.")
      }

      if(data_n_test_samples<2){
        stop("data_n_test_samples must be at least 2.")
      }

      #------------------------------------------------------------------------
      if(trace==TRUE){
        cat(paste(date(),
                    "Matching Input and Target Data","\n"))
      }
      data_embeddings=data_embeddings$clone(deep=TRUE)
      viable_cases=base::intersect(x=rownames(data_embeddings$embeddings),
                                   names(data_targets))
      data_embeddings$embeddings=data_embeddings$embeddings[viable_cases,,]
      data_targets=data_targets[viable_cases]

      #Reducing to unique cases
      if(trace==TRUE){
        cat(paste(date(),
                    "Checking Uniqueness of Data","\n"))
      }
      n_init_cases=nrow(data_embeddings$embeddings)
      data_embeddings$embeddings=unique(data_embeddings$embeddings)
      n_final_cases=nrow(data_embeddings$embeddings)
      viable_cases=base::intersect(x=rownames(data_embeddings$embeddings),
                                   names(data_targets))
      data_embeddings$embeddings=data_embeddings$embeddings[viable_cases,,]
      data_targets=data_targets[viable_cases]
      if(trace==TRUE){
        cat(paste(date(),
                    "Total Cases:",n_init_cases,
                    "Unique Cases:",n_final_cases,"\n"))
      }


      #Checking Minimal Frequencies.
      if(trace==TRUE){
        cat(paste(date(),
                    "Checking Minimal Frequencies.","\n"))
      }
      freq_check<-table(data_targets)
      freq_check_eval<-freq_check<4
      if(sum(freq_check_eval)>0){
        cat<-subset(names(freq_check),
                    subset = freq_check_eval)
        stop(paste("Categories",cat,"have absolute frequencies below 4.",
             "These categories are not suitable for training.
             Please remove the corresponding categories/classes from the data
             and create a new classifier with the reduced data set."))
      }

      #Checking Number of categories.
      if(length(freq_check)<2){
        stop("After checking for uniquness data only consists of one category.
             At least to categories are neceassry for training.")
      }

      #Split data into k folds
      folds=get_folds(target=data_targets,k_folds=data_n_test_samples)

      #Create a Vector with names of categories
      categories<-names(table(data_targets))

      #Saving Training Information
      if(use_bpl==TRUE){
        n_unlabeled_data=length(data_targets)-length(na.omit(data_targets))
      } else {
        n_unlabeled_data=0
      }
      names(n_unlabeled_data)="unlabeled"
      n_labeled_data=as.vector(table(data_targets))
      names(n_labeled_data)=names(table(data_targets))
      self$last_training$data=append(n_labeled_data,n_unlabeled_data)

      #Init Object for Saving pbl_labeling
      data_pbl=matrix(data=0,
                      nrow=folds$n_folds+1,
                      ncol=bpl_max_steps)
      dimnames(data_pbl)=list(folds=c(paste0("fold_",seq(from=1,to=folds$n_folds,by=1)),
                              "final_train"),
                              bpl_steps=NULL)

      #Init Object for Saving syntehtic xases
      data_bsc_train=matrix(data = 0,
                      nrow = folds$n_folds+1,
                      ncol = length(categories))
      rownames(data_bsc_train)=c(paste0("fold_",seq(from=1,to=folds$n_folds,by=1)),
                                 "final")

      data_bsc_test=data_bsc_train

      #Initializing Objects for Saving Performance
      test_metric=array(dim=c(folds$n_folds,
                             4,
                             17),
                       dimnames = list(iterations=NULL,
                                       steps=c("Baseline",
                                               "BSC",
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

      #Initializing core objects
      embeddings_all=data_embeddings$embeddings
      targets_labeleld_all=na.omit(data_targets)

      names_unlabeled=names(subset(data_targets,is.na(data_targets)==TRUE))

      #Setting a new ID for the classifier
      self$name=paste0(self$name_root,"_id_",generate_id(16))

      for(iter in 1:folds$n_folds){
        #---------------------------------------------
        #Create a Train and Validation Sample
        names_targets_labaled_test=folds$val_sample[[iter]]
        names_targets_labeled_train=folds$train_sample[[iter]]

        targets_labeleld_train=targets_labeleld_all[names_targets_labeled_train]
        targets_labeled_test=targets_labeleld_all[names_targets_labaled_test]

        #Train baseline model or normal training--------------------------------
        if(use_baseline==TRUE |
           (use_bsc==FALSE & use_bpl==FALSE) |
           (use_bsc==FALSE & use_bpl==TRUE)){
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Training Baseline Model","\n"))
          }

          #Get Train and Test Sample
          baseline_sample<-get_stratified_train_test_split(
            targets = targets_labeleld_all[names_targets_labeled_train],
            val_size = bsl_val_size)

          names_targets_labeled_train_train=baseline_sample$train_sample
          names_targets_labeled_train_test=baseline_sample$test_sample

          targets_labeled_train_train=targets_labeleld_all[names_targets_labeled_train_train]
          targets_labeled_val=targets_labeleld_all[names_targets_labeled_train_test]

          #Train model
          private$basic_train(embedding_train=embeddings_all[names_targets_labeled_train_train,,],
                              target_train=targets_labeled_train_train,
                              embedding_test=embeddings_all[names_targets_labeled_train_test,,],
                              target_test=targets_labeled_val,
                              epochs=epochs,
                              batch_size=batch_size,
                              trace=FALSE,
                              keras_trace=keras_trace,
                              view_metrics=view_metrics,
                              reset_model=TRUE,
                              dir_checkpoint=dir_checkpoint)

          #Predict val targets
          test_predictions=self$predict(newdata = embeddings_all[names_targets_labaled_test,,],
                                        verbose = keras_trace,
                                        batch_size =batch_size)
          test_pred_cat=test_predictions$expected_category
          names(test_pred_cat)=rownames(test_predictions)
          test_pred_cat<-test_pred_cat[names(targets_labeled_test)]
          test_res=get_coder_metrics(true_values = targets_labeled_test,
                                     predicted_values = test_pred_cat)

          #Save results for baseline model
          test_metric[iter,1,]<-test_res
          #cat(paste("Baseline:",test_res["avg_alpha"]))

          if(use_bsc==TRUE | use_bpl==TRUE){
          iota_objects_start[iter]=list(iotarelr::check_new_rater(true_values = targets_labeled_test,
                                                                  assigned_values = test_pred_cat,
                                                                  free_aem = FALSE))
          iota_objects_start_free[iter]=list(iotarelr::check_new_rater(true_values = targets_labeled_test,
                                                                       assigned_values = test_pred_cat,
                                                                       free_aem = TRUE))
          }
          #cat(paste("Train",length(targets_labeled_train_train),
          #          "Validation",length(targets_labeled_val),
          #          "Test",length(targets_labeled_test)))
        }

        #Create and Train with synthetic cases----------------------------------
        if(use_bsc==TRUE){
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Applying Augmention with Balanced Synthetic Cases","\n"))
          }

          #Generating Data For Training
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Generating Synthetic Cases","\n"))
          }
          #save(embeddings_train_labeled,targets_train_labeled,bsc_methods,bsc_max_k,
          #     file="debug.RData")

          syn_cases<-get_synthetic_cases(embedding=embeddings_all[names_targets_labeled_train,,],
                                         target=targets_labeleld_train,
                                         method=bsc_methods,
                                         max_k=bsc_max_k,
                                         times = self$text_embedding_model$times,
                                         features = self$text_embedding_model$features)
          targets_synthetic_all=factor(syn_cases$syntetic_targets,
                                       levels = categories)
          embeddings_syntehtic_all=syn_cases$syntetic_embeddings

          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Generating Synthetic Cases Done","\n"))
          }

          #Combining original labeled data and synthetic data
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Selecting Synthetic Cases","\n"))
          }
          #Checking frequencies of categories and adding syn_cases
          cat_freq=table(targets_labeleld_train)
          cat_max=max(cat_freq)
          cat_delta=cat_max-cat_freq

          if(bsc_add_all==TRUE){
            cat_delta[]=Inf
          }

          cat_freq_syn=table(targets_synthetic_all)

          names_syntethic_targets_selected=NULL
          for(cat in categories){
            if(cat_delta[cat]>0){
              condition=(targets_synthetic_all==cat)
              tmp_subset=subset(x = targets_synthetic_all,
                                subset = condition)
              names_syntethic_targets_selected[cat]=list(
                sample(x=names(tmp_subset),
                       size = min(cat_delta[cat],length(tmp_subset)),
                       replace = FALSE)
              )
            }
          }
          names_syntethic_targets_selected=(unlist(names_syntethic_targets_selected,
                                                   use.names = FALSE))

          #Combining original labeled data and synthetic data
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Combining Original and Synthetic Data","\n"))
          }

          #embeddings_labeled_syn=rbind(embeddings,
          #                             syn_cases$syntetic_embeddings[syn_cases_selected,])
          #targets_labeled_syn=c(targets_train_labeled,as.factor(syn_cases$syntetic_targets[syn_cases_selected]))
          #targets_labeled_syn=targets_labeled_syn[rownames(embeddings_labeled_syn)]

          #Creating training and test sample
          bsc_train_test_split<-get_stratified_train_test_split(
            targets = c(targets_labeleld_train,targets_synthetic_all[names_syntethic_targets_selected]),
            val_size=bsc_val_size)

          #Including names of synthetic cases
          names_targets_labeled_train_train=bsc_train_test_split$train_sample
          names_targets_labeled_train_test=bsc_train_test_split$test_sample

          #Creating the final dataset for training. Please note that units
          #with NA in target are included for pseudo labeling if requested
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Creating Train Dataset","\n"))
          }

          embeddings_all_and_synthetic=abind::abind(
            embeddings_all,
            embeddings_syntehtic_all,
            along = 1)

          targets_all_and_synthetic=c(
            targets_labeleld_all,
            targets_synthetic_all)

          #Creating the final test dataset for training
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Creating Test Dataset","\n"))
          }

          #Save freq of every labeled original and synthetic case
          data_bsc_train[iter,]<-table(targets_all_and_synthetic[names_targets_labeled_train_train])
          data_bsc_test[iter,]<-table(targets_all_and_synthetic[names_targets_labeled_train_test])

          #Train model
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Start Training","\n"))
          }
          private$basic_train(embedding_train=embeddings_all_and_synthetic[names_targets_labeled_train_train,,],
                              target_train=targets_all_and_synthetic[names_targets_labeled_train_train],
                              embedding_test=embeddings_all_and_synthetic[names_targets_labeled_train_test,,],
                              target_test=targets_all_and_synthetic[names_targets_labeled_train_test],
                              epochs=epochs,
                              batch_size=batch_size,
                              trace=FALSE,
                              keras_trace=keras_trace,
                              view_metrics=view_metrics,
                              reset_model=TRUE,
                              dir_checkpoint=dir_checkpoint)
          #Predict val targets
          test_predictions=self$predict(newdata = embeddings_all[names_targets_labaled_test,,],
                                        verbose = keras_trace,
                                        batch_size =batch_size)
          test_pred_cat=test_predictions$expected_category
          names(test_pred_cat)=rownames(test_predictions)
          test_pred_cat=test_pred_cat[names(targets_labeled_test)]
          test_res=get_coder_metrics(true_values = targets_labeled_test,
                                     predicted_values = test_pred_cat)
          #Save results for baseline model
          test_metric[iter,2,]<-test_res
          #cat(paste("BSC",test_res["avg_alpha"]))

          #cat(paste("Train",length(names_targets_labeled_train_train),
          #            "Validation",length(names_targets_labeled_train_test),
          #            "Test",length(targets_labeled_test)))

        }

        #Applying Pseudo Labeling-----------------------------------------------
        if(use_bpl==TRUE){
          categories<-names(table(data_targets))
          if(trace==TRUE){
            cat(paste(date(),
                        "Iter:",iter,"from",folds$n_folds,
                        "Applying Pseudo Labeling","\n"))
          }

          #Defining the basic parameter for while
          step=1
          val_avg_alpha=0

          if(use_bsc==TRUE){
            pseudo_label_embeddings_all=embeddings_all_and_synthetic
            pseudo_label_targets_labeled_train=targets_all_and_synthetic[names_targets_labeled_train_train]
            pseudo_label_targets_labeled_test=targets_labeled_test
            pseudo_label_targets_labeled_val=targets_all_and_synthetic[names_targets_labeled_train_test]
          } else {
            pseudo_label_embeddings_all=embeddings_all
            pseudo_label_targets_labeled_train=targets_labeled_train_train
            pseudo_label_targets_labeled_val=targets_labeled_val
            pseudo_label_targets_labeled_test=targets_labeled_test
          }
          weights_cases_list=NULL
          weights_cases_list[1]=list(names_targets_labeled_train_train)

          added_cases_train=100


          #Start of While-------------------------------------------------------
          while(step <=bpl_max_steps & added_cases_train>0){

            if(bpl_dynamic_inc==TRUE){
              bpl_inc_ratio=step/bpl_max_steps
            } else {
              bpl_inc_ratio=1
            }

            #cat(paste("Train",length(pseudo_label_targets_labeled_train),
            #            "Validation",length(pseudo_label_targets_labeled_val),
            #            "Test",length(pseudo_label_targets_labeled_test),
            #            "Unlabeled",length(names_unlabeled)))


            #Estimate the labels for the remaining data
            est_remaining_data=self$predict(newdata=embeddings_all[names_unlabeled,,],
                                            verbose = keras_trace,
                                            batch_size =batch_size)

            #Create Matrix for saving the results
            new_categories<-matrix(nrow= nrow(est_remaining_data),
                                   ncol=2)
            rownames(new_categories)=rownames(est_remaining_data)
            colnames(new_categories)=c("cat","prob")

            #Gather information for every case. That is the category with the
            #highest probability and save both
            for(i in 1:nrow(est_remaining_data)){
              tmp_est_prob=est_remaining_data[i,1:(ncol(est_remaining_data)-1)]
              new_categories[i,1]=categories[which.max(tmp_est_prob)]
              new_categories[i,2]=max(tmp_est_prob)
            }
            new_categories<-as.data.frame(new_categories)

            #Transforming the probabilities to a information index
            new_categories[,2]=abs(bpl_anchor-(as.numeric(new_categories[,2])-1/length(categories))/(1-1/length(categories)))
            new_categories=as.data.frame(new_categories)

            #Reducing the new categories to the desired range
            condition=(new_categories[,2]>=bpl_min & new_categories[,2]<=bpl_max)
            new_categories=subset(new_categories,
                                  condition)

            #calculate the minimal freq of the available categories
            new_cat_freq=table(new_categories$cat)
            min_new_freq=max(floor(min(new_cat_freq)*bpl_inc_ratio),1)
            new_categories_names=names(new_cat_freq)

            #Order cases with increasing distance from maximal information
            names_final_new_categories=NULL
            for(cat in new_categories_names){
              condition=(new_categories[,1]==cat)
              tmp=subset(x=new_categories,
                         subset=condition)
              tmp=tmp[order(tmp$prob,decreasing = FALSE),]
              if(bpl_balance==TRUE){
                #Chose always the same number of new cases to ensure the balance
                #of all categories
                names_final_new_categories=append(x=names_final_new_categories,
                                            values=rownames(tmp)[1:min_new_freq])
              } else {
                n_inc=max(floor(nrow(tmp)*bpl_inc_ratio),1)
                names_final_new_categories=append(x=names_final_new_categories,
                                            values=rownames(tmp)[1:n_inc])
              }
            }

            new_categories<-new_categories[names_final_new_categories,]

            targets_pseudo_labeled<-new_categories[names_final_new_categories,1]
            targets_pseudo_labeled<-factor(targets_pseudo_labeled,
                                         levels=categories)
            names(targets_pseudo_labeled)<-names_final_new_categories

            targets_labeled_and_pseudo<-c(
              pseudo_label_targets_labeled_train,
              targets_pseudo_labeled)

          #Counting new cases
          added_cases_train=length(targets_pseudo_labeled)
          data_pbl[iter,step]=added_cases_train

          #Calculating the weights for the new cases
          weights_cases_list[2]=list(names(targets_pseudo_labeled))
          tmp_weights=NULL
          tmp_weights_names=NULL
          for(i in 1:length(weights_cases_list)){
            if(i==1){
              w=1
            } else {
              w=bpl_weight_start+ bpl_weight_inc*step
            }
            tmp_weights=append(x=tmp_weights,
                                 values = rep(
                                   x=w,
                                   times = length(weights_cases_list[[i]]))
                               )
            tmp_weights_names=append(x=tmp_weights_names,
                                     values = weights_cases_list[[i]])
            }
            names(tmp_weights)=tmp_weights_names
            sample_weights=tmp_weights[names(targets_labeled_and_pseudo)]

           #Train model
            if(bpl_epochs_per_step>1){
              use_callback=TRUE
            } else {
              use_callback=FALSE
            }
                private$basic_train(embedding_train=pseudo_label_embeddings_all[names(targets_labeled_and_pseudo),,],
                                    target_train=targets_labeled_and_pseudo,
                                    embedding_test=pseudo_label_embeddings_all[names(pseudo_label_targets_labeled_val),,],
                                    target_test=pseudo_label_targets_labeled_val,
                                    epochs = bpl_epochs_per_step,
                                    use_callback=use_callback,
                                    batch_size=batch_size,
                                    trace=FALSE,
                                    keras_trace=keras_trace,
                                    view_metrics=view_metrics,
                                    reset_model=bpl_model_reset,
                                    dir_checkpoint=dir_checkpoint,
                                    sample_weights=sample_weights)

              #predict val targets
              val_predictions=self$predict(newdata=pseudo_label_embeddings_all[names(pseudo_label_targets_labeled_val),,],
                                            verbose = keras_trace,
                                            batch_size =batch_size)
              val_pred_cat=val_predictions$expected_category
              names(val_pred_cat)=rownames(val_predictions)
              val_pred_cat=val_pred_cat[names(pseudo_label_targets_labeled_val)]
              val_res=get_coder_metrics(true_values = pseudo_label_targets_labeled_val,
                                        predicted_values = val_pred_cat)

              #cat(table(data.frame(pred=val_predictions$expected_category,
              #              true=pseudo_label_targets_labeled_val)))

              #Predict test targets
              test_predictions=self$predict(newdata = embeddings_all[names(pseudo_label_targets_labeled_test),,],
                                            verbose = keras_trace,
                                            batch_size =batch_size)
              test_pred_cat=test_predictions$expected_category
              names(test_pred_cat)=rownames(test_predictions)
              test_pred_cat<-test_pred_cat[names(pseudo_label_targets_labeled_test)]
              test_res=get_coder_metrics(true_values = pseudo_label_targets_labeled_test,
                                         predicted_values = test_pred_cat)

              if(val_avg_alpha<val_res["avg_alpha"]){
                val_avg_alpha=val_res["avg_alpha"]
                new_best_model=self$bundeled_model
                best_val_metric=val_res
                best_test_metric=test_res
              }
              #cat(paste("Validation:",val_res["avg_alpha"]))
              #cat(paste("Test:",test_res["avg_alpha"]))

              #cat(paste("Train",length(targets_labeled_and_pseudo),
              #            "Validation",length(pseudo_label_targets_labeled_val),
              #            "Test",length(pseudo_label_targets_labeled_test),
              #            "Unlabeled",length(targets_pseudo_labeled)))

              if(trace==TRUE){
                cat(paste(date(),
                            "Epoch",step,"Done","\n"))
              }

            #increase step
            step=step+1
          }

          self$bundeled_model=new_best_model
          test_metric[iter,"BPL",]<-best_test_metric
          test_res<-best_test_metric
        }
        test_metric[iter,"Final",]<-test_res


        test_predictions=self$predict(newdata = embeddings_all[names_targets_labaled_test,,],
                                      verbose = keras_trace,
                                      batch_size =batch_size)
        test_pred_cat=test_predictions$expected_category
        names(test_pred_cat)=rownames(test_predictions)
        test_pred_cat<-test_pred_cat[names(targets_labeled_test)]
        test_res=get_coder_metrics(true_values = targets_labeled_test,
                                   predicted_values = test_pred_cat)
        iota_objects_end[iter]=list(iotarelr::check_new_rater(true_values = targets_labeled_test,
                                                                assigned_values = test_pred_cat,
                                                                free_aem = FALSE))
        iota_objects_end_free[iter]=list(iotarelr::check_new_rater(true_values = targets_labeled_test,
                                                                     assigned_values = test_pred_cat,
                                                                     free_aem = TRUE))

        #----------------------------------------------
      }

      #Insert Final Training here and Savings here
      self$reliability$test_metric=test_metric
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
      if((use_bsc==FALSE & use_bpl==FALSE) |
         #(use_baseline=TRUE) |
        (use_bsc==FALSE & use_bpl==TRUE)){
        embeddings_train=data_embeddings$clone(deep=TRUE)
        targets_train=data_targets
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training of Baseline Model","\n"))
        }
        #Get Train and Test Sample
        baseline_sample<-get_stratified_train_test_split(
          targets = targets_labeleld_all,
          val_size = bsl_val_size)

        names_targets_labeled_train_train=baseline_sample$train_sample
        names_targets_labeled_train_test=baseline_sample$test_sample

        targets_labeled_train_train=targets_labeleld_all[names_targets_labeled_train_train]
        targets_labeled_val=targets_labeleld_all[names_targets_labeled_train_test]

        #Train model
        private$basic_train(embedding_train=embeddings_all[names_targets_labeled_train_train,,],
                            target_train=targets_labeled_train_train,
                            embedding_test=embeddings_all[names_targets_labeled_train_test,,],
                            target_test=targets_labeled_val,
                            epochs=epochs,
                            batch_size=batch_size,
                            trace=FALSE,
                            keras_trace=keras_trace,
                            view_metrics=view_metrics,
                            reset_model=TRUE,
                            dir_checkpoint=dir_checkpoint)

        #cat(paste("Train",length(names_targets_labeled_train_train),
        #            "Validation",length(names_targets_labeled_train_test)))

      }

      #Final Training with BSC--------------------------------------------------

      if(use_bsc==TRUE){
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Applying Augmention with Balanced Synthetic Cases"),"\n")
        }
        #Generating Data For Training
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Generating Synthetic Cases"),"\n")
        }
        #save(embeddings_train_labeled,targets_train_labeled,bsc_methods,bsc_max_k,
        #     file="debug.RData")
        syn_cases<-get_synthetic_cases(embedding=embeddings_all[names(targets_labeleld_all),,],
                                       target=targets_labeleld_all,
                                       method=bsc_methods,
                                       max_k=bsc_max_k,
                                       times = self$text_embedding_model$times,
                                       features = self$text_embedding_model$features)
        targets_synthetic_all=factor(syn_cases$syntetic_targets,
                                     levels = categories)
        embeddings_syntehtic_all=syn_cases$syntetic_embeddings

        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Generating Synthetic Cases Done"),"\n")
        }

        #Combining original labeled data and synthetic data
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Selecting Synthetic Cases"),"\n")
        }
        #Checking frequencies of categories and adding syn_cases
        cat_freq=table(targets_labeleld_all)
        cat_max=max(cat_freq)
        cat_delta=cat_max-cat_freq

        if(bsc_add_all==TRUE){
          cat_delta[]=Inf
        }

        cat_freq_syn=table(targets_synthetic_all)

        names_syntethic_targets_selected=NULL
        for(cat in categories){
          if(cat_delta[cat]>0){
            condition=(targets_synthetic_all==cat)
            tmp_subset=subset(x = targets_synthetic_all,
                              subset = condition)
            names_syntethic_targets_selected[cat]=list(
              sample(x=names(tmp_subset),
                     size = min(cat_delta[cat],length(tmp_subset)),
                     replace = FALSE)
            )
          }
        }
        names_syntethic_targets_selected=(unlist(names_syntethic_targets_selected,
                                                 use.names = FALSE))

        #Combining original labeled data and synthetic data
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Combining Original and Synthetic Data"),"\n")
        }

        #Creating training and test sample
        bsc_train_test_split<-get_stratified_train_test_split(
          targets = c(targets_labeleld_all,targets_synthetic_all[names_syntethic_targets_selected]),
          val_size=bsc_val_size)

        #Including names of synthetic cases
        names_targets_labeled_train_train=bsc_train_test_split$train_sample
        names_targets_labeled_train_test=bsc_train_test_split$test_sample

        #Creating the final dataset for training. Please note that units
        #with NA in target are included for pseudo labeling if requested
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Creating Train Dataset","\n"))
        }

        embeddings_all_and_synthetic=abind::abind(
          embeddings_all,
          embeddings_syntehtic_all,
          along = 1)

        targets_all_and_synthetic=c(
          targets_labeleld_all,
          targets_synthetic_all)

        #Creating the final test dataset for training
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Creating Test Dataset","\n"))
        }

        #Save freq of every labeled original and synthetic case
        data_bsc_train["final",]<-table(targets_all_and_synthetic[names_targets_labeled_train_train])
        data_bsc_test["final",]<-table(targets_all_and_synthetic[names_targets_labeled_train_test])

        #Train model
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Start Training","\n"))
        }
        private$basic_train(embedding_train=embeddings_all_and_synthetic[names_targets_labeled_train_train,,],
                            target_train=targets_all_and_synthetic[names_targets_labeled_train_train],
                            embedding_test=embeddings_all_and_synthetic[names_targets_labeled_train_test,,],
                            target_test=targets_all_and_synthetic[names_targets_labeled_train_test],
                            epochs=epochs,
                            batch_size=batch_size,
                            trace=FALSE,
                            keras_trace=keras_trace,
                            view_metrics=view_metrics,
                            reset_model=TRUE,
                            dir_checkpoint=dir_checkpoint)
        #cat(paste("Train",length(names_targets_labeled_train_train),
        #            "Validation",length(names_targets_labeled_train_test)))
      }

        #Applying Pseudo Labeling-----------------------------------------------
      if(use_bpl==TRUE){
        categories<-names(table(data_targets))
        if(trace==TRUE){
          cat(paste(date(),
                      "Final Training",
                      "Applying Pseudo Labeling","\n"))
        }

        #Defining the basic parameter for while
        step=1
        val_avg_alpha=0

        if(use_bsc==TRUE){
          pseudo_label_embeddings_all=embeddings_all_and_synthetic
          pseudo_label_targets_labeled_train=targets_all_and_synthetic[names_targets_labeled_train_train]
          pseudo_label_targets_labeled_test=targets_labeled_test
          pseudo_label_targets_labeled_val=targets_all_and_synthetic[names_targets_labeled_train_test]
        } else {
          pseudo_label_embeddings_all=embeddings_all
          pseudo_label_targets_labeled_train=targets_labeled_train_train
          pseudo_label_targets_labeled_val=targets_labeled_val
          pseudo_label_targets_labeled_test=targets_labeled_test
        }
        weights_cases_list=NULL
        weights_cases_list[1]=list(names_targets_labeled_train_train)

        added_cases_train=100


        #Start of While-------------------------------------------------------
        while(step <=bpl_max_steps & added_cases_train>0){

          #cat(paste("Train",length(pseudo_label_targets_labeled_train),
          #            "Validation",length(pseudo_label_targets_labeled_val),
          #            "Unlabeled",length(names_unlabeled)))

          if(bpl_dynamic_inc==TRUE){
            bpl_inc_ratio=step/bpl_max_steps
          } else {
            bpl_inc_ratio=1
          }

          #Estimate the labels for the remaining data
          est_remaining_data=self$predict(newdata=embeddings_all[names_unlabeled,,],
                                          verbose = keras_trace,
                                          batch_size =batch_size)

          #Create Matrix for saving the results
          new_categories<-matrix(nrow= nrow(est_remaining_data),
                                 ncol=2)
          rownames(new_categories)=rownames(est_remaining_data)
          colnames(new_categories)=c("cat","prob")

          #Gather information for every case. That is the category with the
          #highest probability and save both
          for(i in 1:nrow(est_remaining_data)){
            tmp_est_prob=est_remaining_data[i,1:(ncol(est_remaining_data)-1)]
            new_categories[i,1]=categories[which.max(tmp_est_prob)]
            new_categories[i,2]=max(tmp_est_prob)
          }
          new_categories<-as.data.frame(new_categories)

          #Transforming the probabilities to a information index
          new_categories[,2]=abs(bpl_anchor-(as.numeric(new_categories[,2])-1/length(categories))/(1-1/length(categories)))
          new_categories=as.data.frame(new_categories)

          #Reducing the new categories to the desired range
          condition=(new_categories[,2]>=bpl_min & new_categories[,2]<=bpl_max)
          new_categories=subset(new_categories,
                                condition)

          #calculate the minimal freq of the available categories
          new_cat_freq=table(new_categories$cat)
          min_new_freq=max(floor(min(new_cat_freq)*bpl_inc_ratio),1)
          new_categories_names=names(new_cat_freq)

          #Order cases with increasing distance from maximal information
          names_final_new_categories=NULL
          for(cat in new_categories_names){
            condition=(new_categories[,1]==cat)
            tmp=subset(x=new_categories,
                       subset=condition)
            tmp=tmp[order(tmp$prob,decreasing = FALSE),]
            if(bpl_balance==TRUE){
              #Chose always the same number of new cases to ensure the balance
              #of all categories
              names_final_new_categories=append(x=names_final_new_categories,
                                                values=rownames(tmp)[1:min_new_freq])
            } else {
              n_inc=max(floor(nrow(tmp)*bpl_inc_ratio),1)
              names_final_new_categories=append(x=names_final_new_categories,
                                                values=rownames(tmp)[1:n_inc])
            }
          }

          new_categories<-new_categories[names_final_new_categories,]


          targets_pseudo_labeled<-new_categories[names_final_new_categories,1]
          targets_pseudo_labeled<-factor(targets_pseudo_labeled,
                                         levels=categories)
          names(targets_pseudo_labeled)<-names_final_new_categories

          targets_labeled_and_pseudo<-c(
            pseudo_label_targets_labeled_train,
            targets_pseudo_labeled)

          #Counting new cases
          added_cases_train=length(targets_pseudo_labeled)
          data_pbl["final_train",step]=added_cases_train

          #Calculating the weights for the new cases
          weights_cases_list[2]=list(names(targets_pseudo_labeled))
          tmp_weights=NULL
          tmp_weights_names=NULL
          for(i in 1:length(weights_cases_list)){
            if(i==1){
              w=1
            } else {
              bpl_weight_start+ bpl_weight_inc*step
            }
            tmp_weights=append(x=tmp_weights,
                               values = rep(
                                 x=w,
                                 times = length(weights_cases_list[[i]]))
            )
            tmp_weights_names=append(x=tmp_weights_names,
                                     values = weights_cases_list[[i]])
          }
          names(tmp_weights)=tmp_weights_names
          sample_weights=tmp_weights[names(targets_labeled_and_pseudo)]

          #Train model
          if(bpl_epochs_per_step>1){
            use_callback=TRUE
          } else {
            use_callback=FALSE
          }
          private$basic_train(embedding_train=pseudo_label_embeddings_all[names(targets_labeled_and_pseudo),,],
                              target_train=targets_labeled_and_pseudo,
                              embedding_test=pseudo_label_embeddings_all[names(pseudo_label_targets_labeled_val),,],
                              target_test=pseudo_label_targets_labeled_val,
                              epochs = bpl_epochs_per_step,
                              use_callback=use_callback,
                              batch_size=batch_size,
                              trace=FALSE,
                              keras_trace=keras_trace,
                              view_metrics=view_metrics,
                              reset_model=bpl_model_reset,
                              dir_checkpoint=dir_checkpoint,
                              sample_weights=sample_weights)
          #cat(paste("Train",length(targets_labeled_and_pseudo),
          #            "Validation",length(pseudo_label_targets_labeled_val),
          #            "Unlabeled",length(targets_pseudo_labeled)))

          #predict val targets
          val_predictions=self$predict(newdata=pseudo_label_embeddings_all[names(pseudo_label_targets_labeled_val),,],
                                       verbose = keras_trace,
                                       batch_size =batch_size)
          val_pred_cat=val_predictions$expected_category
          names(val_pred_cat)=rownames(val_predictions)
          val_pred_cat=val_pred_cat[names(pseudo_label_targets_labeled_val)]
          val_res=get_coder_metrics(true_values = pseudo_label_targets_labeled_val,
                                    predicted_values = val_pred_cat)

          if(val_avg_alpha<val_res["avg_alpha"]){
            val_avg_alpha=val_res["avg_alpha"]
            new_best_model=self$bundeled_model
            best_val_metric=val_res
          }
          #cat(paste("Validation:",val_res["avg_alpha"]))

          if(trace==TRUE){
            cat(paste(date(),
                        "Epoch",step,"Done","\n"))
          }

          #increase step
          step=step+1
        }

        self$bundeled_model=new_best_model
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

      test_metric_mean=matrix(data=0,
                             nrow = nrow(test_metric[1,,]),
                             ncol = ncol(test_metric[1,,]))
      rownames(test_metric_mean)=rownames(test_metric[1,,])
      colnames(test_metric_mean)=colnames(test_metric[1,,])

      n_mean=vector(length = nrow(test_metric[1,,]))
      n_mean[]=folds$n_folds

      for(i in 1:folds$n_folds){
          tmp_val_metric=test_metric[i,,]
          for(j in 1:nrow(tmp_val_metric)){
            if(sum(is.na(tmp_val_metric[j,]))!=length(tmp_val_metric[j,])){
              test_metric_mean[j,]=test_metric_mean[j,]+tmp_val_metric[j,]
            } else {
              n_mean[j]=n_mean[j]-1
            }
          }
        }

      test_metric_mean=test_metric_mean/n_mean
      test_metric_mean[is.nan(test_metric_mean)]=NA
      self$reliability$test_metric_mean=test_metric_mean

      self$last_training$learning_time=as.numeric(difftime(Sys.time(),start_time,
                                                           units="mins"))

      #Unload Cluster for Parallel Execution
      if(use_bsc==TRUE){
        parallel::stopCluster(cl)
      }

      if(trace==TRUE){
        cat(paste(date(),
                    "Training Complete","\n"))
      }
    },

    #-------------------------------------------------------------------------
    #'@description Method for predicting new data with a trained neural net.
    #'@param newdata Object of class \code{TextEmbeddingModel} or
    #'\code{data.frame} for which predictions should be made.
    #'@param verbose \code{int} \code{verbose=0} does not cat any
    #'information about the training process from keras on the console.
    #'\code{verbose=1} prints a progress bar. \code{verbose=2} prints
    #'one line of information for every epoch.
    #'@param batch_size \code{int} Size of batches.
    #'@return Returns a \code{data.frame} containing the predictions and
    #'the probabilities of the different labels for each case.
    predict=function(newdata,
                     batch_size=32,
                     verbose=1){
      #Checking input data
      #if(methods::isClass(where=newdata,"data.frame")==FALSE){
      #  stop("newdata mus be a data frame")
      #}
      if("EmbeddedText" %in% class(newdata)){
        embedding_model_config<-newdata$get_model_info()
        for(check in names(embedding_model_config)){
          if(embedding_model_config[[check]]!=self$text_embedding_model$model[[check]]){
            stop("The TextEmbeddingModel that generated the newdata is not
               the same as the TextEmbeddingModel when generating the classifier.")
          }
        }
        real_newdata=newdata$embeddings
      } else {
        real_newdata=newdata
      }

      #Ensuring the correct order of the variables for prediction
      real_newdata<-real_newdata[,,self$model_config$input_variables,drop=FALSE]
      current_row_names=rownames(real_newdata)
      if(self$model_config$n_req==0 &
         self$model_config$n_self_attention_heads==0){
        real_newdata=array_to_matrix(real_newdata)
      }

      if(length(self$model_config$target_levels)>2){
        #Multi Class
        #Predicting target variable
        model<-bundle::unbundle(self$bundeled_model)
        #predictions_prob<-predict(object = model,
        #                          x = real_newdata,
        #                          batch_size = as.integer(batch_size),
        #                          verbose=as.integer(verbose))
        predictions_prob<-model$predict(
                                  x = np$array(real_newdata),
                                  batch_size = as.integer(batch_size),
                                  verbose=as.integer(verbose))
        predictions<-predictions_prob %>% keras::k_argmax()
      } else {
        model<-bundle::unbundle(self$bundeled_model)
        predictions_prob<-model$predict(
                                  x = np$array(real_newdata),
                                  batch_size = as.integer(batch_size),
                                  verbose=as.integer(verbose))

        #Add Column for the second characteristic
        predictions=vector(length = length(predictions_prob))
        predictions_binary_prob<-matrix(ncol=2,
                                        nrow=length(predictions_prob))

        for(i in 1:length(predictions_prob)){
          if(predictions_prob[i]>=0.5){
            predictions_binary_prob[i,1]=1-predictions_prob[i]
            predictions_binary_prob[i,2]=predictions_prob[i]
            predictions[i]=1
          } else {
            predictions_binary_prob[i,1]=1-predictions_prob[i]
            predictions_binary_prob[i,2]=predictions_prob[i]
            predictions[i]=0
          }
        }
        predictions_prob<-predictions_binary_prob
      }

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
    #'@param authors List of authors.
    #'@param citation Free text citation.
    #'@param url URL of a corresponding homepage.
    set_publication_info=function(authors ,
                                  citation,
                                  url=NULL){

      private$publication_info$developed_by$authors<-authors
      private$publication_info$developed_by$citation<-citation
      private$publication_info$developed_by$url<-url

    },
    #--------------------------------------------------------------------------
    #'@description Method for requesting the bibliographic information of the classifier.
    #'@return \code{list} with all saved bibliographic information.
    get_publication_info=function(){
      return(private$publication_info)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting the license of the classifier.
    #'@param license \code{string} containing the abbreviation of the license or
    #'the license text.
    set_license=function(license="CC BY-NC-SA"){
      private$model_license<-license
    },
    #'@description Method for setting the license of the classifier.
    #'@param license \code{string} containing the abbreviation of the license or
    #'the license text.
    get_license=function(){
      return(private$model_license)
    },
    #--------------------------------------------------------------------------
    #'@description Method for setting a description of the classifier.
    #'@param eng \code{string} A text describing the training of the learner,
    #'its theoretical and empirical background, and the different output labels
    #'in English.
    #'@param native \code{string} A text describing the training of the learner,
    #'its theoretical and empirical background, and the different output labels
    #'in the native language of the classifier.
    #'@param abstract_eng \code{string} A text providing a summary of the description
    #'in English.
    #'@param abstract_native \code{string} A text providing a summary of the description
    #'in the native language of the classifier.
    #'@param keywords_eng \code{vector} of keyword in English.
    #'@param keywords_native \code{vector} of keyword in the native language of the classifier.
    set_model_description=function(eng=NULL,
                                   native=NULL,
                                   abstract_eng=NULL,
                                   abstract_native=NULL,
                                   keywords_eng=NULL,
                                   keywords_native=NULL){
      if(!is.null(eng)){
        private$model_description$eng=eng
      }
      if(!is.null(native)){
        private$model_description$native=native
      }

      if(!is.null(abstract_eng)){
        private$model_description$abstract_eng=abstract_eng
      }
      if(!is.null(abstract_native)){
        private$model_description$abstract_native=abstract_native
      }

      if(!is.null(keywords_eng)){
        private$model_description$keywords_eng=keywords_eng
      }
      if(!is.null(keywords_native)){
        private$model_description$keywords_native=keywords_native
      }

    },
    #'@description Method for requesting the model description.
    #'@return \code{list} with the description of the classifier in English
    #'and the native language.
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
        authors =NULL,
        citation=NULL,
        url=NULL
      )
    ),
    model_description=list(
      eng=NULL,
      native=NULL,
      abstract_eng=NULL,
      abstract_native=NULL,
      keywords_eng=NULL,
      keywords_native=NULL
    ),
    model_license=NULL,
    #Training Process----------------------------------------------------------
    init_weights=NULL,
    #--------------------------------------------------------------------------
    basic_train=function(embedding_train,
                         target_train,
                         embedding_test,
                         target_test,
                         sample_weights=NULL,
                         reset_model=FALSE,
                         use_callback=TRUE,
                         epochs=100,
                         batch_size=32,
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

      #Clear session to provide enough resources for computations
      tf$keras$backend$clear_session()

      #load names and order of input variables and train_target levels
      variable_name_order<-self$model_config$input_variables
      target_levels_order<-self$model_config$target_levels

      #Ensuring the same encoding
      target_train<-factor(as.character(target_train),
                           levels = target_levels_order)
      target_test<-factor(as.character(target_test),
                          levels = target_levels_order)

      data_embedding_train<-data_embedding_train[,,variable_name_order]
      data_embedding_test<-data_embedding_test[,,variable_name_order]

      #variable_name_order<-colnames(data_embedding_train)
      #target_levels_order<-levels(target_train)

      #Transforming train_target for the use in keras.
      #That is switching characters to numeric
      target_train_transformed<-as.numeric(target_train)-1
      target_test_transformed<-as.numeric(target_test)-1

      #Convert Input data if the network cannot process sequential data
      if(self$model_config$n_req==0 &
         self$model_config$n_self_attention_heads==0){
        input_embeddings_train= array_to_matrix(data_embedding_train)
        input_embeddings_test=array_to_matrix(data_embedding_test)
      } else {
        input_embeddings_train=data_embedding_train
        input_embeddings_test=data_embedding_test
      }

      n_categories=as.integer(length(levels(target_train)))

      if(length(target_levels_order)>2){
        #Multi Class
        output_categories_train=keras::to_categorical(y=target_train_transformed,
                                                      num_classes=n_categories)
        output_categories_test=keras::to_categorical(y=target_test_transformed,
                                                     num_classes=n_categories)
      } else {
        #Binary Classification
        output_categories_train=target_train_transformed
        output_categories_test=target_test_transformed
      }

      model<-bundle::unbundle(self$bundeled_model)

      if(reset_model==TRUE){
        model$set_weights(private$init_weights)
        #cat("Model reseted")
      }

      tf<-reticulate::import("tensorflow")

      if(self$model_config$init_config$optimizer=="adam"){
        model$compile(
          loss = self$model_config$init_config$err_fct,
          optimizer=tf$keras$optimizers$legacy$Adam(),
          metrics=self$model_config$init_config$metric)
      } else if (self$model_config$init_config$optimizer=="rmsprop"){
        model$compile(
          loss = self$model_config$init_config$err_fct,
          optimizer=tf$keras$optimizers$legacy$RMSprop(),
          metrics=self$model_config$init_config$metric)
      }

      if(dir.exists(paste0(dir_checkpoint,"/checkpoints"))==FALSE){
        cat(paste(date(),"Creating Checkpoint Directory"))
        dir.create(paste0(dir_checkpoint,"/checkpoints"))
      }

      if(use_callback==TRUE){
        callback=tf$keras$callbacks$ModelCheckpoint(
          filepath = paste0(dir_checkpoint,"/checkpoints/"),
          monitor = paste0("val_",self$model_config$init_config$metric),
          verbose = as.integer(min(keras_trace,1)),
          mode = "auto",
          save_best_only = TRUE,
          save_weights_only = TRUE)
      } else {
        callback=reticulate::py_none()
      }

      if(!is.null(sample_weights)){
        sample_weights=np$array(sample_weights)
      }

        train_results<-model$fit(
          verbose=as.integer(keras_trace),
          x=np$array(input_embeddings_train),
          y=np$array(output_categories_train),
          #validation_data=list(x_val=input_embeddings_test,
          #                     y_val=output_categories_test),
          validation_data=reticulate::tuple(list(x_val=np$array(input_embeddings_test),
                               y_val=np$array(output_categories_test))),
          epochs = as.integer(epochs),
          batch_size = as.integer(batch_size),
          callbacks = callback,
          #view_metrics=view_metrics,
          sample_weight=sample_weights)


      if(use_callback==TRUE){
        #cat(paste(date(),"Load Weights From Best Checkpoint"))
        model$load_weights(paste0(dir_checkpoint,"/checkpoints"))
      }

      self$bundeled_model=bundle::bundle(model)
      self$model_config$input_variables<-variable_name_order
      self$model_config$target_levels<-target_levels_order
      self$last_training$history<-train_results
    }
  )
)
