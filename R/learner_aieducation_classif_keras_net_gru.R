#' @title Classification Keras Neural Net Gru Learner
#' @author FBerding
#' @name mlr_learners_classif.keras_net_gru
#'
#' @description
#' FIXME: BRIEF DESCRIPTION OF THE LEARNER.
#' Calls [aieducation::keras_net_gru()] from FIXME: (CRAN VS NO CRAN): \CRANpkg{aifeducation} | 'aifeducation'.
#'
#' @section Initial parameter values:
#' FIXME: DEVIATIONS FROM UPSTREAM PARAMETERS. DELETE IF NOT APPLICABLE.
#'
#' @section Custom mlr3 defaults:
#' FIXME: DEVIATIONS FROM UPSTREAM DEFAULTS. DELETE IF NOT APPLICABLE.
#'
#' @section Installation:
#' FIXME: CUSTOM INSTALLATION INSTRUCTIONS. DELETE IF NOT APPLICABLE.
#'
#' @templateVar id classif.keras_net_gru
#'
#' @references
#' `r format_bib(FIXME: ONE OR MORE REFERENCES FROM bibentries.R)`
#'
#' @export
LearnerClassifkeras_net_gru = R6::R6Class("LearnerClassifkeras_net_gru",
  inherit = mlr3::LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      # FIXME: MANUALLY ADD PARAMETERS BELOW AND THEN DELETE THIS LINE
      param_set = paradox::ps(
        times =  paradox::p_int(lower = 1L, tags = "train"),
        features =  paradox::p_int(lower = 1L, tags = "train"),
        n_hidden =  paradox::p_int(lower = 1L, default = 2L,tags = "train"),
        n_hidden_size =  paradox::p_int(lower = 1L,default = 3L,tags = "train"),
        n_gru =  paradox::p_int(lower = 1L,default = 1L, tags="train"),
        n_gru_size =  paradox::p_int(lower = 1, default = 10, tags = "train"),
        dropout =  paradox::p_dbl(lower = 0,default = 0.05, upper = 0.5),
        recurrent_dropout =  paradox::p_dbl(lower = 0,default = 0.05, upper = 0.5),
        #optimizer=keras::optimizer_rmsprop(),
        act_fct= paradox::p_fct(levels=c("relu","sigmoid","tanh"), default = "relu", tags="train"),
        act_fct_last= paradox::p_fct(levels=c("softmax","relu","sigmoid","tanh"), default = "relu", tags="train"),
        err_fct= paradox::p_fct(levels=c("categorical_crossentropy","mse"), default = "categorical_crossentropy",tags = "train"),
        epochs =  paradox::p_int(lower=1L, default= 30L, tags="train"),
        batch_size =  paradox::p_int(lower=1L,default = 100L, tags = "train"),
        rel_tolerance= paradox::p_dbl(lower=0.0, default = 1e-8, upper=1),
        view_metrics= paradox::p_lgl(default = FALSE,tags =c("train","auxiliary")),
        validation_split= paradox::p_dbl(default=0.0,lower=0.0, upper=0.99,tags="train"),
        monitor= paradox::p_fct(levels=c("loss","val_loss","accuracy","val_accuracy"),default = "loss",tags="train")
      )

      # FIXME: MANUALLY UPDATE PARAM VALUES BELOW IF APPLICABLE THEN DELETE THIS LINE.
      param_set$values = list()

      super$initialize(
        id = "classif.keras_net_gru",
        packages = "aifeducation",
        feature_types = c("numeric"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("twoclass", "multiclass"),
        man = "mlr3extralearners::mlr_learners_classif.keras_net_gru",
        label = "Keras Neural Net with GRU-Layer"
      )
    }
  ),
  private = list(
    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      # FIXME: IF LEARNER DOES NOT HAVE 'weights' PROPERTY THEN DELETE THESE LINES.
      if ("weights" %in% task$properties) {
        # Add weights to learner
      }

      # FIXME: CREATE OBJECTS FOR THE TRAIN CALL
      # AT LEAST "data" AND "formula" ARE REQUIRED
      formula = task$truth()
      data = task$data(cols = task$feature_names)

      # FIXME: HERE IS SPACE FOR SOME CUSTOM ADJUSTMENTS BEFORE PROCEEDING TO THE
      # TRAIN CALL. CHECK OTHER LEARNERS FOR WHAT CAN BE DONE HERE
      # USE THE mlr3misc::invoke FUNCTION (IT'S SIMILAR TO do.call())

      mlr3misc::invoke(
        keras_net_gru,
        target = formula,
        input = data,
        .args = pars
      )
    },
    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")

      # get newdata and ensure same ordering in train and predict
      #newdata = ordered_features(task, self)
      newdata = as.data.frame(task$data(cols = self$model$input_variables))

      # Calculate predictions for the selected predict type.
      type = self$predict_type

      pred =  mlr3misc::invoke(.f = keras_net_gru_predict,
                               model = self$model,
                               newdata = newdata
                               #type = type,
                               #.args = pars
      )

      # FIXME: ADD PREDICTIONS TO LIST BELOW
      return(list(response = pred))
    }
  )
)

#.extralrns_dict$add("classif.keras_net_gru", LearnerClassifkeras_net_gru)
mlr3::mlr_learners$add("classif.keras_net_gru", LearnerClassifkeras_net_gru)
