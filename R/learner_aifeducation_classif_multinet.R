#' @title Classification Multi Layer Neural Net Learner
#' @author FBerding
#' @name mlr_learners_classif.multinet
#'
#' @description
#' FIXME: BRIEF DESCRIPTION OF THE LEARNER.
#' Calls [aifeducation::multi_net()] from FIXME: (CRAN VS NO CRAN): \CRANpkg{aifeducation} | 'aifeducation'.
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
#' @templateVar id classif.multinet
#'
#' @references
#' `r format_bib(FIXME: ONE OR MORE REFERENCES FROM bibentries.R)`
#'
#' @export
LearnerClassifMultiNet = R6::R6Class("LearnerClassifMultiNet",
  inherit = mlr3::LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      # FIXME: MANUALLY ADD PARAMETERS BELOW AND THEN DELETE THIS LINE
      param_set =  paradox::ps(
        n_hidden =  paradox::p_int(lower = 1L, default = 3L,tags = "train"),
        n_hidden_size =  paradox::p_int(lower = 1L,default = 7L,tags = "train"),
        learning_rate= paradox::p_dbl(lower = 1e-5, default=0.01, tags="train"),
        act_fct= paradox::p_fct(levels=c("relu","sigmoid","tanh"), default = "relu", tags="train"),
        act_fct_last= paradox::p_fct(levels=c("softmax","relu","sigmoid","tanh"), default = "softmax", tags="train"),
        err_msr= paradox::p_fct(levels=c("cross_entropy","mse","iota2"), default = "iota2",tags = "train"),
        freq_recalc_iota2 = paradox::p_int(lower = 1L,default = 5L,tags = "train"),
        max_iter =  paradox::p_int(lower=1L, default= 30L, tags="train"),
        cr_rel_change= paradox::p_dbl(lower=0.0, default = 1e-8, upper=1,tags="train"),
        cr_abs_error= paradox::p_dbl(lower=0.0, default = 0.0, upper=1,tags="train"),
        validation_split= paradox::p_dbl(lower=0.01,default=0.2,upper=0.99,tags="train"),
        split_method= paradox::p_fct(levels=c("strata","equal"),default="strata"),
        monitor= paradox::p_fct(levels=c("loss","val_loss"),default = "val_loss",tags="train"),
        return_best= paradox::p_lgl(default=TRUE,tags="train"),
        patience =  paradox::p_int(lower = 1L, default = 4L,tags = "train"),
        trace= paradox::p_lgl(default=FALSE,tags="train")
      )

      # FIXME: MANUALLY UPDATE PARAM VALUES BELOW IF APPLICABLE THEN DELETE THIS LINE.
      param_set$values = list()

      super$initialize(
        id = "classif.multinet",
        packages = "aifeducation",
        feature_types = c("integer", "numeric"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("twoclass", "multiclass"),
        man = "mlr3extralearners::mlr_learners_classif.multinet",
        label = "Multi Layer Neural Net"
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
      data = as.data.frame(task$data(cols = task$feature_names))

      # FIXME: HERE IS SPACE FOR SOME CUSTOM ADJUSTMENTS BEFORE PROCEEDING TO THE
      # TRAIN CALL. CHECK OTHER LEARNERS FOR WHAT CAN BE DONE HERE
      # USE THE mlr3misc::invoke FUNCTION (IT'S SIMILAR TO do.call())

      mlr3misc::invoke(
        .f=multi_net,
        output = formula,
        input = data,
        .args = pars
      )
    },
    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")

      # get newdata and ensure same ordering in train and predict
      newdata = task$data(cols = self$model$features_order)
      #newdata = ordered_features(task,self)
      #newdata = as.data.frame(lapply(newdata, FUN=as.numeric))


      # Calculate predictions for the selected predict type.
      type = self$predict_type

      pred =  mlr3misc::invoke(.f = multi_net_predict,
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

#.extralrns_dict$add("classif.multinet", LearnerClassifMultiNet)
#mlr3::mlr_learners$add("classif.multinet", LearnerClassifMultiNet)
