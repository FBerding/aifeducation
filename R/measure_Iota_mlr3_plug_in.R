#'@importFrom R6 R6Class

  super<-NULL
  #----------------------------------------------------------------------------
  #Iota2 Measures
  MeasureDynamicIotaIndex=R6::R6Class("MeasureDIotaIndex",
                            inherit = mlr3::MeasureClassif,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  # custom id for the measure
                                  id = "classif.dynamiciotaindex",

                                  # additional packages required to calculate this measure
                                  packages = "iotarelr",

                                  # properties, see below
                                  properties = character(),

                                  # required predict type of the learner
                                  predict_type = "response",

                                  # feasible range of values
                                  range = c(0, 1),

                                  # minimize during tuning?
                                  minimize = FALSE
                                )
                              }
                            ),

                            private = list(
                              # custom scoring function operating on the prediction object
                              .score = function(prediction, ...) {
                                fun.classif.classif.dynamiciotaindex=function(truth, response)
                                {
                                    rater_rel<-iotarelr::check_new_rater(
                                      true_values=as.character(truth),
                                      assigned_values = as.character(response),
                                      #categorical_levels=levels(truth),
                                      con_trace = FALSE
                                      )
                                  dynamic_iota_index<-rater_rel$scale_level$iota_index_dyn2
                                  return(as.numeric(dynamic_iota_index))
                                }
                                fun.classif.classif.dynamiciotaindex(prediction$truth, prediction$response)

                              }
                            )
  )


  #----------------------------------------------------------------------------

   #Iota2 Measures
  MeasureMinIota2=R6::R6Class("MeasureMinIota2",
                                      inherit = mlr3::MeasureClassif,
                                      public = list(
                                        initialize = function() {
                                          super$initialize(
                                            # custom id for the measure
                                            id = "classif.measureminiota2",

                                            # additional packages required to calculate this measure
                                            packages = character(),

                                            # properties, see below
                                            properties = character(),

                                            # required predict type of the learner
                                            predict_type = "response",

                                            # feasible range of values
                                            range = c(0, 1),

                                            # minimize during tuning?
                                            minimize = FALSE
                                          )
                                        }
                                      ),

                                      private = list(
                                        # custom scoring function operating on the prediction object
                                        .score = function(prediction, ...) {
                                          fun.classif.classif.measureminiota2=function(truth, response)
                                          {
                                            rater_rel<-iotarelr::check_new_rater(
                                              true_values=as.character(truth),
                                              assigned_values = as.character(response),
                                              #categorical_levels=levels(truth),
                                              con_trace = FALSE
                                            )
                                            min_iota_2<-min(rater_rel$categorical_level$raw_estimates$iota,
                                                            na.rm=TRUE)
                                            return(as.numeric(min_iota_2))
                                          }
                                          fun.classif.classif.measureminiota2(prediction$truth, prediction$response)

                                        }
                                      )
  )


#------------------------------------------------------------------------------
  MeasureAvgIota2=R6::R6Class("MeasureAvgIota2",
                              inherit = mlr3::MeasureClassif,
                              public = list(
                                initialize = function() {
                                  super$initialize(
                                    # custom id for the measure
                                    id = "classif.measureavgiota2",

                                    # additional packages required to calculate this measure
                                    packages = character(),

                                    # properties, see below
                                    properties = character(),

                                    # required predict type of the learner
                                    predict_type = "response",

                                    # feasible range of values
                                    range = c(0, 1),

                                    # minimize during tuning?
                                    minimize = FALSE
                                  )
                                }
                              ),

                              private = list(
                                # custom scoring function operating on the prediction object
                                .score = function(prediction, ...) {
                                  fun.classif.classif.measureavgiota2=function(truth, response)
                                  {
                                    rater_rel<-iotarelr::check_new_rater(
                                      true_values=as.character(truth),
                                      assigned_values = as.character(response),
                                      #categorical_levels=levels(truth),
                                      con_trace = FALSE
                                    )
                                    avg_iota_2<-mean(rater_rel$categorical_level$raw_estimates$iota,
                                                    na.rm=TRUE)
                                    return(as.numeric(avg_iota_2))
                                  }
                                  fun.classif.classif.measureavgiota2(prediction$truth, prediction$response)

                                }
                              )
  )


  #----------------------------------------------------------------------------
  #Krippendorff's Alpha
  MeasureKalpha=R6::R6Class("MeasureKalpha",
                            inherit = mlr3::MeasureClassif,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  # custom id for the measure
                                  id = "classif.kalpha",

                                  # additional packages required to calculate this measure
                                  packages = character(),

                                  # properties, see below
                                  properties = character(),

                                  # required predict type of the learner
                                  predict_type = "response",

                                  # feasible range of values
                                  range = c(-1, 1),

                                  # minimize during tuning?
                                  minimize = FALSE
                                )
                              }
                            ),

                            private = list(
                              # custom scoring function operating on the prediction object
                              .score = function(prediction, ...) {
                                fun.classif.kalpha=function(truth, response)
                                {
                                  Wert<-irr::kripp.alpha(rbind(truth,response), method = c("ordinal"))
                                  Wert$value
                                }
                                fun.classif.kalpha(prediction$truth, prediction$response)

                              }
                            )
  )




  #---------------------------------------------------------------------------
  #Iota1 Measures
  AverageIota=R6::R6Class("AverageIota",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_AVG",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_AVG=function(truth, response)
                              {
                                Wert<-iotarelr::compute_iota1(cbind(truth,response))
                                Wert$average_iota
                              }
                              fun.classif.Iota_AVG(prediction$truth, prediction$response)

                            }
                          )
)



  MinimumIota=R6::R6Class("MinimumIota",
                          inherit = mlr3::MeasureClassif,
                          public = list(
                            initialize = function() {
                              super$initialize(
                                # custom id for the measure
                                id = "classif.Iota_MIN",

                                # additional packages required to calculate this measure
                                packages = character(),

                                # properties, see below
                                properties = character(),

                                # required predict type of the learner
                                predict_type = "response",

                                # feasible range of values
                                range = c(0, 1),

                                # minimize during tuning?
                                minimize = FALSE
                              )
                            }
                          ),

                          private = list(
                            # custom scoring function operating on the prediction object
                            .score = function(prediction, ...) {
                              fun.classif.Iota_MIN=function(truth, response)
                              {
                                Wert<-iotarelr::compute_iota1(cbind(truth,response))
                                Wert<-min(Wert$iota)
                              }
                              fun.classif.Iota_MIN(prediction$truth, prediction$response)

                            }
                          )
)

mlr3::mlr_measures$add("classif.Iota_MIN", MinimumIota)
#--------------------------------------------------------------------------------



