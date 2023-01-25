.onLoad<-function(libname, pkgname){
#Additional learners
mlr3::mlr_learners$add("classif.keras_seq_net", LearnerClassifKeras_seq_net)
mlr3::mlr_learners$add("classif.keras_net_gru", LearnerClassifkeras_net_gru)
mlr3::mlr_learners$add("classif.multinet", LearnerClassifMultiNet)

#Additional measures
mlr3::mlr_measures$add("classif.dynamiciotaindex", MeasureDynamicIotaIndex)
mlr3::mlr_measures$add("classif.measureminiota2", MeasureMinIota2)
mlr3::mlr_measures$add("classif.measureavgiota2", MeasureAvgIota2)

mlr3::mlr_measures$add("classif.Iota_AVG", AverageIota)
mlr3::mlr_measures$add("classif.Iota_MIN", MinimumIota)

mlr3::mlr_measures$add("classif.kalpha", MeasureKalpha)

}
