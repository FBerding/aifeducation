#'Get pre configurated learners
#'
#'Function for getting pre-configurated learners for convenience.
#'
#' @importFrom mlr3pipelines po GraphLearner concat_graphs
#' @importFrom mlr3 lrn
#' @importFrom paradox to_tune
#' @export
 get_configurated_learner<-function(learner_name,
                                    use_smote=TRUE,
                                    smote_rep=1,
                                    autotuning=FALSE,
                                    smote_K=1,
                                    smote_k_max=6,
                                    smote_dup=1,
                                    smote_dup_max=5,
                                    features=NULL,
                                    times=NULL)
{
   if(learner_name=="nnet"){
     learner<-mlr3::lrn("classif.nnet",
                  predict_type = "response",
                  decay=0.2,
                  size=100,
                  trace=FALSE,
                  MaxNWts=100000000)

     if(use_smote==TRUE){
       for(i in 1:smote_rep){
        if(i==1){
          po_smote = mlr3pipelines::po("smote",
                                       dup_size=smote_dup[i],
                                       K=smote_K[i],
                                       id=paste0("smote",i)
                                       )
        }
         else{
           po_smote=mlr3pipelines::concat_graphs(po_smote,
                                  mlr3pipelines::po("smote",
                                                    dup_size=smote_dup[i],
                                                    K=smote_K[i],
                                                    id=paste0("smote",i)),
                                  in_place=FALSE)
         }
       }
       learner=mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(
         po_smote,
         learner,
         in_place=FALSE))
     }

     if(autotuning==TRUE & use_smote==TRUE){
       learner$param_set$values$classif.nnet.trace=FALSE
       learner$param_set$values$classif.nnet.size=paradox::to_tune(2,200)
      learner$param_set$values$classif.nnet.decay=paradox::to_tune(0.000001,1,logscale=TRUE)
      #learner$param_set$values$smote.dup_size=paradox::to_tune(1,smote_dup_max)
      #learner$param_set$values$smote.K=paradox::to_tune(1,smote_k_max)
      for(i in 1:smote_rep){
        learner$param_set$values[[paste0("smote",i,".dup_size")]]=paradox::to_tune(1,smote_dup_max[i])
        learner$param_set$values[[paste0("smote",i,".K")]]=paradox::to_tune(1,smote_k_max[i])
      }


     }
     if(autotuning==TRUE & use_smote==FALSE){
       learner$param_set$values$trace=FALSE
       learner$param_set$values$size=paradox::to_tune(2,200)
       learner$param_set$values$decay=paradox::to_tune(0.000001,1,logscale=TRUE)
     }

     #if(autotuning==TRUE){
     #learner = mlr3tuning::AutoTuner$new(
    #   learner=learner,
    #   resampling = inner_sampling,
    #   measure =  cr_optim,
    #   terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
    #   tuner=mlr3tuning::tnr("random_search")
    # )
    # }
     return(learner)
   }
   if(learner_name=="rpart"){
     learner<-mlr3::lrn("classif.rpart",
                  predict_type = "response",
                  cp=0.01,
                  maxdepth=30,
                  maxsurrogate=5,
                  minsplit=20
                  )

     if(use_smote==TRUE){
       for(i in 1:smote_rep){
         if(i==1){
           po_smote = mlr3pipelines::po("smote",
                                        dup_size=smote_dup[i],
                                        K=smote_K[i],
                                        id=paste0("smote",i)
           )
         }
         else{
           po_smote=mlr3pipelines::concat_graphs(po_smote,
                                  mlr3pipelines::po("smote",
                                                    dup_size=smote_dup[i],
                                                    K=smote_K[i],
                                                    id=paste0("smote",i)),
                                  in_place=FALSE)
         }
       }
       learner=mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(
         po_smote,
         learner,
         in_place=FALSE))
     }

     if(autotuning==TRUE & use_smote==TRUE){
       learner$param_set$values$classif.rpart.cp=paradox::to_tune(0.001,1,TRUE)
       learner$param_set$values$classif.rpart.maxdepth=paradox::to_tune(1,30)
       learner$param_set$values$classif.rpart.maxsurrogate=paradox::to_tune(1,50)
       learner$param_set$values$classif.rpart.minsplit=paradox::to_tune(20,100,TRUE)
       #learner$param_set$values$smote.dup_size=paradox::to_tune(1,smote_dup_max)
       #learner$param_set$values$smote.K=paradox::to_tune(1,smote_k_max)
       for(i in 1:smote_rep){
         learner$param_set$values[[paste0("smote",i,".dup_size")]]=paradox::to_tune(1,smote_dup_max[i])
         learner$param_set$values[[paste0("smote",i,".K")]]=paradox::to_tune(1,smote_k_max[i])
       }
     }
     if(autotuning==TRUE & use_smote==FALSE){
       learner$param_set$values$cp=paradox::to_tune(0.001,1,TRUE)
       learner$param_set$values$maxdepth=paradox::to_tune(1,30)
       learner$param_set$values$maxsurrogate=paradox::to_tune(1,50)
       learner$param_set$values$minsplit=paradox::to_tune(20,100,TRUE)
     }

     #if(autotuning==TRUE){
    #   learner = mlr3tuning::AutoTuner$new(
    #     learner=learner,
    #     resampling = inner_sampling,
    #     measure =  cr_optim,
    #     terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
    #     tuner=mlr3tuning::tnr("random_search")
    #   )
    # }
     return(learner)
   }
   if(learner_name=="ranger"){
     learner<-mlr3::lrn("classif.ranger",
                  predict_type = "response",
                  importance="impurity",
                  max.depth=30,
                  min.node.size=1,
                  minprop=0.1,
                  replace=TRUE,
                  splitrule="gini"
     )

     if(use_smote==TRUE){
       for(i in 1:smote_rep){
         if(i==1){
           po_smote = mlr3pipelines::po("smote",
                                        dup_size=smote_dup[i],
                                        K=smote_K[i],
                                        id=paste0("smote",i)
           )
         }
         else{
           po_smote=mlr3pipelines::concat_graphs(po_smote,
                                  mlr3pipelines::po("smote",
                                                    dup_size=smote_dup[i],
                                                    K=smote_K[i],
                                                    id=paste0("smote",i)),
                                  in_place=FALSE)
         }
       }
       learner=mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(
         po_smote,
         learner,
         in_place=FALSE))
     }

     if(autotuning==TRUE & use_smote==TRUE){
       #learner$param_set$values$classif.ranger.importance=paradox::to_tune(levels=c(
       # "none","impurity","permutation"))
       learner$param_set$values$classif.ranger.importance=paradox::to_tune(levels=c(
        "none","impurity"))
       learner$param_set$values$classif.ranger.max.depth=paradox::to_tune(1,60)
       learner$param_set$values$classif.ranger.min.node.size=paradox::to_tune(1,10)
       learner$param_set$values$classif.ranger.minprop=paradox::to_tune(0.01,.20,TRUE)
       learner$param_set$values$classif.ranger.replace=paradox::to_tune(levels=c(TRUE,FALSE))
       learner$param_set$values$classif.ranger.splitrule=paradox::to_tune(levels=c("gini","extratrees"))
       #learner$param_set$values$smote.dup_size=paradox::to_tune(1,smote_dup_max)
       #learner$param_set$values$smote.K=paradox::to_tune(1,smote_k_max)
       for(i in 1:smote_rep){
         learner$param_set$values[[paste0("smote",i,".dup_size")]]=paradox::to_tune(1,smote_dup_max[i])
         learner$param_set$values[[paste0("smote",i,".K")]]=paradox::to_tune(1,smote_k_max[i])
       }
     }
     if(autotuning==TRUE & use_smote==FALSE){
       learner$param_set$values$importance=paradox::to_tune(levels=c(
         "none","impurity"))
       learner$param_set$values$max.depth=paradox::to_tune(1,60)
       learner$param_set$values$min.node.size=paradox::to_tune(1,10)
       learner$param_set$values$minprop=paradox::to_tune(0.01,.20,TRUE)
       learner$param_set$values$replace=paradox::to_tune(levels=c(TRUE,FALSE))
       learner$param_set$values$splitrule=paradox::to_tune(levels=c("gini","extratrees"))
     }

     #if(autotuning==TRUE){
    #   learner = mlr3tuning::AutoTuner$new(
    #     learner=learner,
    #     resampling = inner_sampling,
    #     measure =  cr_optim,
    #     terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
    #     tuner=mlr3tuning::tnr("random_search")
    #   )
    # }
     return(learner)
   }
     #------------------------------------------------------------------------
     if(learner_name=="classif.keras_seq_net"){
       learner<-mlr3::lrn("classif.keras_seq_net",
                    n_hidden=3,
                    n_hidden_size=50,
                    #optimizer=keras::optimizer_rmsprop(),
                    act_fct="sigmoid",
                    act_fct_last="softmax",
                    err_fct="categorical_crossentropy",
                    epochs = 3000L,
                    batch_size = 1L,
                    rel_tolerance=1e-3,
                    view_metrics = FALSE,
                    validation_split = 0.0,
                    monitor="loss"
       )

       if(use_smote==TRUE){
         learner$param_set$values$monitor="val_loss"
         learner$param_set$values$validation_split=0.2
         for(i in 1:smote_rep){
           if(i==1){
             po_smote = mlr3pipelines::po("smote",
                                          dup_size=smote_dup[i],
                                          K=smote_K[i],
                                          id=paste0("smote",i)
             )
           }
           else{
             po_smote=mlr3pipelines::concat_graphs(po_smote,
                                    mlr3pipelines::po("smote",
                                                      dup_size=smote_dup[i],
                                                      K=smote_K[i],
                                                      id=paste0("smote",i)),
                                    in_place=FALSE)
           }
         }
         learner=mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(
           po_smote,
           learner,
           in_place=FALSE))
       }

       if(autotuning==TRUE & use_smote==TRUE){
         learner$param_set$values$classif.keras_seq_net.monitor="val_loss"
         learner$param_set$values$classif.keras_seq_net.validation_split=0.2

         learner$param_set$values$classif.keras_seq_net.n_hidden=paradox::to_tune(1,12)
         learner$param_set$values$classif.keras_seq_net.n_hidden_size=paradox::to_tune(3,30)
         learner$param_set$values$classif.keras_seq_net.rel_tolerance=paradox::to_tune(upper=1e-2,lower=1e-9,logscale=TRUE)
         #learner$param_set$values$classif.keras_seq_net.act_fct=paradox::to_tune(levels=c("relu","sigmoid"))
         #learner$param_set$values$classif.keras_seq_net.act_fct_last=paradox::to_tune(levels=c("softmax","relu","sigmoid"))
         #learner$param_set$values$classif.keras_seq_net.err_fct==paradox::to_tune(levels=c("categorical_crossentropy","mse"))
         #learner$param_set$values$smote.dup_size=paradox::to_tune(1,smote_dup_max)
         #learner$param_set$values$smote.K=paradox::to_tune(1,smote_k_max)
         for(i in 1:smote_rep){
           learner$param_set$values[[paste0("smote",i,".dup_size")]]=paradox::to_tune(1,smote_dup_max[i])
           learner$param_set$values[[paste0("smote",i,".K")]]=paradox::to_tune(1,smote_k_max[i])
         }
       }
       if(autotuning==TRUE & use_smote==FALSE){
         learner$param_set$values$n_hidden=paradox::to_tune(1,12)
         learner$param_set$values$n_hidden_size=paradox::to_tune(1,30)
        # learner$param_set$values$act_fct=paradox::to_tune(levels=c("relu","sigmoid"))
         learner$param_set$values$rel_tolerance=paradox::to_tune(upper=1e-2,lower=1e-9,logscale=TRUE)
         #learner$param_set$values$act_fct_last=paradox::to_tune(levels=c("softmax","relu","sigmoid"))
         #learner$param_set$values$err_fct==paradox::to_tune(levels=c("categorical_crossentropy","mse"))
       }

       #if(autotuning==TRUE){
      #   learner = mlr3tuning::AutoTuner$new(
      #     learner=learner,
      #     resampling = inner_sampling,
      #     measure =  cr_optim,
      #     terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
      #     tuner=mlr3tuning::tnr("random_search")
      #   )
      # }
       return(learner)
     }
   #------------------------------------------------------------------------
   if(learner_name=="classif.keras_net_gru"){
     learner<-mlr3::lrn("classif.keras_net_gru",
                  n_hidden=2,
                  n_hidden_size=6,
                  n_gru=1,
                  n_gru_size=30,
                  times=times,
                  features=features,
                  dropout=0,
                  recurrent_dropout=0,
                  #optimizer=keras::optimizer_rmsprop(),
                  act_fct="sigmoid",
                  act_fct_last="softmax",
                  err_fct="categorical_crossentropy",
                  epochs = 3000L,
                  batch_size = 5L,
                  rel_tolerance=1e-3,
                  view_metrics = FALSE,
                  validation_split = 0.0,
                  monitor="loss"
     )

     if(use_smote==TRUE){
       learner$param_set$values$monitor="val_loss"
       learner$param_set$values$validation_split=0.2
       for(i in 1:smote_rep){
         if(i==1){
           po_smote = mlr3pipelines::po("smote",
                                        dup_size=smote_dup[i],
                                        K=smote_K[i],
                                        id=paste0("smote",i)
           )
         }
         else{
           po_smote=mlr3pipelines::concat_graphs(po_smote,
                                  mlr3pipelines::po("smote",
                                                    dup_size=smote_dup[i],
                                                    K=smote_K[i],
                                                    id=paste0("smote",i)),
                                  in_place=FALSE)
         }
       }
       learner=mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(
         po_smote,
         learner,
         in_place=FALSE))
     }

     if(autotuning==TRUE & use_smote==TRUE){
       learner$param_set$values$classif.keras_net_gru.monitor="val_loss"
       learner$param_set$values$classif.keras_net_gru.validation_split=0.2

       learner$param_set$values$classif.keras_net_gru.n_hidden=paradox::to_tune(1,12)
       learner$param_set$values$classif.keras_net_gru.n_hidden_size=paradox::to_tune(3,30)
       learner$param_set$values$classif.keras_net_gru.rel_tolerance=paradox::to_tune(upper=1e-2,lower=1e-9,logscale=TRUE)
       #learner$param_set$values$classif.keras_net_gru.n_gru=paradox::to_tune(1,5)
       learner$param_set$values$classif.keras_net_gru.n_gru_size=paradox::to_tune(3,50)
       learner$param_set$values$classif.keras_net_gru.dropout=paradox::to_tune(lower=0,upper=0.2)
       learner$param_set$values$classif.keras_net_gru.recurrent_dropout=paradox::to_tune(lower=0,upper=0.2)
       #learner$param_set$values$classif.keras_seq_net.act_fct=paradox::to_tune(levels=c("relu","sigmoid"))
       #learner$param_set$values$classif.keras_seq_net.act_fct_last=paradox::to_tune(levels=c("softmax","relu","sigmoid"))
       #learner$param_set$values$classif.keras_seq_net.err_fct==paradox::to_tune(levels=c("categorical_crossentropy","mse"))
       #learner$param_set$values$smote.dup_size=paradox::to_tune(1,smote_dup_max)
       #learner$param_set$values$smote.K=paradox::to_tune(1,smote_k_max)
       for(i in 1:smote_rep){
         learner$param_set$values[[paste0("smote",i,".dup_size")]]=paradox::to_tune(1,smote_dup_max[i])
         learner$param_set$values[[paste0("smote",i,".K")]]=paradox::to_tune(1,smote_k_max[i])
       }
     }
     if(autotuning==TRUE & use_smote==FALSE){
       learner$param_set$values$n_hidden=paradox::to_tune(1,12)
       learner$param_set$values$n_hidden_size=paradox::to_tune(1,30)
       # learner$param_set$values$act_fct=paradox::to_tune(levels=c("relu","sigmoid"))
       learner$param_set$values$rel_tolerance=paradox::to_tune(upper=1e-2,lower=1e-9,logscale=TRUE)
       learner$param_set$values$n_gru_size=paradox::to_tune(3,50)
       learner$param_set$values$dropout=paradox::to_tune(lower=0,upper=0.2)
       learner$param_set$values$recurrent_dropout=paradox::to_tune(lower=0,upper=0.2)
       #learner$param_set$values$act_fct_last=paradox::to_tune(levels=c("softmax","relu","sigmoid"))
       #learner$param_set$values$err_fct==paradox::to_tune(levels=c("categorical_crossentropy","mse"))
     }

     #if(autotuning==TRUE){
     #   learner = mlr3tuning::AutoTuner$new(
     #     learner=learner,
     #     resampling = inner_sampling,
     #     measure =  cr_optim,
     #     terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
     #     tuner=mlr3tuning::tnr("random_search")
     #   )
     # }
     return(learner)
   }
   #-------------------------------------------------------------------------

   if(learner_name=="multinet"){
     learner<-mlr3::lrn("classif.multinet",
                        predict_type = "response",
                        n_hidden=3,
                        n_hidden_size=100,
                        learning_rate = 0.001,
                        act_fct="relu",
                        act_fct_last="softmax",
                        err_msr="iota2",
                        freq_recalc_iota2=10,
                        max_iter = 1000,
                        cr_rel_change = 1e-6,
                        cr_abs_error=1e-4,
                        validation_split=0.2,
                        split_method="strata",
                        monitor="val_loss",
                        patience=3,
                        return_best=TRUE,
                        trace=FALSE
                        )

     if(use_smote==TRUE){
       for(i in 1:smote_rep){
         if(i==1){
           po_smote = mlr3pipelines::po("smote",
                                        dup_size=smote_dup[i],
                                        K=smote_K[i],
                                        id=paste0("smote",i)
           )
         }
         else{
           po_smote=mlr3pipelines::concat_graphs(po_smote,
                                                 mlr3pipelines::po("smote",
                                                                   dup_size=smote_dup[i],
                                                                   K=smote_K[i],
                                                                   id=paste0("smote",i)),
                                                 in_place=FALSE)
         }
       }
       learner=mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(
         po_smote,
         learner,
         in_place=FALSE))
     }

     if(autotuning==TRUE & use_smote==TRUE){
       learner$param_set$values$classif.multinet.trace=FALSE
       learner$param_set$values$classif.multinet.n_hidden_size=paradox::to_tune(5,100)
       learner$param_set$values$classif.multinet.n_hidden=paradox::to_tune(2,5)
       learner$param_set$values$classif.multinet.learning_rate=paradox::to_tune(lower=0.001,
                                                                                upper=0.800,
                                                                                logscale=TRUE)

       #learner$param_set$values$smote.dup_size=paradox::to_tune(1,smote_dup_max)
       #learner$param_set$values$smote.K=paradox::to_tune(1,smote_k_max)
       for(i in 1:smote_rep){
         learner$param_set$values[[paste0("smote",i,".dup_size")]]=paradox::to_tune(1,smote_dup_max[i])
         learner$param_set$values[[paste0("smote",i,".K")]]=paradox::to_tune(1,smote_k_max[i])
       }


     }
     if(autotuning==TRUE & use_smote==FALSE){
      learner$param_set$values$trace=FALSE
      learner$param_set$values$n_hidden_size=paradox::to_tune(5,100)
      learner$param_set$values$n_hidden=paradox::to_tune(2,5)
      learner$param_set$values$learning_rate=paradox::to_tune(lower=0.001,
                                                              upper=0.800,
                                                              logscale=TRUE)

     }

     #if(autotuning==TRUE){
     #learner = mlr3tuning::AutoTuner$new(
     #   learner=learner,
     #   resampling = inner_sampling,
     #   measure =  cr_optim,
     #   terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
     #   tuner=mlr3tuning::tnr("random_search")
     # )
     # }
     return(learner)
   }

   if(learner_name=="multinet_entropy"){
     learner<-mlr3::lrn("classif.multinet",
                        predict_type = "response",
                        n_hidden=3,
                        n_hidden_size=100,
                        learning_rate = 0.001,
                        act_fct="relu",
                        act_fct_last="softmax",
                        err_msr="cross_entropy",
                        freq_recalc_iota2=10,
                        max_iter = 1000,
                        cr_rel_change = 1e-6,
                        cr_abs_error=1e-4,
                        validation_split=0.2,
                        split_method="strata",
                        monitor="val_loss",
                        patience=5,
                        return_best=TRUE,
                        trace=FALSE
     )

     if(use_smote==TRUE){
       for(i in 1:smote_rep){
         if(i==1){
           po_smote = mlr3pipelines::po("smote",
                                        dup_size=smote_dup[i],
                                        K=smote_K[i],
                                        id=paste0("smote",i)
           )
         }
         else{
           po_smote=mlr3pipelines::concat_graphs(po_smote,
                                                 mlr3pipelines::po("smote",
                                                                   dup_size=smote_dup[i],
                                                                   K=smote_K[i],
                                                                   id=paste0("smote",i)),
                                                 in_place=FALSE)
         }
       }
       learner=mlr3pipelines::GraphLearner$new(mlr3pipelines::concat_graphs(
         po_smote,
         learner,
         in_place=FALSE))
     }

     if(autotuning==TRUE & use_smote==TRUE){
       learner$param_set$values$classif.multinet.trace=FALSE
       learner$param_set$values$classif.multinet.n_hidden_size=paradox::to_tune(5,100)
       learner$param_set$values$classif.multinet.n_hidden=paradox::to_tune(2,5)
       learner$param_set$values$classif.multinet.learning_rate=paradox::to_tune(lower=0.001,
                                                                                upper=0.800,
                                                                                logscale=TRUE)

       #learner$param_set$values$smote.dup_size=paradox::to_tune(1,smote_dup_max)
       #learner$param_set$values$smote.K=paradox::to_tune(1,smote_k_max)
       for(i in 1:smote_rep){
         learner$param_set$values[[paste0("smote",i,".dup_size")]]=paradox::to_tune(1,smote_dup_max[i])
         learner$param_set$values[[paste0("smote",i,".K")]]=paradox::to_tune(1,smote_k_max[i])
       }


     }
     if(autotuning==TRUE & use_smote==FALSE){
       learner$param_set$values$trace=FALSE
       learner$param_set$values$n_hidden_size=paradox::to_tune(5,100)
       learner$param_set$values$n_hidden=paradox::to_tune(2,5)
       learner$param_set$values$learning_rate=paradox::to_tune(lower=0.001,
                                                               upper=0.800,
                                                               logscale=TRUE)

     }

     #if(autotuning==TRUE){
     #learner = mlr3tuning::AutoTuner$new(
     #   learner=learner,
     #   resampling = inner_sampling,
     #   measure =  cr_optim,
     #   terminator=mlr3tuning::trm("evals",n_evals=max_n_tuning),
     #   tuner=mlr3tuning::tnr("random_search")
     # )
     # }
     return(learner)
   }
}


