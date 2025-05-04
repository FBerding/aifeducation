generate_args_for_tests=function(object,
                                 var_objects=list(),
                                 necessary_objects=list(),
                                 var_override=list(
                                   sustain_interval=30,
                                   trace=FALSE,
                                   epochs=50,
                                   batch_size=20,
                                   ml_trace=0,
                                   n_cores=2,
                                   data_folds=2,
                                   pl_max_steps=2,
                                   pl_max=1,
                                   pl_anchor=1,
                                   pl_min=0,
                                   sustain_track=TRUE,
                                   sustain_iso_code="DEU",
                                   data_val_size=0.25,
                                   lr_rate=1e-3,
                                   lr_warm_up_ratio=0.01
                                 )
                                 ){
  arg_list=rlang::fn_fmls(object[[method]])
  arg_names=names(arg_list)
  param_dict=get_param_dict()

  #Generate list of values for every parameter that can vary
  arg_value_list=NULL
  for(param in arg_names){
    current_entry=param_dict[[param]]
    if(is_valid_and_exportable_param(param,param_dict) & !(param%in%c(names(var_override),names(necessary_objects)))){

      if(current_entry$type=="string"){
        if(!is.null(current_entry$allowed_values)){
          arg_value_list[param]=list(current_entry$allowed_values)
        }
      } else if(current_entry$type=="bool") {
        arg_value_list[param]=list(c(FALSE,TRUE))
      } else {
        if(current_entry$min==-Inf){
          tmp_min=-1
        } else {
          tmp_min=current_entry$min
        }

        if(current_entry$max==Inf){
          tmp_max=2
        } else {
          tmp_max=current_entry$max
        }
      }

      if(current_entry$type=="int"){
        arg_value_list[param]=list(seq(from=tmp_min,to=tmp_max,by=1))
      } else if(current_entry$type=="double"){
        arg_value_list[param]=list(c(tmp_min,tmp_max,0.5*tmp_min+0.5*tmp_max))
      } else if(current_entry$type=="(double"){
        arg_value_list[param]=list(c(0.99*tmp_min,tmp_max,0.5*tmp_min+0.5*tmp_max))
      }else if(current_entry$type=="double)"){
        arg_value_list[param]=list(c(tmp_min,0.99*tmp_max,0.5*tmp_min+0.5*tmp_max))
      }else if(current_entry$type=="(double)"){
        arg_value_list[param]=list(c(0.99*tmp_min,0.99*tmp_max,0.5*tmp_min+0.5*tmp_max))
      }
    }
  }

  #Add var objects
  for(var_object in names(var_objects)){
    arg_value_list[var_object]=list(c(FALSE,TRUE))
  }

  #create all combinations
  arg_comb=expand.grid(arg_value_list,KEEP.OUT.ATTRS = FALSE,stringsAsFactors=FALSE)

  #Convert combinations to list and add override parameters and necessary parameters
  arg_comb_list=NULL
  override_subset=intersect(arg_names,names(var_override))
  necessary_subset=intersect(arg_names,names(necessary_objects))
  for(i in 1:nrow(arg_comb)){
    arg_comb_list[i]=list(
      c(arg_comb[i,],
        var_override[override_subset],
        necessary_objects[necessary_subset])
      )
  }

  #add var objects
  for(i in seq_along(arg_comb_list)){
    for(var_object in names(var_objects)){
      if(arg_comb_list[[i]][[var_object]]==TRUE){
        arg_comb_list[[i]][var_object]=list(var_objects[[var_object]])
      } else {
        arg_comb_list[[i]][var_object]=list(NULL)
      }
    }
  }

return(arg_list=arg_comb_list,n_combos=length(arg_comb_list))
}
