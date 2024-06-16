data.frame_to_py_dataset=function(data_frame){
  if(nrow(data_frame)==1){
    data_frame=rbind(data_frame,data_frame)
    data_list=as.list(data_frame)
    data_dict=reticulate::dict(data_list)
    dataset=datasets$Dataset$from_dict(data_dict)
    dataset=dataset$select(indices=list(as.integer(0)))
  } else {
    data_list=as.list(data_frame)
    data_dict=reticulate::dict(data_list)
    dataset=datasets$Dataset$from_dict(data_dict)
  }
  return(dataset)
}

prepare_r_array_for_dataset=function(r_array){
  return(np$squeeze(np$split(reticulate::np_array(r_array),as.integer(nrow(r_array)),axis=0L)))
}

get_batches_index=function(number_rows,batch_size){
  n_batches=ceiling(number_rows/batch_size)
  index_list=NULL
  for(i in 1:n_batches){
    min=1+(i-1)*batch_size
    max=min(batch_size+(i-1)*batch_size,number_rows)
    index_list[i]=list(seq.int(from=min,to=max,by=1))
  }
  return(index_list)
}
