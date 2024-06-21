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

py_dataset_to_embeddings=function(py_dataset){
  py_dataset$set_format("np")
  embeddings=py_dataset["input"]
  rownames(embeddings)=py_dataset["id"]
  return(embeddings)
}

prepare_r_array_for_dataset=function(r_array){
  return(np$squeeze(np$split(reticulate::np_array(r_array),as.integer(nrow(r_array)),axis=0L)))
}

get_batches_index=function(number_rows,batch_size,zero_based=FALSE){
  n_batches=ceiling(number_rows/batch_size)
  index_list=NULL
  for(i in 1:n_batches){
    min=1+(i-1)*batch_size
    max=min(batch_size+(i-1)*batch_size,number_rows)
    if(zero_based==FALSE){
      index_list[i]=list(seq.int(from=min,to=max,by=1))
    } else {
      index_list[i]=list(seq.int(from=min,to=max,by=1)-1)
    }

  }
  return(index_list)
}

reduce_to_unique=function(dataset_to_reduce,column_name){
  selected_column=dataset_to_reduce$data$column(column_name)
  unique_values=pyarrow$compute$unique(selected_column)
  unique_indices=pyarrow$compute$index_in(unique_values,selected_column)
  reduced_dataset=dataset_to_reduce$select(unique_indices)
  return(reduced_dataset)
}

class_vector_to_py_dataset=function(vector){
  data_frame=as.data.frame(vector)
  data_frame$id=names(vector)
  colnames(data_frame)=c("labels","id")
  if(length(data_frame)==1){
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
