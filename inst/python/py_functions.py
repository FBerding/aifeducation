import numpy as np

def map_input_to_labels(dataset):
  return {"labels": dataset["input"]}
  
def map_input_to_matrix_form(dataset,times,features):
  sequence=dataset["input"]
  return {"matrix_form": np.float32(np.squeeze(np.reshape(sequence,newshape=(1,times*features))))}

def map_labels_to_one_hot(dataset,num_classes):
  label=int(dataset["labels"])
  one_hot_vector=np.zeros((num_classes))
  one_hot_vector[label]=1
  return {"one_hot_encoding": one_hot_vector}

