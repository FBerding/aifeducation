# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

import numpy as np
import torch

def map_input_to_labels(dataset):
  return {"labels": dataset["input"]}
  
def map_input_to_matrix_form(dataset,times,features):
  sequence=dataset["input"]
  return {"matrix_form": np.float32(np.squeeze(np.reshape(sequence,(1,times*features))))}
  #return {"matrix_form": np.float32(np.squeeze(np.reshape(sequence,newshape=(1,times*features))))}

def map_labels_to_one_hot(dataset,num_classes):
  label=int(dataset["labels"])
  one_hot_vector=np.zeros((num_classes))
  one_hot_vector[label]=1
  return {"one_hot_encoding": one_hot_vector}

def map_switch_pad_values(dataset,pad_value_old,pad_value_new):
  x=dataset["input"]
  features=x.size(2)
  time_sums=torch.sum(x,dim=2)
  mask=(time_sums==features*pad_value_old)
  x[mask]=pad_value_new
  dataset["input"]=x
  return dataset

def switch_pad_values(dataset,pad_value_old,pad_value_new):
  dataset.set_format("torch")
  new_data_set=dataset.map(map_switch_pad_values,fn_kwargs={"pad_value_old":pad_value_old,"pad_value_new":pad_value_new},batched=True)
  return new_data_set
#test_abc=r.abc.map(switch_pad_values,fn_kwargs={"pad_value_old":-100,"pad_value_new":-500,"features":64},batched=True)
#test_abc["input"]
