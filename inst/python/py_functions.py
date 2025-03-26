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

