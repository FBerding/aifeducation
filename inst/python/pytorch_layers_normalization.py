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

import torch 
import numpy as np
import math
import safetensors

#LayerNorm_with_Mask------------------------------------------------------------
#Layer generating the Layer Norm for sequential data.
# Returns a list with the following tensors
# * Input tensor
# * Sequence length of the tensors shape (Batch)
# * mask_times Mask on the level of complete sequences shape (Batch, Times)
# * mask_features Mask on the level of single features shape (Bath, Times, Features)
# True indicates that the sequence or is padded. If True these values should not be part 
# of further computations
# Layer Norm is applied to the last dimensio as described in the paper
# Layer Normalization in equation 4.
class LayerNorm_with_Mask(torch.nn.Module):
    def __init__(self, times, features,pad_value,eps=1e-5):
      super().__init__()
      self.eps=eps
      self.times=times
      self.features = features
      if isinstance(pad_value, torch.Tensor):
        self.pad_value=pad_value.detach()
      else:
        self.pad_value=torch.tensor(pad_value)
      self.gamma = torch.nn.Parameter(torch.ones(1, 1, self.features))

    def forward(self, x,seq_len,mask_times,mask_features):

      #Calculate mean 
      #Set padding value to zero for correct sum
      x_zeros=x*(~mask_features)
      #Create the sum for every timestep and case. These sum has the
      #shape (Batch, Times)
      mean=torch.sum(x_zeros,dim=2)/self.features
      
      #Calculate variance
      #Reshape mean to allow substraction shape (Batch, Times, Features)
      mean_long=torch.unsqueeze(mean,dim=2)
      mean_long=mean_long.expand(-1,-1,self.features)
      
      #Calculate variance which has shape (Batch, Times)
      var=torch.sum(torch.square((x_zeros-mean_long)),dim=2)/self.features
      var=torch.sqrt(var+self.eps)
      
      var_long=torch.unsqueeze(var,dim=2)
      var_long=var_long.expand(-1,-1,self.features)
      #var_long=var_long+self.eps
      
      #Calculate normalized output
      gamma_long=self.gamma.expand(x.size(0),self.times,-1)
      normalized=gamma_long*(x_zeros-mean_long)/var_long
      
      #Insert padding values
      normalized=torch.where(condition=mask_features,input=self.pad_value,other=normalized)

      return normalized, seq_len,mask_times,mask_features

def get_layer_normalization(name,times, features,pad_value,eps=1e-5):
  if name=="layer_norm":
    return LayerNorm_with_Mask(times=times,features=features,pad_value=pad_value,eps=eps)
  elif name=="None":
    return identity_layer()
