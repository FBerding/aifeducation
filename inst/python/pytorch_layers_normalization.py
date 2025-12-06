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
# True indicates that the sequence or feature is padded. If True these values should not be part 
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

#BatchNorm_with_Mask------------------------------------------------------------
#Layer generating the Batch Norm for sequential data.
# Returns a list with the following tensors
# * Input tensor
# * Sequence length of the tensors shape (Batch)
# * mask_times Mask on the level of complete sequences shape (Batch, Times)
# * mask_features Mask on the level of single features shape (Bath, Times, Features)
# True indicates that the sequence or feature is padded. If True these values should not be part 
# of further computations
class BatchNorm_with_Mask(torch.nn.Module):
    def __init__(self, features,pad_value,eps=1e-5,alpha=0.1):
      super().__init__()
      self.eps=eps
      self.alpha=alpha
      self.features = features
      if isinstance(pad_value, torch.Tensor):
        self.pad_value=pad_value.detach()
      else:
        self.pad_value=torch.tensor(pad_value)
      self.gamma = torch.nn.Parameter(torch.ones(1, 1, self.features))
      self.beta = torch.nn.Parameter(torch.zeros(1, 1, self.features))
      
      self.running_mean=torch.zeros((1, 1, self.features))
      self.running_variance=torch.ones((1, 1, self.features))

    def forward(self, x,seq_len,mask_times,mask_features):
      #Set padding value to zero for correct sum
      x_zeros=x*(~mask_features)
      gamma_expanded=self.gamma.expand(x_zeros.size(0),x_zeros.size(1),x_zeros.size(2))
      beta_expanded=self.beta.expand(x_zeros.size(0),x_zeros.size(1),x_zeros.size(2))
      if self.training==True and x_zeros.size(0)>=2:
        #Number of not padded elements
        n_elements=torch.sum(~mask_features,dim=(0,1))
        #Calc Batch Mean for every feature. Size is (Feature)
        batch_mean=torch.sum(x_zeros,dim=(0,1))/n_elements
        #Calc Batch Variance
        batch_variance=torch.pow(x_zeros-torch.unsqueeze(torch.unsqueeze(batch_mean,dim=0),dim=0).expand(x_zeros.size(0),x_zeros.size(1),x_zeros.size(2)),2)
        batch_variance=torch.sum(batch_variance,dim=(0,1))/n_elements
        #Update running mean and variance
        self.running_mean=(1-self.alpha)*self.running_mean+self.alpha*torch.unsqueeze(torch.unsqueeze(batch_mean.detach(),dim=0),dim=0)
        self.running_variance=(1-self.alpha)*self.running_variance+self.alpha*torch.unsqueeze(torch.unsqueeze(batch_variance.detach(),dim=0),dim=0)*(x_zeros.size(0)/(x_zeros.size(0)-1))
        #Normalize Scale and shift
        y=gamma_expanded*(x_zeros-batch_mean)/(torch.sqrt(batch_variance)+self.eps)+beta_expanded
      else:
        #Normalize Scale and shift
        y=gamma_expanded*(x_zeros- self.running_mean)/(torch.sqrt(self.running_variance)+self.eps)+beta_expanded
      #Insert padding values
      normalized=y.masked_fill_(mask=mask_features, value=self.pad_value)
      #Return results
      return normalized, seq_len, mask_times, mask_features

def get_layer_normalization(name,times, features,pad_value,eps=1e-5):
  if name=="LayerNorm":
    return LayerNorm_with_Mask(times=times,features=features,pad_value=pad_value,eps=eps)
  elif name=="BatchNorm":
    return BatchNorm_with_Mask(features=features,pad_value=pad_value,eps=eps,alpha=0.1)
  elif name=="None":
    return identity_layer(pad_value=pad_value,apply_masking=True)
