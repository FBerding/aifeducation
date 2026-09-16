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

def calc_Correlation(x):
    batch_size, times, features = x.shape
    x_flat = x.reshape(batch_size * times, features) #(B*T,F)
    valid_mask = (torch.sum(x_flat, dim=1, keepdim=True) != 0).to(x_flat.dtype) #(B*T,1)
    n_cases = torch.sum(valid_mask) #()
    sum_x = torch.sum(x_flat, dim=0, keepdim=True) #(1,F)
    mean_x = sum_x / n_cases
    x_centered = (x_flat - mean_x) * valid_mask
    cov_matrix = torch.mm(x_centered.transpose(0, 1), x_centered)/(n_cases-1)
    
    std_dev = torch.sqrt(torch.diag(cov_matrix))
    std_matrix = torch.outer(std_dev, std_dev) + 1e-8
    corr_matrix = cov_matrix / std_matrix
    
    corr_squared = torch.square(corr_matrix)
    total_sum = torch.sum(corr_squared)-torch.sum(torch.diag(corr_squared, diagonal=0))
    valid_mask_final = (n_cases > 1).to(x.dtype)
    cov_sum = total_sum * valid_mask_final/features
    return cov_sum

class DenseAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self, features_in, features_out, noise_factor, pad_value, orthogonal_method, te_n_layers=3):
        super().__init__()
        self.features_in = features_in
        self.features_out = features_out
        self.noise_factor = noise_factor
        self.n_layers = te_n_layers
        self.difference = self.features_in - self.features_out
        #Calculate feature sizes
        dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.features_in - self.difference * fraction)
            dims.append(dim)
        #Create Weights
        self.encoder_layer_names = []
        for i in range(self.n_layers):
            name = f"param_w{i+1}"
            out_d = dims[i+1]
            in_d = dims[i]
            # Register
            param = torch.nn.Parameter(torch.randn(out_d, in_d))
            self.register_parameter(name, param)
            self.encoder_layer_names.append(name)
            # Apply orthogonal parametrizations
            if orthogonal_method != "None":
                torch.nn.utils.parametrizations.orthogonal(module=self, name=name, orthogonal_map=orthogonal_method)
        #Add Padding Layer
        if pad_value != 0:
            self.switch_pad_value_start = layer_switch_pad_values(pad_value_old=pad_value, pad_value_new=0)
            self.switch_pad_value_final = layer_switch_pad_values(pad_value_old=0, pad_value_new=pad_value)
        else:
            self.switch_pad_value_start = None
            self.switch_pad_value_final = None

    def forward(self, x, encoder_mode=False):
        # Switch padding value if necessary
        if self.switch_pad_value_start is not None:
            x = self.switch_pad_value_start(x)
        if encoder_mode == False:
            # Add noise
            if self.training:
                mask = self.get_mask(x,self.features_in)
                y = x + self.add_noise(x)
                y = ~mask * y
            else:
              y=x
            # Encoder Part
            for name in self.encoder_layer_names:
                w = getattr(self, name)
                y = torch.nn.functional.linear(y, weight=w)
            #Latent Space
            latent_space = y*~self.get_mask(x,self.features_out)
            # Decoder Part
            for name in reversed(self.encoder_layer_names):
                w = getattr(self, name)
                y = torch.nn.functional.linear(y, weight=torch.transpose(w, dim0=1, dim1=0))
            # Switch padding value back if necessary
            if self.switch_pad_value_start is not None:
                y = self.switch_pad_value_final(y)
            return y, latent_space    
        elif encoder_mode == True:
            # Encoder Part
            y=x
            for name in self.encoder_layer_names:
                w = getattr(self, name)
                y = torch.nn.functional.linear(y, weight=w)
            y=y*~self.get_mask(y,self.features_out)
            # Switch padding value back if necessary
            if self.switch_pad_value_start is not None:
                y = self.switch_pad_value_final(y)
            return y
    def get_mask(self, x,features):
      time_sums = torch.sum(x, dim=2,keepdim=True) #(B,T,1)
      mask = (time_sums == 0)
      mask=mask.expand((x.size(0),x.size(1),features))
      return mask.detach()
    def add_noise(self, x):
      noise = self.noise_factor * torch.rand(size=x.size(),device=x.device,dtype=x.dtype)
      return noise.detach()
    
class ConvAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self, features_in,features_out,noise_factor):
      super().__init__()
      self.features_in=features_in
      self.features_out=features_out
      self.noise_factor=noise_factor
      self.difference=self.features_in-self.features_out
      self.stride=1
      self.kernel_size=2
      
      #dilation of 1 means no dilation
      self.dilation=1
      
      self.param_w1=torch.nn.Parameter(torch.randn(math.ceil(self.features_in-self.difference*(1/2)),self.features_in,self.kernel_size))
      self.param_w2=torch.nn.Parameter(torch.randn(self.features_out,math.ceil(self.features_in-self.difference*(1/2)),self.kernel_size))
      
      self.sequence_reduction=torch.nn.AvgPool1d(kernel_size=(self.kernel_size),stride=1,padding=0)
      if not orthogonal_method=="None":
        torch.nn.utils.parametrizations.orthogonal(self, "param_w1",orthogonal_map="householder")
        torch.nn.utils.parametrizations.orthogonal(self, "param_w2",orthogonal_map="householder")

    def forward(self, x, encoder_mode=False, return_scs=False):
      if encoder_mode==False:
        #Add noise
        if self.training==True:
          mask=self.get_mask(x)
          x=x+self.add_noise(x)
          x=~mask*x
        
        #Change position of time and features
        x=torch.transpose(x, dim0=1, dim1=2)
        
        #Encoder
        x=torch.nn.functional.tanh(torch.nn.functional.conv1d(x,weight=self.param_w1,stride=self.stride,padding='same',dilation=self.dilation))

        #Latent Space
        latent_space=torch.nn.functional.tanh(torch.nn.functional.conv1d(x,weight=self.param_w2,stride=self.stride,padding='same',dilation=self.dilation))
        latent_space=torch.transpose(latent_space, dim0=1, dim1=2)
        latent_space=~self.get_mask(latent_space)*latent_space
        latent_space=torch.transpose(latent_space, dim0=1, dim1=2)

        #Decoder
        x=torch.nn.functional.tanh(torch.nn.functional.conv_transpose1d(latent_space,weight=self.param_w2,stride=self.stride,padding=0,output_padding=0,dilation=self.dilation))
        x=self.sequence_reduction(x)
        x=torch.nn.functional.tanh(torch.nn.functional.conv_transpose1d(x,weight=self.param_w1,stride=self.stride,padding=0,output_padding=0,dilation=self.dilation))
        x=self.sequence_reduction(x)
        
        #Change position of time and features
        x=torch.transpose(x, dim0=1, dim1=2)
        
        if return_scs==False:
          return x
        else:
          latent_space=torch.transpose(latent_space, dim0=1, dim1=2)
          return x, calc_SquaredCovSum(latent_space)
      elif encoder_mode==True:
        #Change position of time and features
        x=torch.transpose(x, dim0=1, dim1=2)
        #Encoder
        x=torch.nn.functional.tanh(torch.nn.functional.conv1d(x,weight=self.param_w1,stride=self.stride,padding='same'))

        #Latent Space
        x=torch.nn.functional.tanh(torch.nn.functional.conv1d(x,weight=self.param_w2,stride=self.stride,padding='same'))
        #Change position of time and features
        x=torch.transpose(x, dim0=1, dim1=2)
        x=~self.get_mask(x)*x
        return x
      
    def get_mask(self,x):
      device=('cuda' if torch.cuda.is_available() else 'cpu')
      time_sums=torch.sum(x,dim=2)
      mask=(time_sums==0)
      mask_long=torch.reshape(torch.repeat_interleave(mask,repeats=x.size(dim=2),dim=1),(x.size(dim=0),x.size(dim=1),x.size(dim=2)))
      mask_long=mask_long.to(device)
      return mask_long
    def add_noise(self, x):
      device=('cuda' if torch.cuda.is_available() else 'cpu')
      noise=self.noise_factor*torch.rand(size=x.size())
      noise=noise.to(device)
      return(noise)


@torch.inference_mode()
def TeFeatureExtractorBatchExtract(model,dataset,batch_size):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    dtype=torch.float
    model.to(device,dtype=dtype)
  else:
    dtype=torch.float
    model.to(device,dtype=dtype)
    
  model.eval()
  predictionloader=torch.utils.data.DataLoader(
    dataset,
    batch_size=batch_size,
    shuffle=False)

  iteration=0
  for batch in predictionloader:
    inputs=batch["input"]
    inputs = inputs.to(device,dtype=dtype)
    predictions=model(inputs,encoder_mode=True)
    
    if iteration==0:
      predictions_list=predictions.to("cpu")
    else:
      predictions_list=torch.concatenate((predictions_list,predictions.to("cpu")), axis=0, out=None)
    iteration+=1
  
  return predictions_list
