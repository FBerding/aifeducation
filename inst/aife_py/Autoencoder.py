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

class layer_switch_pad_values(torch.nn.Module):
  def __init__(self,pad_value_old,pad_value_new):
    super().__init__()
    
    if isinstance(pad_value_old, torch.Tensor):
      self.pad_value_old=pad_value_old.detach()
    else:
      self.pad_value_old=torch.tensor(pad_value_old)
      
    if isinstance(pad_value_new, torch.Tensor):
      self.pad_value_new=pad_value_new.detach()
    else:
      self.pad_value_new=torch.tensor(pad_value_new)
    
  def forward(self,x):
    features=x.size(2)
    time_sums=torch.sum(x,dim=2)
    mask=(time_sums==features*self.pad_value_old)
    
    mask=torch.reshape(torch.repeat_interleave(mask,repeats=features,dim=1),(x.size(dim=0),x.size(dim=1),features))
    
    z=torch.where(condition=mask, input=self.pad_value_new, other=x)
    return z

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
    
import torch
import math

import torch
import math

class ConvAutoencoder_with_Mask_PT2(torch.nn.Module):
    def __init__(self, features_in, features_out, time_in, time_out, noise_factor, pad_value, orthogonal_method, te_n_layers=3):
        super().__init__()
        self.injection_value = torch.nn.parameter.Parameter(data=torch.ones((1)))
        if isinstance(pad_value, torch.Tensor):
            self.pad_value = pad_value.detach()
        else:
            self.pad_value = torch.tensor(pad_value)
            
        self.features_in = features_in
        self.features_out = features_out
        self.time_in = time_in
        self.time_out = time_out
        self.noise_factor = noise_factor
        self.n_layers = te_n_layers
        
        # Calculate featurs per layer (Height H)
        self.feature_diff = self.features_in - self.features_out
        f_dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.features_in - self.feature_diff * fraction)
            f_dims.append(dim)
            
        # Calculate timepoints per feature (Width W)
        self.time_diff = self.time_in - self.time_out
        t_dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.time_in - self.time_diff * fraction)
            t_dims.append(dim)
            
        # Kernel, Strides und Output-Paddings
        self.layer_strides_h = []
        self.layer_kernels_h = []
        self.layer_strides_w = []
        self.layer_kernels_w = []
        
        self.decoder_output_paddings_h = []
        self.decoder_output_paddings_w = []
        
        self.encoder_layer_names = []
        
        for i in range(self.n_layers):
            # Feature-Dimension (H)
            in_f = f_dims[i]
            out_f = f_dims[i+1]
            stride_h = max(1, math.floor(in_f / out_f))
            kernel_h = in_f - (out_f - 1) * stride_h
            
            # Time-Dimension (W)
            in_t = t_dims[i]
            out_t = t_dims[i+1]
            stride_t = max(1, math.floor(in_t / out_t))
            kernel_t = in_t - (out_t - 1) * stride_t
            
            self.layer_strides_h.append(stride_h)
            self.layer_kernels_h.append(kernel_h)
            self.layer_strides_w.append(stride_t)
            self.layer_kernels_w.append(kernel_t)
            
            # Output Paddings 
            out_pad_h = in_f - ((out_f - 1) * stride_h + kernel_h)
            out_pad_w = in_t - ((out_t - 1) * stride_t + kernel_t)
            self.decoder_output_paddings_h.append(out_pad_h)
            self.decoder_output_paddings_w.append(out_pad_w)
            
            # Weights 
            # (out_channels=1, in_channels=1, kernel_size_h, kernel_size_w)
            name = f"param_w{i+1}"
            param = torch.nn.Parameter(torch.randn(1, 1, kernel_h, kernel_t))
            self.register_parameter(name, param)
            self.encoder_layer_names.append(name)
            
            if orthogonal_method != "None":
                torch.nn.utils.parametrizations.orthogonal(module=self, name=name, orthogonal_map=orthogonal_method)

    def forward(self, x, encoder_mode=False):
        # Input: (B, T, F)
        # Calculate Mask
        time_sums = torch.sum(x, dim=2)
        mask = (time_sums == self.features_in * self.pad_value)
        mask_features = torch.unsqueeze(mask, dim=2).expand(x.size()).detach()
        
        y = torch.where(mask_features, self.pad_value, x)
        
        if not encoder_mode:
            if self.training:
                y = y + self.add_noise(y)
            
            # Sort: (B, T, F) -> (B, F, T) -> (B, 1, F, T)
            # F = Heigth (H), T = Width (W)
            y = y.transpose(1, 2).unsqueeze(1)
            
            # encoder
            for idx, name in enumerate(self.encoder_layer_names):
                w = getattr(self, name)
                y = torch.nn.functional.conv2d(
                    y, weight=w, 
                    stride=(self.layer_strides_h[idx], self.layer_strides_w[idx]), 
                    padding=0
                )
            
            # Latent Space  (B, T, F) 
            y = y.squeeze(1) (B, F_out, T_out)
            latent_space = y.transpose(1, 2) (B, T_out, F_out)
            
            # decoder
            y = latent_space.transpose(1, 2).unsqueeze(1) # (B, 1, F_out, T_out)
            for idx in reversed(range(self.n_layers)):
                name = self.encoder_layer_names[idx]
                w = getattr(self, name)
                w_dec = w.transpose(0, 1)
                y = torch.nn.functional.conv_transpose2d(
                    y, weight=w_dec, 
                    stride=(self.layer_strides_h[idx], self.layer_strides_w[idx]), 
                    padding=0,
                    output_padding=(self.decoder_output_paddings_h[idx], self.decoder_output_paddings_w[idx])
                )
                
            # (B, T, F)
            y = y.squeeze(1).transpose(1, 2)
            y = torch.where(mask_features, self.pad_value, y)
            return y, latent_space
            
        else:
            y = x.transpose(1, 2).unsqueeze(1)
            for idx, name in enumerate(self.encoder_layer_names):
                w = getattr(self, name)
                y = torch.nn.functional.conv2d(
                    y, weight=w, 
                    stride=(self.layer_strides_h[idx], self.layer_strides_w[idx]), 
                    padding=0
                )
            y = y.squeeze(1).transpose(1, 2)
            return y

    def add_noise(self, x):
        noise = self.noise_factor * torch.rand(size=x.size(), device=x.device, dtype=x.dtype)
        return noise.detach()

import torch
import math

import torch
import math

class ConvAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self, features_in, features_out, time_in, time_out, noise_factor, pad_value, orthogonal_method, te_n_layers=3):
        super().__init__()
        self.injection_value = torch.nn.parameter.Parameter(data=torch.ones((1)))
        if isinstance(pad_value, torch.Tensor):
            self.pad_value = pad_value.detach()
        else:
            self.pad_value = torch.tensor(pad_value)
            
        self.features_in = features_in
        self.features_out = features_out
        self.time_in = time_in
        self.time_out = time_out
        self.noise_factor = noise_factor
        self.n_layers = te_n_layers
        
        # 1. Feature-Dimensionen pro Layer berechnen (Höhe)
        self.feature_diff = self.features_in - self.features_out
        f_dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.features_in - self.feature_diff * fraction)
            f_dims.append(dim)
            
        # 2. Zeit-Dimensionen pro Layer berechnen (Breite)
        self.time_diff = self.time_in - self.time_out
        t_dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.time_in - self.time_diff * fraction)
            t_dims.append(dim)
            
        # Parameter-Namen-Listen
        self.encoder_dense_f_names = []
        self.encoder_dense_t_names = []
        
        for i in range(self.n_layers):
            in_f = f_dims[i]
            out_f = f_dims[i+1]
            
            in_t = t_dims[i]
            out_t = t_dims[i+1]
            
            # --- DENSE LAYER FÜR FEATURES ---
            # Form: (out_features, in_features)
            df_w_name = f"dense_f_w{i+1}"
            df_b_name = f"dense_f_b{i+1}"
            param_dfw = torch.nn.Parameter(torch.randn(out_f, in_f))
            param_dfb = torch.nn.Parameter(torch.zeros(out_f))
            self.register_parameter(df_w_name, param_dfw)
            self.register_parameter(df_b_name, param_dfb)
            self.encoder_dense_f_names.append((df_w_name, df_b_name))
            
            # --- DENSE LAYER FÜR ZEIT ---
            # Form: (out_features, in_features) -> wirkt auf die transformierte T-Dimension
            dt_w_name = f"dense_t_w{i+1}"
            dt_b_name = f"dense_t_b{i+1}"
            param_dtw = torch.nn.Parameter(torch.randn(out_t, in_t))
            param_dtb = torch.nn.Parameter(torch.zeros(out_t))
            self.register_parameter(dt_w_name, param_dtw)
            self.register_parameter(dt_b_name, param_dtb)
            self.encoder_dense_t_names.append((dt_w_name, dt_b_name))
            
            if orthogonal_method != "None":
                torch.nn.utils.parametrizations.orthogonal(module=self, name=df_w_name, orthogonal_map=orthogonal_method)
                torch.nn.utils.parametrizations.orthogonal(module=self, name=dt_w_name, orthogonal_map=orthogonal_method)

    def forward(self, x, encoder_mode=False):
        # Input-Form: (B, T, F)
        time_sums = torch.sum(x, dim=2)
        mask = (time_sums == self.features_in * self.pad_value)
        mask_features = torch.unsqueeze(mask, dim=2).expand(x.size()).detach()
        
        y = torch.where(mask_features, self.injection_value, x)
        
        if not encoder_mode:
            if self.training:
                y = y + self.add_noise(y)
            
            # ---- ENCODER ----
            for idx in range(self.n_layers):
                # A) Feature-Reduktion via Dense (B, T, F_in) -> (B, T, F_out)
                df_w, df_b = self.encoder_dense_f_names[idx]
                y = torch.nn.functional.linear(y, weight=getattr(self, df_w), bias=getattr(self, df_b))
                
                # B) Zeit-Reduktion via Dense
                # Drehen auf (B, F, T), damit T am Ende steht
                y = y.transpose(1, 2)
                dt_w, dt_b = self.encoder_dense_t_names[idx]
                y = torch.nn.functional.linear(y, weight=getattr(self, dt_w), bias=getattr(self, dt_b)) # -> (B, F, T_out)
                # Zurückdrehen auf (B, T, F)
                y = y.transpose(1, 2)
            
            latent_space = y # Form: (B, T_latent, F_latent)
            
            # ---- DECODER ----
            for idx in reversed(range(self.n_layers)):
                # A) Transponierter Zeit-Schritt (T_out -> T_in)
                y = y.transpose(1, 2) # Drehen zu (B, F, T)
                dt_w, _ = self.encoder_dense_t_names[idx]
                # Invertieren durch Transponieren der Gewichtsmatrix (.t())
                y = torch.nn.functional.linear(y, weight=getattr(self, dt_w).t())
                y = y.transpose(1, 2) # Zurück zu (B, T, F)
                
                # B) Transponierter Feature-Schritt (F_out -> F_in)
                df_w, _ = self.encoder_dense_f_names[idx]
                y = torch.nn.functional.linear(y, weight=getattr(self, df_w).t())
                
            y = torch.where(mask_features, self.pad_value, y)
            return y, latent_space
            
        else:
            # Reiner Encoder-Mode
            for idx in range(self.n_layers):
                df_w, df_b = self.encoder_dense_f_names[idx]
                y = torch.nn.functional.linear(y, weight=getattr(self, df_w), bias=getattr(self, df_b))
                
                y = y.transpose(1, 2)
                dt_w, dt_b = self.encoder_dense_t_names[idx]
                y = torch.nn.functional.linear(y, weight=getattr(self, dt_w), bias=getattr(self, dt_b))
                y = y.transpose(1, 2)
            return y

    def add_noise(self, x):
        noise = self.noise_factor * torch.rand(size=x.size(), device=x.device, dtype=x.dtype)
        return noise.detach()




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
