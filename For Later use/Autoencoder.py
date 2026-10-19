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

from .Normalizers import get_layer_normalization

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
    def __init__(self, features_in, features_out, times, noise_factor, pad_value, orthogonal_method, te_n_layers=3):
        super().__init__()
        self.features_in = features_in
        self.times = times
        self.features_out = features_out
        self.noise_factor = noise_factor
        self.n_layers = te_n_layers
        self.difference = self.features_in - self.features_out
        if isinstance(pad_value, torch.Tensor):
          self.register_buffer("pad_value",pad_value.clone().float())
        else:
          self.register_buffer("pad_value",torch.tensor(pad_value,dtype=torch.float))
        
        # Feature-Größen berechnen
        dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.features_in - self.difference * fraction)
            dims.append(dim)
            
        # Residual-Verbindung als Linear-Layer ohne Bias
        self.residual = torch.nn.Linear(features_in, features_out, bias=False)
        torch.nn.init.uniform_(self.residual.weight, 0.0, 1.0)
        
        # Mu und SD
        self.mu_layer=torch.nn.Linear(features_out, features_out, bias=False)
        self.sd_layer=torch.nn.Linear(features_out, features_out, bias=False)
        
        # Encoder Dense-Layer
        self.encoder_layers = torch.nn.ModuleList()
        self.encoder_lns = torch.nn.ModuleList()
        # Separate Liste für die eigenen Decoder-Biases
        self.decoder_biases = torch.nn.ParameterList()
        self.decoder_lns = torch.nn.ModuleList()
        self.activation = {}
        
        for i in range(self.n_layers):
            out_d = dims[i+1]
            in_d = dims[i]
            
            # Encoder Layer (enthält eigenen Weight und Encoder-Bias)
            layer = torch.nn.Linear(in_features=in_d, out_features=out_d, bias=True)
            torch.nn.init.orthogonal_(layer.weight)
            torch.nn.init.zeros_(layer.bias)
            
            if orthogonal_method != "None":
                torch.nn.utils.parametrizations.orthogonal(module=layer, name="weight", orthogonal_map=orthogonal_method)
                
            self.encoder_layers.append(layer)
            
            # Eigener Decoder-Bias: Da der Decoder rückwärts läuft und die Gewichte transponiert, 
            # ist die Ausgangsdimension des entsprechenden Decoder-Schritts gleich der Eingangsdimension `in_d` des Encoders.
            dec_bias = torch.nn.Parameter(torch.zeros(in_d))
            self.decoder_biases.append(dec_bias)
            
            if i > 0 and i < self.n_layers - 1:
              lns_name="LayerNorm"
            else:
              lns_name="None"
            #Normalizer
            self.encoder_lns.append(
              get_layer_normalization(
                name=lns_name,
                times=self.times, 
                features=in_d,
                pad_value=0.0,
                eps=1e-6
              )
            )
            self.decoder_lns.append(
              get_layer_normalization(
                name=lns_name,
                times=self.times, 
                features=out_d,
                pad_value=0.0,
                eps=1e-6
              )
            )
            
        # Padding Layer
        if pad_value != 0:
            self.switch_pad_value_start = layer_switch_pad_values(pad_value_old=pad_value, pad_value_new=0)
            self.switch_pad_value_final = layer_switch_pad_values(pad_value_old=0, pad_value_new=pad_value)
        else:
            self.switch_pad_value_start = None
            self.switch_pad_value_final = None

    def forward(self, x, encoder_mode=False):
        mask = self.get_mask(x)
        if self.switch_pad_value_start is not None:
            x = self.switch_pad_value_start(x)
        if encoder_mode:
            hidden_states = self.encoder(x,mask)
            mu=self.mu_layer(hidden_states)
            latent_space=mu
            if self.switch_pad_value_start is not None:
                latent_space = self.switch_pad_value_final(latent_space)
            return latent_space
        else:
            hidden_states = self.encoder(x,mask)
            
            mu=self.mu_layer(hidden_states)
            log_var=self.sd_layer(hidden_states)
            
            std=torch.exp(0.5 * log_var)
            eps = torch.randn_like(std)
            latent_space=mu + eps * std
            latent_space=latent_space*~mask
            target = self.decoder(
                output_mask=mask,
                y=latent_space
            )
            if self.switch_pad_value_start is not None:
                target = self.switch_pad_value_final(target)
            return target, latent_space,mu,log_var,mask  

    def encoder(self, x,mask):
        if self.training:
            x_noise = x + self.add_noise(x)
            x_noise = ~mask * x_noise
            y = x_noise
        else:
            x_noise = x
            y = x
            
        for idx, layer in enumerate(self.encoder_layers):
            if idx > 0 and idx < self.n_layers - 1:
                y,_ =  self.encoder_lns[idx](y,torch.squeeze(mask,dim=2))
                y = layer(y)
                y = torch.nn.functional.gelu(y)
            else:
                y = layer(y)
            y = y * ~mask
            
        y = self.residual(x_noise) + y
        y = torch.nn.functional.normalize(y, p=2, dim=-1)
        y = y * ~mask
        return y

    def decoder(self, output_mask, y):
        latent_space = y
        
        # Umkehrung der Layer und Biases für den Decoder
        reversed_layers = list(reversed(self.encoder_layers))
        reversed_dec_biases = list(reversed(self.decoder_biases))
        decoder_lns_rev=list(reversed(self.decoder_lns))
        
        for idx, layer in enumerate(reversed_layers):
            w = layer.weight
            b_dec = reversed_dec_biases[idx] # Der eigene Decoder-Bias für diesen Schritt
            
            if idx > 0 and idx < self.n_layers - 1:
                y,_ =  decoder_lns_rev[idx](y,torch.squeeze(output_mask,dim=2))
                # Der eigene Bias wird direkt in die lineare Operation übergeben (y * w.T + b_dec)
                y = torch.nn.functional.linear(y, weight=w.t(), bias=b_dec)
                y = torch.nn.functional.gelu(y)
            else:
                y = torch.nn.functional.linear(y, weight=w.t(), bias=b_dec)
            y = y * ~output_mask
            
        y = torch.nn.functional.linear(latent_space, weight=self.residual.weight.t()) + y  
        return y
      
    def get_mask(self, x):
        is_pad=(x==self.pad_value)
        mask=torch.all(is_pad, dim=2, keepdim=True)
        return mask.detach()

    def add_noise(self, x):
        noise = self.noise_factor * torch.rand(size=x.size(), device=x.device, dtype=x.dtype)
        return noise.detach()
    

class DenseAutoencoder_with_Mask_PT_Times(torch.nn.Module):
    def __init__(self, features_in, features_out, time_in, time_out, noise_factor, pad_value, orthogonal_method, te_n_layers=3):
        super().__init__()
        #self.injection_value = torch.nn.parameter.Parameter(data=torch.ones((1)))
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
              #Add Padding Layer
        if pad_value != 0:
            self.switch_pad_value_start = layer_switch_pad_values(pad_value_old=pad_value, pad_value_new=0.0)
            self.switch_pad_value_final = layer_switch_pad_values(pad_value_old=0.0, pad_value_new=pad_value)
        else:
            self.switch_pad_value_start = None
            self.switch_pad_value_final = None

    def forward(self, x, encoder_mode=False):
        # Input-Form: (B, T, F)
        time_sums = torch.sum(x, dim=2)
        mask = (time_sums == self.features_in * self.pad_value)
        mask_features = torch.unsqueeze(mask, dim=2).expand(x.size()).detach()
        
        # Switch padding value if necessary
        if self.switch_pad_value_start is not None:
            x = self.switch_pad_value_start(x)
        
        if not encoder_mode:
            if self.training:
              y = x + self.add_noise(x)
            else:
              y=x
            #y = torch.where(mask_features, self.injection_value, y)
            y = torch.where(mask_features, 0.0, y)
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
            if self.switch_pad_value_start is not None:
              y = self.switch_pad_value_final(y)
            return y, latent_space
            
        else:
            # Reiner Encoder-Mode
            y = torch.where(mask_features, 0.0, x)
            for idx in range(self.n_layers):
                df_w, df_b = self.encoder_dense_f_names[idx]
                y = torch.nn.functional.linear(y, weight=getattr(self, df_w), bias=getattr(self, df_b))
      
                y = y.transpose(1, 2)
                dt_w, dt_b = self.encoder_dense_t_names[idx]
                y = torch.nn.functional.linear(y, weight=getattr(self, dt_w), bias=getattr(self, dt_b))
                y = y.transpose(1, 2)
                
                # Switch padding value back if necessary
                if self.switch_pad_value_start is not None:
                  y = self.switch_pad_value_final(y)
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
