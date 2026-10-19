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
from .Activations import get_act_fct


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
    
  def forward(self,x,mask):
    z=torch.where(condition=mask, input=self.pad_value_new, other=x)
    return z

class DenseAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self, features_in, features_out, times, encoder_dropout,decoder_dropout,decoder_share_weights,noise_factor,fe_type,fe_normalization,fe_act_fct, pad_value, orthogonal_method, te_n_layers=3):
        super().__init__()
        self.features_in = features_in
        self.times = times
        self.features_out = features_out
        self.noise_factor = noise_factor
        self.n_layers = te_n_layers
        self.decoder_share_weights=decoder_share_weights
        self.difference = self.features_in - self.features_out
        if isinstance(pad_value, torch.Tensor):
          self.register_buffer("pad_value",pad_value.clone().float())
        else:
          self.register_buffer("pad_value",torch.tensor(pad_value,dtype=torch.float))
        
        #Register type of model
        self.fe_type=fe_type
        print(self.fe_type)
        self.is_conv=(self.fe_type=="Conv")
        # Feature-Größen berechnen
        dims = []
        for i in range(self.n_layers + 1):
            fraction = i / self.n_layers
            dim = math.ceil(self.features_in - self.difference * fraction)
            dims.append(dim)
        
        # Times und Padding berechnen
        if self.is_conv:
          if self.times==2:
            self.seq_len=[4]
            self.padding=[2]
          elif self.times%2==0:
            self.seq_len=[self.times]
            self.padding=[0]
          else:
            self.seq_len=[int(self.times+1)]
            self.padding=[1]
            
          for i in range(0,self.n_layers):
            if self.seq_len[i]>2:
              if self.seq_len[i]%2!=0:
                self.seq_len.append(int((self.seq_len[i]+1)/2))
              else:
                self.seq_len.append(int(self.seq_len[i]/2))
              if self.seq_len[i+1]%2!=0:
                self.padding.append(1)
              else:
                self.padding.append(0)
            else:
              self.seq_len.append(2)
              self.padding.append(2)
          print(self.seq_len) 
          print(self.padding)
        else:
          self.seq_len=[self.times]*(self.n_layers+1)
          self.padding=[0]*(self.n_layers+1)

        # Residual-Verbindung als Linear-Layer ohne Bias
        self.residual_connections = torch.nn.ModuleList()

        # Mu und SD
        self.mu_layer=torch.nn.Linear(features_out, features_out, bias=False)
        self.sd_layer=torch.nn.Linear(features_out, features_out, bias=False)
        
        # Decoder head
        self.decoder_head=torch.nn.Linear(features_in, features_in, bias=True)

        # Encoder Dense-Layer
        self.encoder_layers = torch.nn.ModuleList()
        self.encoder_lns = torch.nn.ModuleList()
        
        # Separe Liste für decoder parameter wenn !self.decoder_share_weights
        self.decoder_params=torch.nn.ParameterList()
        self.decoder_params_res=torch.nn.ParameterList()
        
        # Separate Liste für die eigenen Decoder-Biases
        self.decoder_biases = torch.nn.ParameterList()
        self.decoder_lns = torch.nn.ModuleList()
        
        #Separate list for activations
        self.activation_encoder = torch.nn.ModuleList()
        self.activation_decoder = torch.nn.ModuleList()
        
        #Separate list for dropout
        self.dropout_encoder = torch.nn.ModuleList()
        self.dropout_decoder = torch.nn.ModuleList()
        
        for i in range(self.n_layers):
            out_d = dims[i+1]
            in_d = dims[i]
            if self.fe_type=="Conv":
                layer=torch.nn.Conv1d(
                    in_channels=in_d, 
                    out_channels=out_d, 
                    kernel_size=2, 
                    stride=2, 
                    padding=0,
                    bias=True
                )
            elif self.fe_type=="Dense":
                layer=torch.nn.Linear(in_d, out_d, bias=True)
            torch.nn.init.orthogonal_(layer.weight)
            if orthogonal_method != "None" and self.fe_type=="Dense":
                torch.nn.utils.parametrizations.orthogonal(module=layer, name="weight", orthogonal_map=orthogonal_method)
            self.encoder_layers.append(layer)
            #Add decoder weights if !self.decoder_share_weights
            if not self.decoder_share_weights:
                decoder_weights=layer.weight.clone()
                self.decoder_params.append(decoder_weights)
            
            #Residual connection
            if self.fe_type=="Conv":
              res_connect=torch.nn.Conv1d(
                    in_channels=in_d, 
                    out_channels=out_d, 
                    kernel_size=2, 
                    stride=2, 
                    padding=0,
                    bias=False
                )
            elif self.fe_type=="Dense":
              res_connect=torch.nn.Linear(in_d, out_d, bias=False)
            self.residual_connections.append(res_connect)
            if not self.decoder_share_weights:
              decoder_params_res_weights=layer.weight.clone()
              self.decoder_params_res.append(decoder_params_res_weights)
            
            # Eigener Decoder-Bias: Da der Decoder rückwärts läuft und die Gewichte transponiert, 
            # ist die Ausgangsdimension des entsprechenden Decoder-Schritts gleich der Eingangsdimension `in_d` des Encoders.
            dec_bias = torch.nn.Parameter(torch.zeros(in_d))
            self.decoder_biases.append(dec_bias)

            #Normalizer
            self.encoder_lns.append(
              get_layer_normalization(
                name=fe_normalization,
                times=self.seq_len[i], 
                features=in_d,
                pad_value=0.0,
                eps=1e-6
              )
            )
            self.decoder_lns.append(
              get_layer_normalization(
                name=fe_normalization,
                times=self.seq_len[i], 
                features=out_d,
                pad_value=0.0,
                eps=1e-6
              )
            )
            
            #Activations  
            self.activation_encoder.append(
              get_act_fct(name=fe_act_fct)
            )
            self.activation_decoder.append(
              get_act_fct(name=fe_act_fct)
            )
            
            #Dropout
            self.dropout_encoder.append(
              torch.nn.Dropout(p=encoder_dropout)
            )
            self.dropout_decoder.append(
              torch.nn.Dropout(p=decoder_dropout)
            )

    def forward(self, x, encoder_mode=False):
        mask = self.get_mask(x)
        x = x*~mask
        if encoder_mode:
            hidden_states = self.encoder(x,mask)
            mu=self.mu_layer(hidden_states)
            latent_space=mu
            latent_space=self.add_pad_value(latent_space,mask[:,0:self.seq_len[-1],:])
            return latent_space
        else:
            hidden_states = self.encoder(x,mask)

            mu=self.mu_layer(hidden_states)
            log_var=self.sd_layer(hidden_states)

            std=torch.exp(0.5 * log_var)
            eps = torch.randn_like(std)
            
            latent_space=mu + eps * std
            latent_space=latent_space*~mask[:,0:self.seq_len[-1],:]
            
            target = self.decoder(
                output_mask=mask,
                y=latent_space
            )
            target=self.add_pad_value(target,mask)
            return target, latent_space,mu,log_var,mask,mask[:,0:self.seq_len[-1],:]

    def encoder(self, x,mask):
        if self.training:
            x_noise = x + self.add_noise(x)
            x_noise = ~mask * x_noise
            y = x_noise
        else:
            x_noise = x
            y = x
            
        for idx, layer in enumerate(self.encoder_layers):
            y_start=y
            y,_ =  self.encoder_lns[idx](y,torch.squeeze(mask[:,0:self.seq_len[idx],:],dim=2))
            if self.is_conv:
              y=torch.nn.functional.pad(y, pad=(0,0,self.padding[idx],0), mode='constant', value=0.0)
              y=torch.permute(y,(0,2,1))
              y = layer(y)
              y=torch.permute(y,(0,2,1))
            else:
              y = layer(y)
            y = self.activation_encoder[idx](y)
            y=self.dropout_encoder[idx](y)
            if self.is_conv:
              y_start=torch.nn.functional.pad(y_start, pad=(0,0,self.padding[idx],0), mode='constant', value=0.0)
              y_start=torch.permute(y_start,(0,2,1))
              y=torch.permute(y,(0,2,1))
              res=self.residual_connections[idx](y_start)
              #res=self.dropout_encoder[idx](res)
              y=y+res
              y=torch.permute(y,(0,2,1))
            else:
              res=self.residual_connections[idx](y_start)
              #res=self.dropout_encoder[idx](res)
              y=y+res
            y = y * ~mask[:,0:self.seq_len[idx+1],:]
        return y

    def decoder(self, output_mask, y):
        latent_space = y
        # Umkehrung der Layer und Biases für den Decoder
        reversed_layers = list(reversed(self.encoder_layers))
        reversed_dec_biases = list(reversed(self.decoder_biases))
        decoder_lns_rev=list(reversed(self.decoder_lns))
        dropout_rev=list(reversed(self.dropout_decoder))
        activation_rev=list(reversed(self.activation_decoder))
        residual_connections_rev=list(reversed(self.residual_connections))
        padding_rev=list(reversed(self.padding))
        seq_len_rev=list(reversed(self.seq_len))
        seq_len_rev.append(self.times)
        decoder_weights_rev=list(reversed(self.decoder_params))
        decoder_weights_res_res=list(reversed(self.decoder_params_res))
        
        tmp_mask=torch.nn.functional.pad(output_mask, pad=(0,0,0,self.padding[0]), mode='constant', value=True)
        for idx, layer in enumerate(reversed_layers):
            b_dec=reversed_dec_biases[idx]
            if not self.decoder_share_weights:
              w=decoder_weights_rev[idx]
              rw=decoder_weights_res_res[idx]
            else:
              w=layer.weight
              rw=residual_connections_rev[idx].weight
            y_start=y
            y,_ =  decoder_lns_rev[idx](y,torch.squeeze(tmp_mask[:,0:seq_len_rev[idx],:],dim=2))
            if self.is_conv:
                y=torch.permute(y,(0,2,1))
                y=torch.nn.functional.conv_transpose1d(
                  y, 
                  weight=w, 
                  bias=b_dec, 
                  stride=2, 
                  padding=0, 
                  output_padding=0, 
                  groups=1, 
                  dilation=1
                )
                y=torch.permute(y,(0,2,1))
                y=y[:,0:seq_len_rev[idx+1],:]
            else:
              y=torch.nn.functional.linear(
                y, 
                weight=w.t(),
                bias=b_dec
              )
            y = activation_rev[idx](y)
            y=dropout_rev[idx](y)
            #Residual
            if self.is_conv:
              y=torch.permute(y,(0,2,1))
              y_start=torch.permute(y_start,(0,2,1))
              res=torch.nn.functional.conv_transpose1d(
                  y_start, 
                  weight=rw, 
                  stride=2, 
                  padding=0, 
                  output_padding=0, 
                  groups=1, 
                  dilation=1
                )
              res=res[:,:,0:seq_len_rev[idx+1]]
              #res=dropout_rev[idx](res)
              y=y+res
              y=torch.permute(y,(0,2,1))
            else:
              res=torch.nn.functional.linear(
                y_start, 
                weight=rw.t()
              )
              #res=dropout_rev[idx](res)
              y=y+res
            y = y * ~tmp_mask[:,0:seq_len_rev[idx+1],:]
        y=y[:,0:self.times,:]
        y=self.decoder_head(y)
        y = y * ~output_mask
        return y
      
    def get_mask(self, x):
        is_pad=(x==self.pad_value)
        mask=torch.all(is_pad, dim=2, keepdim=True)
        return mask.detach()

    def add_noise(self, x):
        noise = self.noise_factor * torch.rand(size=x.size(), device=x.device, dtype=x.dtype)
        return noise.detach()
    
    def add_pad_value(self,x,mask):
        y=torch.where(
              condition=mask, 
              input=self.pad_value, 
              other=x)
        return y
      
    def times_out(self):
      return min(self.seq_len)
    


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
