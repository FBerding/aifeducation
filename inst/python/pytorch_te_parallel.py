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


class masking_layer(torch.nn.Module):
  #def __init__():
  def forward(self,x):
    features=x.size()[-1]
    device=('cuda' if torch.cuda.is_available() else 'cpu')
    time_sums=torch.sum(x,dim=2)
    #Get mask on the level of sequences/times
    mask_times=(time_sums==0)
    #Get corresponding sequence length
    seq_len=torch.sum(~mask_times,dim=1)
    #Get mask for the level of features
    n_elements=seq_len*features
    mask_features=torch.reshape(torch.repeat_interleave(mask_times,repeats=features,dim=1),(x.size(dim=0),x.size(dim=1),features))
    #Bring values to device
    seq_len=seq_len.to(device)
    mask_times=mask_times.to(device)
    mask_features=mask_features.to(device)
    return seq_len, mask_times, mask_features

class LayerNorm_with_Mask(torch.nn.Module):
    def __init__(self, times, features,eps=1e-5):
      super().__init__()
      self.eps=eps
      self.times=times
      self.features = features
      self.gamma = torch.nn.Parameter(torch.ones(1, 1, self.features))
      self.beta = torch.nn.Parameter(torch.zeros(1, 1, self.features))

    def forward(self, x,seq_len,mask_features):
      n_elements=seq_len*self.features

      mean=torch.sum(torch.sum(x,dim=1),dim=1)/n_elements
      mean=torch.reshape(mean,(x.size(dim=0),1))
      mean_long=torch.reshape(mean.repeat(1,self.times*self.features),(x.size(dim=0),self.times,self.features))

      var=torch.square((x-mean_long)*(~mask_features))

      normalized=((~mask_features)*(x-mean_long)/torch.sqrt(var+self.eps))*self.gamma+self.beta
      return normalized

class PackAndMasking(torch.nn.Module):
  def __init__(self):
    super().__init__()
  
  def forward(self,x,seq_len):
    return torch.nn.utils.rnn.pack_padded_sequence(
    input=x,
    lengths=seq_len.to("cpu",dtype=torch.int64),
    enforce_sorted=False, 
    batch_first=True)

class UnPackAndMasking(torch.nn.Module):
  def __init__(self,sequence_length):
    super().__init__()
    self.sequence_length=sequence_length
    
  def forward(self,x):
    return torch.nn.utils.rnn.pad_packed_sequence(
    sequence=x[0],
    total_length=self.sequence_length,
    padding_value=0.0,
    batch_first=True)
    
class FourierEncoder(torch.nn.Module):
  def __init__(self, dense_dim, features, dropout_rate,act_fct="elu",bias=True,parametrizations="None"):
    super().__init__()
    
    self.dense_dim=dense_dim
    self.dropout_rate=dropout_rate
    self.features=features
    self.bias=bias
    self.parametrizations=parametrizations
    self.act_fct=act_fct
   
    self.attention=FourierTransformation_PT()
    self.dropout=torch.nn.Dropout(p=dropout_rate)
    self.layernorm_1=torch.nn.LayerNorm(normalized_shape=self.features, eps=1e-05, elementwise_affine=True, bias=True, device=None, dtype=None)
    self.dense_proj=torch.nn.Sequential(
      torch.nn.Linear(in_features=self.features,out_features=self.dense_dim,bias=self.bias),
      get_act_fct(self.act_fct),
      torch.nn.Linear(in_features=self.dense_dim,out_features=self.features,bias=self.bias)
    )
    self.layernorm_2=torch.nn.LayerNorm(normalized_shape=self.features, eps=1e-05, elementwise_affine=True, bias=True, device=None, dtype=None)
    
    if self.parametrizations=="orthogonal":
      torch.nn.utils.parametrizations.orthogonal(module=self.dense_proj[0], name='weight')
      torch.nn.utils.parametrizations.orthogonal(module=self.dense_proj[2], name='weight')
    elif self.parametrizations=="weight_norm":
      torch.nn.utils.parametrizations.weight_norm(module=self.dense_proj[0], name='weight', dim=0)
      torch.nn.utils.parametrizations.weight_norm(module=self.dense_proj[2], name='weight', dim=0)
    elif self.parametrizations=="spectral_norm":
        torch.nn.utils.spectral_norm(module=self.dense_proj[0], name='weight', n_power_iterations=1, eps=1e-12, dim=None)
        torch.nn.utils.spectral_norm(module=self.dense_proj[2], name='weight', n_power_iterations=1, eps=1e-12, dim=None)

  def forward(self,x,seq_len=None,mask_times=None,mask_features=None):
    attention_output=self.attention(x)
    attention_output=self.dropout(attention_output)
    proj_input=self.layernorm_1(attention_output)
    proj_output=self.dense_proj(proj_input)
    return self.layernorm_2(proj_input+proj_output)
  
class TransformerEncoder(torch.nn.Module):
  def __init__(self, embed_dim, dense_dim, num_heads, dropout_rate,times,act_fct="elu",bias=True,parametrizations="None"):
    super().__init__()
    self.embed_dim=embed_dim
    self.dense_dim=dense_dim
    self.num_heads=num_heads
    self.dropout_rate=dropout_rate
    self.bias=bias
    self.parametrizations=parametrizations
    self.times=times
    self.act_fct=act_fct

    self.attention=torch.nn.MultiheadAttention(
      embed_dim=self.embed_dim,
      num_heads=self.num_heads,
      dropout=0,
      batch_first=True)
    self.dropout=torch.nn.Dropout(p=dropout_rate)
    self.layernorm_1=LayerNorm_with_Mask(times=self.times,features=self.embed_dim)
    self.dense_1=torch.nn.Linear(in_features=self.embed_dim,out_features=self.dense_dim,bias=self.bias)
    self.act_fct= get_act_fct(self.act_fct)
    self.dense_2=torch.nn.Linear(in_features=self.dense_dim,out_features=self.embed_dim,bias=self.bias)
    #self.dense_proj=torch.nn.Sequential(
    #  torch.nn.Linear(in_features=self.embed_dim,out_features=self.dense_dim,bias=self.bias),
    #  torch.nn.GELU(),
    #  torch.nn.Linear(in_features=self.dense_dim,out_features=self.embed_dim,bias=self.bias))
    self.layernorm_2=LayerNorm_with_Mask(times=self.times,features=self.embed_dim)
  
    if self.parametrizations=="orthogonal":
      torch.nn.utils.parametrizations.orthogonal(module=self.dense_1, name='weight')
      torch.nn.utils.parametrizations.orthogonal(module=self.dense_2, name='weight')
    elif self.parametrizations=="weight_norm":
      torch.nn.utils.parametrizations.weight_norm(module=self.dense_1, name='weight', dim=0)
      torch.nn.utils.parametrizations.weight_norm(module=self.dense_2, name='weight', dim=0)
    elif self.parametrizations=="spectral_norm":
        torch.nn.utils.spectral_norm(module=self.dense_1, name='weight', n_power_iterations=1, eps=1e-12, dim=None)
        torch.nn.utils.spectral_norm(module=self.dense_2, name='weight', n_power_iterations=1, eps=1e-12, dim=None)

  def forward(self,x,seq_len,mask_times,mask_features):
    attention_output=self.attention(
      query=x,
      key=x,
      value=x,
      key_padding_mask=mask_times)[0]
    attention_output=self.dropout(attention_output)
    
    proj_input=self.layernorm_1(x=attention_output,seq_len=seq_len,mask_features=mask_features)
    
    mask_intermediate=self.calc_mask_intermediate(mask_features)
    
    proj_intermediate=(~mask_intermediate)*self.dense_1(proj_input)
    proj_intermediate=self.act_fct(proj_intermediate)
    proj_output=(~mask_features)*self.dense_2(proj_intermediate)
    return self.layernorm_2(x=proj_input+proj_output,seq_len=seq_len,mask_features=mask_features)
  
  def calc_mask_intermediate(self,mask_features):
    if self.embed_dim>=self.dense_dim:
      mask_features_new=torch.index_select(mask_features,2,torch.arange(start=0, end=self.dense_dim))
    else:
      tmp_mask=torch.index_select(mask_features,2,torch.arange(start=0, end=1))
      mask_features_new=tmp_mask.repeat(1,1,self.embed_dim)
    return mask_features_new
    
class conv_layer_2d_with_pooling(torch.nn.Module):
  def __init__(self,in_channels, out_channels, kernel_size,kernel_size_pooling, times, features, device=None, dtype=None,bias=True,parametrizations="None"):
    super().__init__()
    self.times=times
    self.features=features
    self.parametrizations=parametrizations
    
    self.in_channels=in_channels
    self.out_channels=out_channels
    self.kernel_size=kernel_size
    self.stride=1
    self.dilation=1
    self.device=device 
    self.dtype=dtype
    self.bias=bias
   
    self.padding=self.calc_padding()
    
    self.kernel_size_pooling=kernel_size_pooling
    self.stride_pooling=kernel_size_pooling
    self.padding_pooling=0
    
    self.conv_layer=torch.nn.Conv2d(in_channels=self.in_channels, out_channels=self.out_channels, kernel_size=self.kernel_size, stride=self.stride, padding=0, dilation=self.dilation, groups=1, bias=self.bias, padding_mode='zeros', device=self.device, dtype=self.dtype)
    self.pooling_layer=torch.nn.MaxPool2d(kernel_size=self.kernel_size_pooling, stride=self.stride_pooling, padding=0, dilation=self.dilation, return_indices=False, ceil_mode=False)
    
    if self.parametrizations=="orthogonal":
      torch.nn.utils.parametrizations.orthogonal(module=self.conv_layer, name='weight')

  def forward(self, x):
    y=torch.nn.functional.pad(input=x,pad=self.padding,value=0)
    y=self.conv_layer(y)
    y=self.pooling_layer(y)
    return y
  
  def calc_output_size(self):
    h_out=math.floor((self.times+2*self.padding_pooling-self.dilation*(self.kernel_size_pooling-1)-1)/self.stride_pooling+1)
    tmp=(self.features+2*self.padding_pooling-self.dilation*(self.kernel_size_pooling-1)-1)/self.stride_pooling+1
    w_out=math.floor(tmp)
    return h_out, w_out
  
  def calc_padding(self):
    padding_h=self.kernel_size-self.stride
    padding_w=self.kernel_size-self.stride
    return 0,padding_w, 0,padding_h

class conv_layer_stack(torch.nn.Module): 
    def __init__(self,in_channels, out_channels, kernel_size, kernel_size_pooling, times, features, device=None, dtype=None,bias=True,parametrizations="None"):
      super().__init__()
      self.in_channels=in_channels
      self.out_channels=out_channels
      self.kernel_size=kernel_size
      self.device=device 
      self.dtype=dtype
      self.bias=bias
      self.parametrizations=parametrizations

      self.kernel_size_pooling=kernel_size_pooling
      self.stride_pooling=kernel_size_pooling
      
      self.times=times
      self.features=features
      
      self.n_layers=math.floor(torch.log(torch.tensor(min(times,features)))/torch.log(torch.tensor(kernel_size_pooling)))-1
      layer_list=torch.nn.ModuleDict()
      
      #Add inital layer
      layer_list.update({"initial_layer":conv_layer_2d_with_pooling(in_channels=self.in_channels, out_channels=self.out_channels, kernel_size=self.kernel_size,kernel_size_pooling=self.kernel_size_pooling,times=self.times,features=self.features, device=self.device, dtype=self.dtype,bias=self.bias,parametrizations=self.parametrizations)})
      #Add all other layers
      for r in range(self.n_layers):
        selected_layer=list(layer_list.values())[r]
        shape=selected_layer.calc_output_size()
        layer_list.update({"conv_pool_"+str(r+1):conv_layer_2d_with_pooling(in_channels=self.out_channels, out_channels=self.out_channels, kernel_size=self.kernel_size,kernel_size_pooling=self.kernel_size_pooling,times=shape[0],features=shape[1], device=self.device, dtype=self.dtype,bias=self.bias,parametrizations=self.parametrizations)})
        layer_list.update({"conv_pool_sct_fct"+str(r+1):torch.nn.GELU()})
      #Add layer for flatting the tensors
      layer_list.update({"flatten_layer":torch.nn.Flatten(start_dim=1, end_dim=-1)})
      
      #Create model
      model=torch.nn.Sequential()
      for id,layer in layer_list.items():
        model.add_module(name=id,module=layer)
      self.model=model
    
    def forward(self,x):
      #Add channel dimension
      y=x.unsqueeze(1)
      #run model
      y=self.model(y)
      return y
    
    def calc_output_shape(self):
      shape=self.model[2*self.n_layers-1].calc_output_size()
      return self.out_channels*shape[0]*shape[1]

class layer_text_to_feature_map(torch.nn.Module):
  def __init__(self,ks_min,ks_max,times,features,n_filter,bias=True,pooling_type="max",parametrizations="None"):
    super().__init__() 
    self.ks_min=ks_min
    self.ks_max=ks_max
    self.n_ks=ks_max-ks_min
    self.features=features
    self.times=times
    self.n_filter=n_filter
    self.filters_per_ks=math.ceil(self.n_filter/(self.ks_max-self.ks_min))
    self.n_filters_max=math.ceil(self.filters_per_ks/2)
    self.n_filters_min=self.filters_per_ks-self.n_filters_max
    self.bias=bias
    self.parametrizations=parametrizations
    self.pooling_type=pooling_type
    
    self.layer_list=torch.nn.ModuleList()
    
    for i in range(ks_min,ks_max):
      self.layer_list.append(
        layer_1dconv_text(
          kernel_size=i,
          times=self.times,
          features=self.features,
          n_filter=self.n_filter,
          bias=self.bias,
          pooling_type=self.pooling_type,
          parametrizations=self.parametrizations,
          device=None, 
          dtype=None
        )
      )
      
  def forward(self, x):
    for i in range(len(self.layer_list)):
      current_layer=self.layer_list[i]
      tmp=current_layer(x)
      tmp=torch.unsqueeze(tmp,dim=1)
      if self.pooling_type=="max":
        tmp=torch.nn.functional.adaptive_max_pool1d(input=tmp,output_size=self.filters_per_ks)
      elif self.pooling_type=="min":
        tmp=(-1)*torch.nn.functional.adaptive_max_pool1d(input=(-1)*tmp,output_size=self.filters_per_ks)
      elif self.pooling_type=="min_max":
        tmp_min=torch.nn.functional.adaptive_max_pool1d(input=tmp,output_size=self.n_filters_min)
        tmp_max=(-1)*torch.nn.functional.adaptive_max_pool1d(input=(-1)*tmp,output_size=self.n_filters_max)
        tmp=torch.cat((tmp_max,tmp_min),dim=2)
      tmp=torch.squeeze(tmp,dim=1)  
      if i==0:
        y=tmp
      else:
        y=torch.cat((y,tmp),dim=1)
    return y
  
  def calc_output_shape(self):
    return self.n_filter

class layer_1dconv_text(torch.nn.Module):
  def __init__(self,kernel_size,times,features,n_filter,bias=True,pooling_type="max",parametrizations="None",device=None, dtype=None):
    super().__init__()
    self.kernel_size_times=kernel_size
    self.features=features
    self.times=times
    self.n_filter=n_filter
    self.bias=bias
    self.parametrizations=parametrizations
    self.pooling_type=pooling_type
    self.device=device
    self.dtype=dtype
    
    self.stride=1
    self.dilation=1
    
    self.conv_layer=torch.nn.Conv2d(
      in_channels=1, 
      out_channels=self.n_filter, 
      kernel_size=(self.kernel_size_times,self.features), 
      stride=self.stride, 
      padding=0, 
      dilation=self.dilation, 
      groups=1, 
      bias=self.bias, 
      padding_mode='zeros', 
      device=self.device, 
      dtype=self.dtype)
    self.pooling_layer=layer_extreme_pooling_2d(
      kernel_size_times=self.times-1,
      kernel_size_features=1,
      n_filter=self.n_filter,
      pooling_type=self.pooling_type)
  
  def forward(self,x):
    y=torch.unsqueeze(x,dim=1)
    y=self.conv_layer(y)
    y=self.pooling_layer(y)
    return y

class layer_extreme_pooling_2d(torch.nn.Module):
  def __init__(self,kernel_size_times,kernel_size_features,n_filter,pooling_type="max"):
    super().__init__()
    self.kernel_size_times=kernel_size_times
    self.kernel_size_features=kernel_size_features
    self.pooling_type=pooling_type
    
    self.n_filter_max=math.ceil(n_filter/2)
    self.n_filter_min=n_filter-self.n_filter_max
    
    self.pool_layer=torch.nn.MaxPool2d(
      kernel_size=(self.kernel_size_times, self.kernel_size_features), 
      stride=None, 
      padding=0, 
      dilation=1, 
      return_indices=False, 
      ceil_mode=False)
    
    if self.pooling_type=="min_max":
      self.max_pooling=torch.nn.AdaptiveMaxPool1d(output_size=self.n_filter_max, return_indices=False)
      self.min_pooling=torch.nn.AdaptiveMaxPool1d(output_size=self.n_filter_min, return_indices=False)

  def forward(self,x):
    if self.pooling_type=="max":
      return torch.squeeze(self.pool_layer(x),dim=(2,3))
    elif self.pooling_type=="min":
      return torch.squeeze((-1)*self.pool_layer((-1)*x),dim=(2,3))
    elif self.pooling_type=="min_max":
      features_min=(-1)*self.min_pooling(torch.unsqueeze(torch.squeeze(self.pool_layer((-1)*x),dim=(2,3)),dim=1))
      features_max=self.max_pooling(torch.unsqueeze(torch.squeeze(self.pool_layer(x),dim=(2,3)),dim=1))
      return torch.squeeze(torch.cat((features_max,features_min),dim=2),dim=1)

class layer_adaptive_extreme_pooling_1d(torch.nn.Module):
  def __init__(self,output_size,pooling_type="max"):
    super().__init__()
    
    self.output_size=output_size
    self.pooling_type=pooling_type
    
    self.n_out_max=math.ceil(self.output_size/2)
    self.n_out_min=self.output_size-self.n_out_max
    
    self.layer_flatten=torch.nn.Flatten()
    if not pooling_type=="min_max":
      self.layer=torch.nn.AdaptiveMaxPool1d(output_size=self.output_size, return_indices=False)
    else:
      self.layer_min=torch.nn.AdaptiveMaxPool1d(output_size=self.n_out_min, return_indices=False)
      self.layer_max=torch.nn.AdaptiveMaxPool1d(output_size=self.n_out_max, return_indices=False)
  def forward(self,x):
    y=self.layer_flatten(x)
    if self.pooling_type=="max":
      return self.layer(y)
    elif self.pooling_type=="min":
      return (-1)*self.layer((-1)*y)
    else:
      tmp_max=self.layer_max(y)
      tmp_min=(-1)*self.layer_min((-1)*y)
      return torch.cat((tmp_max,tmp_min),dim=1)

class dense_layer_with_mask(torch.nn.Module):
  def __init__(self,input_size,output_size,act_fct="elu",dropout=0.0,bias=True,parametrizations="None"):
    super().__init__()
    
    self.input_size=input_size
    self.output_size=output_size
    self.dropout=dropout
    self.bias=bias
    self.parametrizations=parametrizations
    self.act_fct=act_fct
    
    self.dense=torch.nn.Linear(
            in_features=self.input_size,
            out_features=self.output_size,
            bias=self.bias
            )
    if self.parametrizations=="orthogonal":
      torch.nn.utils.parametrizations.orthogonal(module=self.dense, name='weight')
    elif self.parametrizations=="weight_norm":
      torch.nn.utils.parametrizations.weight_norm(module=self.dense, name='weight', dim=0)
    elif self.parametrizations=="spectral_norm":
      torch.nn.utils.spectral_norm(module=self.dense, name='weight', n_power_iterations=1, eps=1e-12, dim=None)
    
    self.act_fct=get_act_fct(self.act_fct)
    if self.dropout >0:
      self.dropout=torch.nn.Dropout(p=self.dropout)
    else:
      self.dropout=None
  def forward(self,x,mask_features):
    y=self.dense(x)
    if self.input_size>=self.output_size:
      mask_features_new=torch.index_select(mask_features,2,torch.arange(start=0, end=self.output_size))
    else:
      tmp_mask=torch.index_select(mask_features,2,torch.arange(start=0, end=1))
      mask_features_new=tmp_mask.repeat(1,1,self.output_size)
    y=(~mask_features_new)*self.act_fct(y)
    if self.dropout==None:
      return y, mask_features_new
    else:
      return self.dropout(y), mask_features_new

class dense_layer_stack(torch.nn.Module):
  def __init__(self,input_size,output_size,n_layers,dropout,act_fct="elu",bias=True,parametrizations="None"):
    super().__init__()
    
    self.input_size=input_size
    self.output_size=output_size
    self.n_layers=n_layers
    self.dropout=dropout
    self.bias=bias
    self.parametrizations=parametrizations
    self.act_fct=act_fct
    
    self.layer_list=torch.nn.ModuleList()
    
    for i in range(self.n_layers):
      if i==0:
        tmp_layer=dense_layer_with_mask(
            input_size=self.input_size,
            output_size=self.output_size,
            act_fct=self.act_fct,
            bias=self.bias,
            dropout=0.0,
            parametrizations=self.parametrizations
            )
      else:
        tmp_layer=dense_layer_with_mask(
            input_size=self.output_size,
            output_size=self.output_size,
            act_fct=self.act_fct,
            bias=self.bias,
            dropout=self.dropout,
            parametrizations=self.parametrizations
            )
      self.layer_list.append(tmp_layer)

  def forward(self,x,mask_features):
    y=x
    mask=mask_features
    for r in range(self.n_layers):
      current_layer=self.layer_list[r]
      res=current_layer(x=y,mask_features=mask)
      y=res[0]
      mask=res[1]
    return y, mask
    
  def calc_output_shape(self):
    return self.output_size

class recurrent_layer_stack(torch.nn.Module): 
  def __init__(self,input_size,times,output_size,n_layers,rec_type,rec_bidirectional,dropout,bias=True,return_sequence=False,parametrizations="None"):
    super().__init__()
    self.input_size=input_size
    self.output_size=output_size
    self.n_layers=n_layers
    self.rec_type=rec_type
    self.rec_bidirectional=rec_bidirectional
    self.dropout=dropout
    self.bias=bias
    self.parametrizations=parametrizations
    self.return_sequence=return_sequence
    self.times=times
    
    self.pack_and_masking=PackAndMasking()

    if rec_type=="gru":
       self.rec_layers=torch.nn.GRU(
            input_size=self.input_size,
            hidden_size=self.output_size,
            num_layers=self.n_layers,
            dropout=self.dropout, 
            bidirectional=self.rec_bidirectional,
            bias=self.bias,
            batch_first=True)
    elif rec_type=="lstm":
       self.rec_layers=torch.nn.LSTM(
            input_size=self.input_size,
            hidden_size=self.output_size,
            num_layers=self.n_layers,
            dropout=self.dropout, 
            bidirectional=self.rec_bidirectional,
            bias=self.bias,
            batch_first=True)
   
    self.unpack=UnPackAndMasking(sequence_length=self.times)
    
  def forward(self,x,seq_len):
    y=self.pack_and_masking(x,seq_len=seq_len)
    y=self.rec_layers(y)
    if self.return_sequence==True:
      return self.unpack(y)
    else:
      return y[1][-1,:,:]

  def calc_output_shape(self):
    if self.rec_bidirectional==True:
      return self.output_size*2
    else:
      return self.output_size

class tf_encoder_layer_stack(torch.nn.Module):
  def __init__(self,input_size,dense_dim,n_layers,times,dropout,act_fct="elu",attention_type="multihead",positional_embedding="absolute",num_heads=1,bias=True,parametrizations="None"):
    super().__init__()
    self.input_size=input_size
    self.dense_dim=dense_dim
    self.n_layers=n_layers
    self.attention_type=attention_type
    self.num_heads=num_heads
    self.times=times
    self.dropout=dropout
    self.bias=bias
    self.parametrizations=parametrizations
    self.positional_embedding=positional_embedding
    self.act_fct=act_fct
    
    self.layer_list=torch.nn.ModuleList()
    
    if self.positional_embedding=="absolute":
       self.positional_embedding=AddPositionalEmbedding_PT(sequence_length=self.times,embedding_dim=self.input_size)
    
    for r in range(self.n_layers):
      if self.attention_type=="multihead":
        self.layer_list.append(
          TransformerEncoder(
            embed_dim = self.input_size,
            dense_dim= self.dense_dim,
            num_heads =self.num_heads,
            dropout_rate=self.dropout,
            act_fct=self.act_fct,
            bias=self.bias,
            times=self.times,
            parametrizations=self.parametrizations))
      else:
        self.layer_list.append(
          FourierEncoder(dense_dim=self.dense_dim,
                            features=self.input_size,
                            dropout_rate=self.dropout,
                            act_fct=self.act_fct,
                            bias=self.bias,
                            parametrizations=self.parametrizations))

  def forward(self,x,seq_len,mask_times,mask_features):
    y=self.positional_embedding(x)
    for r in range(self.n_layers):
      current_layer=self.layer_list[r]
      y=current_layer(y,seq_len,mask_times,mask_features)
    return y
  
  def calc_output_shape(self):
    return self.input_size
  
class merge_layer(torch.nn.Module):
  def __init__(self,n_extracted_features,pooling_type="max",raw_shape=None,conv_shape=None,dense_shape=None,rec_shape=None,tf_shape=None,tf_attention_type=None,num_heads=1):
    super().__init__()
    
    self.conv_shape=conv_shape
    self.raw_shape=raw_shape
    self.times=self.raw_shape[0]
    self.features=self.raw_shape[1]
    self.dense_shape=dense_shape
    self.rec_shape=rec_shape
    self.tf_shape=tf_shape
    self.tf_attention_type=tf_attention_type
    self.pooling_type=pooling_type
    
    self.target_sequence_length=self.calc_sequence_length()
    self.n_extracted_features=n_extracted_features
    
    self.combine_transformation=torch.nn.Linear(in_features=self.n_extracted_features*self.target_sequence_length,out_features=self.target_sequence_length,bias=False)
    self.combine_flatten=torch.nn.Flatten()
    self.combine_activation=torch.nn.Softmax(dim=1)

    self.num_heads=num_heads

    self.pool_layer=layer_adaptive_extreme_pooling_1d(output_size=self.n_extracted_features,pooling_type=self.pooling_type)
    
    if not conv_shape==None:
      self.conv_normalizer=torch.nn.BatchNorm1d(num_features=self.n_extracted_features)
    
    if not self.rec_shape==None:
      self.rec_dense=torch.nn.Linear(in_features=self.rec_shape,out_features=self.n_extracted_features,bias=False)
      self.rec_normalizer=torch.nn.BatchNorm1d(num_features=self.n_extracted_features)
    
    if not self.dense_shape==None:
      self.dense_dense=dense_layer_with_mask(input_size=self.dense_shape,output_size=self.n_extracted_features,bias=False,parametrizations="None")
      self.dense_normalizer=LayerNorm_with_Mask(times=self.times, features=self.n_extracted_features)
 
    if not self.tf_shape==None:
      if not self.tf_attention_type=="fourier":
        self.tf_dense=dense_layer_with_mask(input_size=self.tf_shape,output_size=self.n_extracted_features,bias=False,parametrizations="None")
        self.tf_normalizer=LayerNorm_with_Mask(times=self.times, features=self.n_extracted_features)
      else:
        self.tf_dense=torch.nn.Linear(in_features=self.tf_shape,out_features=self.n_extracted_features,bias=False)
        self.tf_normalizer=torch.nn.LayerNorm(normalized_shape=self.n_extracted_features)
    
    if not self.raw_shape==None:
      self.raw_dense=dense_layer_with_mask(input_size=self.raw_shape[1],output_size=self.n_extracted_features,bias=False,parametrizations="None")
      self.raw_normalizer=self.tf_normalizer=LayerNorm_with_Mask(times=self.times, features=self.n_extracted_features)

    self.attention_layer=torch.nn.MultiheadAttention(
      embed_dim=self.n_extracted_features, 
      num_heads=self.num_heads, 
      dropout=0.0, 
      bias=True, 
      add_bias_kv=False, 
      add_zero_attn=False, 
      kdim=None, 
      vdim=None, 
      batch_first=True, 
      device=None, 
      dtype=None)
    
    self.final_flatten=torch.nn.Flatten(start_dim=1, end_dim=-1)
      
  def calc_output_shape(self):
    return self.n_extracted_features
  
  def calc_sequence_length(self):
    seq_len=0
    if not self.conv_shape==None:
      seq_len=seq_len+1
    if not self.raw_shape==None:
      seq_len=seq_len+1
    if not self.dense_shape==None:
      seq_len=seq_len+1
    if not self.rec_shape==None:
      seq_len=seq_len+1
    if not self.tf_shape==None:
      seq_len=seq_len+1
    return seq_len
      
  def conotate_tensors(self,input_raw=None,input_conv=None,input_dense=None,input_rec=None,input_tf=None):
    x=torch.unsqueeze(input_raw,dim=1)
    
    if not input_conv==None:
      x=torch.cat((x,torch.unsqueeze(input_conv,dim=1)),dim=1)
    
    if not input_dense==None:
      x=torch.cat((x,torch.unsqueeze(input_dense,dim=1)),dim=1)
    
    if not input_rec==None:
      x=torch.cat((x,torch.unsqueeze(input_rec,dim=1)),dim=1)
      
    if not input_tf==None:
      x=torch.cat((x,torch.unsqueeze(input_tf,dim=1)),dim=1)
    return x
    
  def forward(self,input_raw=None,input_conv=None,input_dense=None,input_rec=None,input_tf=None,seq_len=None,mask_times=None,mask_features=None):
    #Conv
    if not self.conv_shape==None:
      output_conv=self.conv_normalizer(input_conv)
    else:
      output_conv=None
    
    #Raw  
    if not self.raw_shape==None:
      output_raw=self.raw_dense(input_raw,mask_features)
      output_raw=self.raw_normalizer(output_raw[0],seq_len,output_raw[1])
      output_raw=self.pool_layer(output_raw)
    else:
      output_raw=None
      
    #Dense  
    if not self.dense_shape==None:
      output_dense=self.dense_dense(x=input_dense[0],mask_features=input_dense[1])
      output_dense=self.dense_normalizer(output_dense[0],seq_len,mask_features=output_dense[1])
      output_dense.size()
      output_dense=self.pool_layer(output_dense)
    else:
      output_dense=None
      
    #Rec
    if not self.rec_shape==None:
      output_rec=self.rec_dense(input_rec)
      output_rec=self.rec_normalizer(output_rec)
    else:
      output_rec=None

    #Tf
    if not self.tf_shape==None:
      if not self.tf_attention_type=="fourier":
        output_tf=self.tf_dense(x=input_tf,mask_features=mask_features)
        output_tf=self.tf_normalizer(output_tf[0],seq_len,output_tf[1])
        output_tf=self.pool_layer(output_tf)
      else:
        output_tf=self.tf_dense(input_tf)
        output_tf=self.tf_normalizer(output_tf)
        output_tf=self.pool_layer(output_tf)
    else:
      output_tf=None
      
    #Merge
    merged=self.conotate_tensors(
      input_raw=output_raw,
      input_conv=output_conv,
      input_dense=output_dense,
      input_rec=output_rec,
      input_tf=output_tf
    )
    
    #Attention
    attn=self.attention_layer(merged,merged,merged)[0]
    attn=self.combine_flatten(attn)
 
    #Combine
    combine_weights=self.combine_transformation(attn)
    combine_weights=self.combine_activation(combine_weights)
   
   #Select final features
    final=merged*torch.unsqueeze(combine_weights,dim=2).expand((merged.size()[0],self.target_sequence_length,self.n_extracted_features))
    final=torch.sum(final,dim=1)
    return final

class TEClassifierParallel(torch.nn.Module):
  def __init__(self, times, features, target_features,act_fct="elu", pooling_type="max",
              conv_ks_min=2, conv_ks_max=4, conv_bias=False, conv_parametrizations="None", 
              dense_output_size=25,dense_n_layers=0,dense_dropout=0.0,dense_bias=False,dense_parametrizations="None", 
              rec_output_size=25,rec_n_layers=0,rec_type="gru",rec_bidirectional=False,rec_dropout=0.0,rec_bias=False,rec_parametrizations="None", 
              tf_dense_dim=50,tf_n_layers=0,tf_dropout=0.0,tf_attention_type="multihead",tf_positional_embedding="absolute",tf_num_heads=1,tf_bias=False,tf_parametrizations="None"):
    super().__init__()
    self.times=times
    self.features=features
    self.target_features=target_features
    self.pooling_type=pooling_type
    self.act_fct=act_fct
    
    self.masking_layer=masking_layer()
    
    self.conv_ks_min=conv_ks_min
    self.conv_ks_max=conv_ks_max
    self.conv_bias=conv_bias
    self.conv_parametrizations=conv_parametrizations
    if self.conv_ks_min > 0:
      self.conv_layers=layer_text_to_feature_map(
        ks_min=self.conv_ks_min,
        ks_max=self.conv_ks_max,
        times=self.times,
        features=self.features,
        n_filter=self.target_features,
        bias=self.conv_bias,
        pooling_type=self.pooling_type,
        parametrizations=self.conv_parametrizations
      )
      self.conv_shape=self.conv_layers.calc_output_shape()
    else:
      self.conv_layers=None
      self.conv_shape=None
    
    self.dense_output_size=dense_output_size
    self.dense_n_layers=dense_n_layers
    self.dense_dropout=dense_dropout
    self.dense_bias=dense_bias
    self.dense_parametrizations=dense_parametrizations
    if self.dense_n_layers > 0:
      self.dense_layers=dense_layer_stack(
        input_size=self.features,
        output_size=self.dense_output_size,
        act_fct=self.act_fct,
        n_layers=self.dense_n_layers,
        dropout=self.dense_dropout,
        bias=self.dense_bias,
        parametrizations=self.dense_parametrizations
      )
      self.dense_shape=self.dense_layers.calc_output_shape()
    else:
      self.dense_layers=None
      self.dense_shape=None
    
    self.rec_output_size=rec_output_size
    self.rec_n_layers=rec_n_layers
    self.rec_type=rec_type
    self.rec_bidirectional=rec_bidirectional
    self.rec_dropout=rec_dropout
    self.rec_bias=rec_bias
    self.rec_parametrizations=rec_parametrizations
    if self.rec_n_layers > 0:
      self.rec_layers=recurrent_layer_stack(
        input_size=self.features,
        times=self.times,
        output_size=self.rec_output_size,
        n_layers=self.rec_n_layers,
        rec_type=self.rec_type,
        rec_bidirectional=self.rec_bidirectional,
        dropout=self.rec_dropout,
        bias=self.rec_bias,
        return_sequence=False,
        parametrizations=self.rec_parametrizations
        )
      self.rec_shape=self.rec_layers.calc_output_shape()
    else:
      self.rec_layers=None
      self.rec_shape=None
    
    self.tf_dense_dim=tf_dense_dim
    self.tf_n_layers=tf_n_layers
    self.tf_dropout=tf_dropout
    self.tf_attention_type=tf_attention_type
    self.tf_num_heads=tf_num_heads
    self.tf_bias=tf_bias
    self.tf_parametrizations=tf_parametrizations
    self.tf_positional_embedding=tf_positional_embedding
    if tf_n_layers > 0:
      self.tf_layers=tf_encoder_layer_stack(
        input_size=self.features,
        dense_dim=self.tf_dense_dim,
        n_layers=self.tf_n_layers,
        times=self.times,
        dropout=self.tf_dropout,
        attention_type=self.tf_attention_type,
        positional_embedding=self.tf_positional_embedding,
        num_heads=self.tf_num_heads,
        bias=self.tf_bias,
        parametrizations=self.tf_parametrizations
        )
      self.tf_shape=self.tf_layers.calc_output_shape()
    else:
      self.tf_layers=None
      self.tf_shape=None
    
    self.merge_layer=merge_layer(
      n_extracted_features=self.target_features,
      raw_shape=(self.times,self.features),
      conv_shape=self.conv_shape,
      dense_shape=self.dense_shape,
      rec_shape=self.rec_shape,
      tf_shape=self.tf_shape,
      num_heads=1
    )

    self.merge_shape=self.merge_layer.calc_output_shape()
    
    self.classification_head=torch.nn.Linear(
            in_features=self.merge_shape,
            out_features=3,
            bias=True
            )
            
  def forward (self,x,predication_mode=True):
    mask=self.masking_layer(x)
    
    if not self.conv_shape==None:
      conv_output=self.conv_layers(x)
    else:
      conv_output=None
    if not self.dense_shape==None:
      dense_output=self.dense_layers(x,mask[2])
    else:
      dense_output=None
    if not self.rec_shape==None:
      rec_output=self.rec_layers(x,seq_len=mask[0])
    else:
      rec_output=None
    if not self.tf_shape==None:
      tf_output=self.tf_layers(x,seq_len=mask[0],mask_times=mask[1],mask_features=mask[2])
    else:
      tf_output=None
      
    merge_output=self.merge_layer(
      input_raw=x,
      input_conv=conv_output,
      input_dense=dense_output,
      input_rec=rec_output,
      input_tf=tf_output,
      seq_len=mask[0],
      mask_times=mask[1],
      mask_features=mask[2]
    )
    
    classification=self.classification_head(merge_output)

    if predication_mode==False:
      return classification
    else:
      return torch.nn.Softmax(dim=1)(classification)

#abc=multistream_classifier(
#  target_features=60, 
#  pooling_type="min_max",
#  times=10,
#  features=768,
#  act_fct="elu",
#  conv_ks_min=2, conv_ks_max=3,
#  dense_n_layers=0,
#  rec_n_layers=0,
#  tf_n_layers=0)
#test_tensor=torch.rand(7,10,768)
#abc(test_tensor)

