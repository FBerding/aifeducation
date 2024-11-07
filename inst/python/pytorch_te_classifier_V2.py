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
from torcheval.metrics.functional import multiclass_confusion_matrix
import numpy as np
import safetensors

class LayerNorm_with_Mask_PT(torch.nn.Module):
    def __init__(self, features,eps=1e-5):
      super().__init__()
      self.eps=eps
      self.features = features
      self.gamma = torch.nn.Parameter(torch.ones(1, 1, self.features))
      self.beta = torch.nn.Parameter(torch.zeros(1, 1, self.features))

    def forward(self, x):
      mask=self.get_mask(x)

      n_elements=torch.sum(~mask,dim=1)*self.features

      mean=torch.sum(torch.sum(x,dim=1),dim=1)/n_elements
      mean=torch.reshape(mean,(x.size(dim=0),1))
      
      mean_long=torch.reshape(mean.repeat(1,x.size(dim=1)*self.features),(x.size(dim=0),x.size(dim=1),self.features))
      mask_long=torch.reshape(torch.repeat_interleave(mask,repeats=self.features,dim=1),(x.size(dim=0),x.size(dim=1),self.features))
     
      var=torch.square((x-mean_long)*(~mask_long))

      normalized=((~mask_long)*(x-mean_long)/torch.sqrt(var+self.eps))*self.gamma+self.beta
      return normalized
    
    def get_mask(self,x):
      device=('cuda' if torch.cuda.is_available() else 'cpu')
      time_sums=torch.sum(x,dim=2)
      mask=(time_sums==0)
      mask=mask.to(device)
      return mask

class PackAndMasking_PT(torch.nn.Module):
  def __init__(self):
    super().__init__()
  
  def forward(self,x):
    return torch.nn.utils.rnn.pack_padded_sequence(
    input=x,
    lengths=self.get_length(x).to("cpu",dtype=torch.int64),
    enforce_sorted=False, 
    batch_first=True)
  
  def get_length(self,x):
    time_sum=torch.sum(x,dim=2)
    time_sum=(time_sum!=0)
    return torch.sum(time_sum,dim=1)
  
class UnPackAndMasking_PT(torch.nn.Module):
  def __init__(self,sequence_length):
    super().__init__()
    self.sequence_length=sequence_length
    
  def forward(self,x):
    return torch.nn.utils.rnn.pad_packed_sequence(
    x,
    total_length=self.sequence_length,
    batch_first=True)[0]
  
class BiDirectionalGRU_PT(torch.nn.Module):
  def __init__(self,input_size,hidden_size):
    super().__init__()
    self.input_size=input_size
    self.hidden_size=hidden_size
    
    self.gru_layer=torch.nn.GRU(
    input_size= self.input_size,
    hidden_size=self.hidden_size,
    bias=True,
    batch_first=True,
    dropout=0.0,
    bidirectional=True)
    
  def forward(self,x):
    result=self.gru_layer(x)
    return result[0]


class GRU_PT(torch.nn.Module):
  def __init__(self,input_size,hidden_size,num_layers,dropout,bidirectional):
    super().__init__()
    self.input_size=input_size
    self.hidden_size=hidden_size
    self.num_layers=num_layers
    self.dropout=dropout
    self.bidirectional=bidirectional
    
    self.gru_layer=torch.nn.GRU(
    input_size= self.input_size,
    hidden_size=self.hidden_size,
    num_layers=self.num_layers,
    bias=True,
    batch_first=True,
    dropout=self.dropout,
    bidirectional=self.bidirectional)
    
  def forward(self,x):
    result=self.gru_layer(x)
    return result[0]
  
class LSTM_PT(torch.nn.Module):
  def __init__(self,input_size,hidden_size,num_layers,dropout,bidirectional):
    super().__init__()
    self.input_size=input_size
    self.hidden_size=hidden_size
    self.num_layers=num_layers
    self.dropout=dropout
    self.bidirectional=bidirectional
    
    self.lstm_layer=torch.nn.LSTM(
    input_size= self.input_size,
    hidden_size=self.hidden_size,
    num_layers=self.num_layers,
    bias=True,
    batch_first=True,
    dropout=self.dropout,
    bidirectional=self.bidirectional)
    
  def forward(self,x):
    result=self.lstm_layer(x)
    return result[0]



  
class UniDirectionalGRU_PT(torch.nn.Module):
  def __init__(self,input_size,hidden_size):
    super().__init__()
    self.input_size=input_size
    self.hidden_size=hidden_size
    
    self.gru_layer=torch.nn.GRU(
    input_size= self.input_size,
    hidden_size=self.hidden_size,
    bias=True,
    batch_first=True,
    dropout=0.0,
    bidirectional=False)
    
  def forward(self,x):
    result=self.gru_layer(x)
    return result[0]
  
class BiDirectionalLSTM_PT(torch.nn.Module):
  def __init__(self,input_size,hidden_size):
    super().__init__()
    self.input_size=input_size
    self.hidden_size=hidden_size
    
    self.lstm_layer=torch.nn.LSTM(
    input_size= self.input_size,
    hidden_size=self.hidden_size,
    bias=True,
    batch_first=True,
    dropout=0.0,
    bidirectional=True)
    
  def forward(self,x):
    result=self.lstm_layer(x)
    return result[0]

class UniDirectionalLSTM_PT(torch.nn.Module):
  def __init__(self,input_size,hidden_size):
    super().__init__()
    self.input_size=input_size
    self.hidden_size=hidden_size
    
    self.lstm_layer=torch.nn.LSTM(
    input_size= self.input_size,
    hidden_size=self.hidden_size,
    bias=True,
    batch_first=True,
    dropout=0.0,
    bidirectional=False)
    
  def forward(self,x):
    result=self.lstm_layer(x)
    return result[0]

class FourierTransformation_PT(torch.nn.Module):
  def __init__(self):
    super().__init__()
    
  def forward(self,x):
    return torch.real(torch.fft.fft(torch.fft.fft(input=x,dim=2),dim=1))
  
class FourierEncoder_PT(torch.nn.Module):
  def __init__(self, dense_dim, features, dropout_rate):
    super().__init__()
    
    self.dense_dim=dense_dim
    self.dropout_rate=dropout_rate
    self.features=features
   
    self.attention=FourierTransformation_PT()
    self.dropout=torch.nn.Dropout(p=dropout_rate)
    self.layernorm_1=LayerNorm_with_Mask_PT(features=self.features)
    self.dense_proj=torch.nn.Sequential(
      torch.nn.Linear(in_features=self.features,out_features=self.dense_dim),
      torch.nn.GELU(),
      torch.nn.Linear(in_features=self.dense_dim,out_features=self.features)
    )
    self.layernorm_2=LayerNorm_with_Mask_PT(features=self.features)
    
  def forward(self,x):
    attention_output=self.attention(x)
    attention_output=self.dropout(attention_output)
    proj_input=self.layernorm_1(attention_output)
    proj_output=self.dense_proj(proj_input)
    return self.layernorm_2(proj_input+proj_output)
  
  
class TransformerEncoder_PT(torch.nn.Module):
  def __init__(self, embed_dim, dense_dim, num_heads, dropout_rate):
    super().__init__()
    self.embed_dim=embed_dim
    self.dense_dim=dense_dim
    self.num_heads=num_heads
    self.dropout_rate=dropout_rate

    self.attention=torch.nn.MultiheadAttention(
      embed_dim=self.embed_dim,
      num_heads=self.num_heads,
      dropout=0,
      batch_first=True)
    self.dropout=torch.nn.Dropout(p=dropout_rate)
    self.layernorm_1=LayerNorm_with_Mask_PT(features=self.embed_dim)
    self.dense_proj=torch.nn.Sequential(
      torch.nn.Linear(in_features=self.embed_dim,out_features=self.dense_dim),
      torch.nn.GELU(),
      torch.nn.Linear(in_features=self.dense_dim,out_features=self.embed_dim))
    self.layernorm_2=LayerNorm_with_Mask_PT(features=self.embed_dim)

  def forward(self,x):
    attention_output=self.attention(
      query=x,
      key=x,
      value=x,
      key_padding_mask=self.get_mask(x))[0]
    attention_output=self.dropout(attention_output)
    proj_input=self.layernorm_1(attention_output)
    proj_output=self.dense_proj(proj_input)
    return self.layernorm_2(proj_input+proj_output)
  
  def get_mask(self,x):
    time_sum=torch.sum(x,dim=2)
    time_sum=(time_sum!=0)
    return ~time_sum

class AddPositionalEmbedding_PT(torch.nn.Module):
  def __init__(self, sequence_length,embedding_dim):
    super().__init__()
    self.sequence_length=sequence_length
    self.embedding_dim=embedding_dim
    
    self.embedding=torch.nn.Embedding(
      num_embeddings=self.sequence_length+1,
      embedding_dim=self.embedding_dim,
      padding_idx=0
    )
    
  def forward(self, x):
    mask=self.get_mask(x)
    device=('cuda' if torch.cuda.is_available() else 'cpu')

    input_seq=torch.arange(start=1, end=(self.sequence_length+1), step=1)
    input_seq=input_seq.to(device)
    
    input_seq=input_seq.repeat(x.shape[0], 1)
    input_seq.masked_fill_(mask,value=0)
    embedded_positions_masked=self.embedding(input_seq)
   
    return x+embedded_positions_masked
  
  def get_mask(self,x):
    device=('cuda' if torch.cuda.is_available() else 'cpu')
    time_sum=torch.sum(x,dim=2)
    time_sum=(time_sum!=0)
    masks=~time_sum
    masks=masks.to(device)
    return masks
 
class GlobalAveragePooling1D_PT(torch.nn.Module):
  def __init__(self):
    super().__init__()

  def forward(self,x,mask=None):
    if not mask is None:
      mask_r=mask.reshape(mask.size()[0],mask.size()[1],1)
      x=torch.mul(x,mask_r)
    x=torch.sum(x,dim=1)*(1/self.get_length(x))
    return x
  
  def get_length(self,x):
    length=torch.sum(x,dim=2)
    length=(length!=0)
    length=torch.sum(length,dim=1).repeat(x.size(2),1)
    length=torch.transpose(length,dim0=0,dim1=1)
    return length
  

class TextEmbeddingClassifier_PT(torch.nn.Module):
  def __init__(self,features, times, dense_size,dense_layers,rec_size,rec_layers, rec_type,rec_bidirectional, intermediate_size,
  attention_type, repeat_encoder, dense_dropout,rec_dropout, encoder_dropout,
  add_pos_embedding, self_attention_heads, target_levels,classification_head=True):
    
    super().__init__()
    
    self.n_target_levels=len(target_levels)
    layer_list=torch.nn.ModuleDict()
    
    current_size=features
    if rec_layers>0 or repeat_encoder>0:
      if add_pos_embedding==True:
        layer_list.update({"add_positional_embedding":AddPositionalEmbedding_PT(
          sequence_length=times,
          embedding_dim=features)})
      layer_list.update({"normalizaion_layer":LayerNorm_with_Mask_PT(features=features)})

    
    if repeat_encoder>0:
        for r in range(repeat_encoder):
          if attention_type=="multihead":
            layer_list.update({"encoder_"+str(r+1):
              TransformerEncoder_PT(
                embed_dim = features,
                dense_dim= intermediate_size,
                num_heads =self_attention_heads,
                dropout_rate=encoder_dropout)})
          else:
            layer_list.update({"encoder_"+str(r+1):
              FourierEncoder_PT(dense_dim=intermediate_size,
                                features=features,
                                dropout_rate=encoder_dropout)})
                                
    if rec_layers>0:
      layer_list.update({"packandmasking":PackAndMasking_PT()})
      if rec_type=="gru":
        layer_list.update({"gru_bidirectional"+str(rec_bidirectional):
            GRU_PT(
              input_size=current_size,
              hidden_size=rec_size,
              num_layers=rec_layers,
              dropout=rec_dropout, 
              bidirectional=rec_bidirectional)})
      elif rec_type=="lstm":
        layer_list.update({"lstm_bidirectional"+str(rec_bidirectional):
            LSTM_PT(
              input_size=current_size,
              hidden_size=rec_size,
              num_layers=rec_layers,
              dropout=rec_dropout, 
              bidirectional=rec_bidirectional)})
      layer_list.update({"unpackandmasking":UnPackAndMasking_PT(sequence_length=times)})

    layer_list.update({"global_average_pooling":GlobalAveragePooling1D_PT()})
      
    if(rec_layers>0):
      if rec_bidirectional==True:
        current_size_2=2*rec_size
      else:
        current_size_2=rec_size
    else:
      current_size_2=current_size
    
    #Adding standard layer
    if dense_layers>0:
      for i in range(dense_layers):
        if i==0:
          layer_list.update({"dense_"+str(i+1):
            torch.nn.Linear(
              in_features=current_size_2,
              out_features=dense_size
            )})
          layer_list.update({"dense_act_fct_"+str(i+1):
            torch.nn.GELU()})
        else:
          layer_list.update({"dense_"+str(i+1):
            torch.nn.Linear(
              in_features=dense_size,
              out_features=dense_size)})
          layer_list.update({"dense_act_fct_"+str(i+1):
            torch.nn.GELU()})
        if i!=(dense_layers-1):
          layer_list.update({"dense_dropout_"+str(i+1):torch.nn.Dropout(p=dense_dropout)})
    
    if dense_layers==0 and rec_layers==0 and repeat_encoder==0 and times==1:
      last_in_features=features*times
    elif  dense_layers>0:
      last_in_features=dense_size
    elif rec_layers>0:
      if rec_bidirectional==True:
        last_in_features=2*rec_size
      else:
        last_in_features=rec_size
    else:
      last_in_features=features

    #Adding final Layer
    if classification_head==True:
      layer_list.update({"output_categories":
        torch.nn.Linear(
            in_features=last_in_features,
            out_features=self.n_target_levels
        )})


    #Summarize Model
    model=torch.nn.Sequential()
    for id,layer in layer_list.items():
      model.add_module(name=id,module=layer)
    self.model=model
  
  def forward(self, x,predication_mode=True):
    if predication_mode==False:
      return self.model(x)
    else:
      return torch.nn.Softmax(dim=1)(self.model(x))



def TeClassifierTrain_PT_with_Datasets(model,loss_fct_name, optimizer_method, epochs, trace,batch_size,
train_data,val_data,filepath,use_callback,n_classes,class_weights,test_data=None,
log_dir=None, log_write_interval=10, log_top_value=0, log_top_total=1, log_top_message="NA"):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    current_dtype=float
    model.to(device,dtype=current_dtype)
  else:
    current_dtype=torch.double
    model.to(device,dtype=current_dtype)
  
  if optimizer_method=="adam":
    optimizer=torch.optim.Adam(params=model.parameters(),weight_decay=1e-3)
  elif optimizer_method=="rmsprop":
    optimizer=torch.optim.RMSprop(model.parameters())
    
  class_weights=class_weights.clone()
  class_weights=class_weights.to(device)
  
  if loss_fct_name=="CrossEntropyLoss":
    loss_fct=torch.nn.CrossEntropyLoss(
        reduction="none",
        weight = class_weights)
  
  trainloader=torch.utils.data.DataLoader(
    train_data,
    batch_size=batch_size,
    shuffle=True)
    
  valloader=torch.utils.data.DataLoader(
    val_data,
    batch_size=batch_size,
    shuffle=False)
    
  if not (test_data is None):
    testloader=torch.utils.data.DataLoader(
      test_data,
      batch_size=batch_size,
      shuffle=False)
      
  #Tensor for Saving Training History
  if not (test_data is None):
    history_loss=torch.ones(size=(3,epochs),requires_grad=False)*-100
    history_acc=torch.ones(size=(3,epochs),requires_grad=False)*-100
    history_bacc=torch.ones(size=(3,epochs),requires_grad=False)*-100
    history_avg_iota=torch.ones(size=(3,epochs),requires_grad=False)*-100
  else:
    history_loss=torch.ones(size=(2,epochs),requires_grad=False)*-100
    history_acc=torch.ones(size=(2,epochs),requires_grad=False)*-100
    history_bacc=torch.ones(size=(2,epochs),requires_grad=False)*-100
    history_avg_iota=torch.ones(size=(2,epochs),requires_grad=False)*-100
  
  best_bacc=float('-inf')
  best_acc=float('-inf')
  best_val_loss=float('inf')
  best_val_avg_iota=float('-inf')
  
  #Log file
  if not (log_dir is None):
    log_file=log_dir+"/aifeducation_state.log"
    log_file_loss=log_dir+"/aifeducation_loss.log"
    last_log=None
    last_log_loss=None
    current_step=0
    total_epochs=epochs
    total_steps=len(trainloader)+len(valloader)
    if not (test_data is None):
      total_steps=total_steps+len(testloader)
    

  for epoch in range(epochs):
    #logging
    current_step=0
    
    #Training------------------------------------------------------------------
    train_loss=0.0
    n_matches_train=0
    n_total_train=0
    confusion_matrix_train=torch.zeros(size=(n_classes,n_classes))
    confusion_matrix_train=confusion_matrix_train.to(device,dtype=torch.double)
    
    model.train(True)
    for batch in trainloader:
      inputs=batch["input"]
      labels=batch["labels"]

      sample_weights=batch["sample_weights"]
      sample_weights=torch.reshape(input=sample_weights,shape=(sample_weights.size(dim=0),1))

      inputs = inputs.to(device,dtype=current_dtype)
      labels=labels.to(device,dtype=current_dtype)
      sample_weights=sample_weights.to(device,dtype=current_dtype)
      
      optimizer.zero_grad()
      
      outputs=model(inputs,predication_mode=False)
      loss=loss_fct(outputs,labels)*sample_weights
      loss=loss.mean()
      loss.backward()
      optimizer.step()
      train_loss +=loss.item()
      
      #Calc Accuracy
      pred_idx=torch.nn.Softmax(dim=1)(outputs).max(dim=1).indices
      label_idx=labels.max(dim=1).indices
          
      match=(pred_idx==label_idx)
      n_matches_train+=match.sum().item()
      n_total_train+=outputs.size(0)
      
      #Calc Balanced Accuracy
      confusion_matrix_train+=multiclass_confusion_matrix(input=outputs,target=label_idx,num_classes=n_classes)
      
      #Update log file
      if not (log_dir is None):
        current_step+=1
        last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                  total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                  message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
        last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)
     
    #Calc final metrics for epoch  
    acc_train=n_matches_train/n_total_train
    bacc_train=torch.sum(torch.diagonal(confusion_matrix_train)/torch.sum(confusion_matrix_train,dim=1))/n_classes
    avg_iota_train=torch.diagonal(confusion_matrix_train)/(torch.sum(confusion_matrix_train,dim=0)+torch.sum(confusion_matrix_train,dim=1)-torch.diagonal(confusion_matrix_train))
    avg_iota_train=torch.sum(avg_iota_train)/n_classes

    #Validation----------------------------------------------------------------
    val_loss=0.0
    n_matches_val=0
    n_total_val=0
    
    confusion_matrix_val=torch.zeros(size=(n_classes,n_classes))
    confusion_matrix_val=confusion_matrix_val.to(device,dtype=torch.double)
    
    model.eval()
    with torch.no_grad():
      for batch in valloader:
        inputs=batch["input"]
        labels=batch["labels"]

        inputs = inputs.to(device,dtype=current_dtype)
        labels=labels.to(device,dtype=current_dtype)
      
        outputs=model(inputs,predication_mode=False)
        
        loss=loss_fct(outputs,labels).mean()
        val_loss +=loss.item()
      
        pred_idx=torch.nn.Softmax(dim=1)(outputs).max(dim=1).indices
        label_idx=labels.max(dim=1).indices
          
        match=(pred_idx==label_idx)
        n_matches_val+=match.sum().item()
        n_total_val+=outputs.size(0)
        
        #Calc Balanced Accuracy
        confusion_matrix_val+=multiclass_confusion_matrix(input=outputs,target=label_idx,num_classes=n_classes)
        
        #Update log file
        if not (log_dir is None):
          current_step+=1
          last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                    total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                    message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
          last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)

    acc_val=n_matches_val/n_total_val
    bacc_val=torch.sum(torch.diagonal(confusion_matrix_val)/torch.sum(confusion_matrix_val,dim=1))/n_classes
    avg_iota_val=torch.diagonal(confusion_matrix_val)/(torch.sum(confusion_matrix_val,dim=0)+torch.sum(confusion_matrix_val,dim=1)-torch.diagonal(confusion_matrix_val))
    avg_iota_val=torch.sum(avg_iota_val)/n_classes
    
    #Test----------------------------------------------------------------------
    if not (test_data is None):
      test_loss=0.0
      n_matches_test=0
      n_total_test=0
      
      confusion_matrix_test=torch.zeros(size=(n_classes,n_classes))
      confusion_matrix_test=confusion_matrix_test.to(device,dtype=torch.double)
  
      model.eval()
      with torch.no_grad():
        for batch in testloader:
          inputs=batch["input"]
          labels=batch["labels"]

          inputs = inputs.to(device,dtype=current_dtype)
          labels=labels.to(device,dtype=current_dtype)
        
          outputs=model(inputs,predication_mode=False)
          
          loss=loss_fct(outputs,labels).mean()
          test_loss +=loss.item()
        
          pred_idx=torch.nn.Softmax(dim=1)(outputs).max(dim=1).indices
          label_idx=labels.max(dim=1).indices
            
          match=(pred_idx==label_idx)
          n_matches_test+=match.sum().item()
          n_total_test+=outputs.size(0)
          
          #Calc Balanced Accuracy
          confusion_matrix_test+=multiclass_confusion_matrix(input=outputs,target=label_idx,num_classes=n_classes)
          
          #Update log file
          if not (log_dir is None):
            current_step+=1
            last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                      total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                      message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
            last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)
     
      acc_test=n_matches_test/n_total_test
      bacc_test=torch.sum(torch.diagonal(confusion_matrix_test)/torch.sum(confusion_matrix_test,dim=1))/n_classes
      avg_iota_test=torch.diagonal(confusion_matrix_test)/(torch.sum(confusion_matrix_test,dim=0)+torch.sum(confusion_matrix_test,dim=1)-torch.diagonal(confusion_matrix_test))
      avg_iota_test=torch.sum(avg_iota_test)/n_classes
    
    
    #Record History
    if not (test_data is None):
      history_loss[0,epoch]=train_loss/len(trainloader)
      history_loss[1,epoch]=val_loss/len(valloader)
      history_loss[2,epoch]=test_loss/len(testloader)
      
      history_acc[0,epoch]=acc_train
      history_acc[1,epoch]=acc_val
      history_acc[2,epoch]=acc_test
      
      history_bacc[0,epoch]=bacc_train
      history_bacc[1,epoch]=bacc_val
      history_bacc[2,epoch]=bacc_test
      
      history_avg_iota[0,epoch]=avg_iota_train
      history_avg_iota[1,epoch]=avg_iota_val
      history_avg_iota[2,epoch]=avg_iota_test
    else:
      history_loss[0,epoch]=train_loss/len(trainloader)
      history_loss[1,epoch]=val_loss/len(valloader)
      
      history_acc[0,epoch]=acc_train
      history_acc[1,epoch]=acc_val
      
      history_bacc[0,epoch]=bacc_train
      history_bacc[1,epoch]=bacc_val
      
      history_avg_iota[0,epoch]=avg_iota_train
      history_avg_iota[1,epoch]=avg_iota_val

    #Trace---------------------------------------------------------------------
    if trace>=1:
      if test_data is None:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} AI {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f}  AI: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss/len(trainloader),
          acc_train,
          bacc_train,
          avg_iota_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val,
          avg_iota_val))
      else:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} AI {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f} AI {:.4f} | Test Loss: {:.4f} ACC: {:.4f} BACC: {:.4f} AI: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss/len(trainloader),
          acc_train,
          bacc_train,
          avg_iota_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val,
          avg_iota_val,
          test_loss/len(testloader),
          acc_test,
          bacc_test,
          avg_iota_test))
          
    #Callback-------------------------------------------------------------------
    if use_callback==True:
      if avg_iota_val>best_val_avg_iota:
        if trace>=1:
          print("Val Avg. Iota increased from {:.4f} to {:.4f}".format(best_val_avg_iota,avg_iota_val))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_val_avg_iota=avg_iota_val
        best_acc=acc_val
        best_val_loss=val_loss/len(valloader)
      
      if avg_iota_val==best_val_avg_iota and acc_val>best_acc:
        if trace>=1:
          print("Val Accuracy increased from {:.4f} to {:.4f}".format(best_acc,acc_val))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_acc=acc_val
        best_val_avg_iota=avg_iota_val
        best_val_loss=val_loss/len(valloader)
        
      if avg_iota_val==best_val_avg_iota and acc_val==best_acc and val_loss/len(valloader)<best_val_loss:
        if trace>=1:
          print("Val Loss decreased from {:.4f} to {:.4f}".format(best_val_loss,val_loss/len(valloader)))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_acc=acc_val
        best_val_avg_iota=avg_iota_val
        best_val_loss=val_loss/len(valloader)
  
    #Check if there are furhter information for training-----------------------
    # If there are no addtiononal information. Stop training and continue
    if train_loss/len(trainloader)<0.0001 and acc_train==1 and bacc_train==1:
      break
  
  #Finalize--------------------------------------------------------------------
  if use_callback==True:
    if trace>=1:
      print("Load Best Weights from {}".format(filepath))
    model.load_state_dict(torch.load(filepath))


  history={
    "loss":history_loss.numpy(),
    "accuracy":history_acc.numpy(),
    "balanced_accuracy":history_bacc.numpy(),
    "avg_iota":history_avg_iota.numpy()} 
  return history


def TeClassifierBatchPredict(model,dataset,batch_size):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    dtype=float
    model.to(device,dtype=dtype)
  else:
    dtype=torch.double
    model.to(device,dtype=dtype)
    
  model.eval()
  predictionloader=torch.utils.data.DataLoader(
    dataset,
    batch_size=batch_size,
    shuffle=False)

  with torch.no_grad():
    iteration=0
    for batch in predictionloader:
      inputs=batch["input"]
      inputs = inputs.to(device,dtype=dtype)
      predictions=model(inputs,predication_mode=True)
      
      if iteration==0:
        predictions_list=predictions.to("cpu")
      else:
        predictions_list=torch.concatenate((predictions_list,predictions.to("cpu")), axis=0, out=None)
      iteration+=1
  
  return predictions_list
      
      
    
    
