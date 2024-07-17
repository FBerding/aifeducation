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
  def __init__(self,features, times, hidden, rec, rec_type,rec_bidirectional, intermediate_size,
  attention_type, repeat_encoder, dense_dropout,rec_dropout, encoder_dropout,
  add_pos_embedding, self_attention_heads, target_levels,classification_head=True):
    
    super().__init__()
    
    if isinstance(rec, int):
      rec=[rec]
      
    if isinstance(hidden, int):
      hidden=[hidden]
    
    if rec is None:
      n_rec=0
    else:
      n_rec=len(rec)
    
    if hidden is None:
      n_hidden=0
    else:
      n_hidden=len(hidden)
    
    self.n_target_levels=len(target_levels)
    layer_list=torch.nn.ModuleDict()
    
    current_size=features
    if n_rec>0 or repeat_encoder>0:
      if add_pos_embedding==True:
        layer_list.update({"add_positional_embedding":AddPositionalEmbedding_PT(
          sequence_length=times,
          embedding_dim=features)})
      layer_list.update({"normalizaion_layer":LayerNorm_with_Mask_PT(features=features)})
      #current_size=features
    #else:
    #  layer_list.update({"normalizaion_layer":torch.nn.BatchNorm1d(num_features=times*features)})
    #  current_size=times*features
    
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
                                
    if n_rec>0:
      
      if rec_bidirectional==True:
        for i in range(n_rec):
          layer_list.update({"packandmasking_"+str(i+1):PackAndMasking_PT()})
          if i==0:
            if rec_type=="gru":
              layer_list.update({"bidirectional_gru_"+str(i+1):
                BiDirectionalGRU_PT(
                  input_size=current_size,
                  hidden_size=rec[i])})
              
            elif rec_type=="lstm":
              layer_list.update({"bidirectional_lstm_"+str(i+1):
                BiDirectionalLSTM_PT(
                  input_size=current_size,
                  hidden_size=rec[i])})
          else:
            if rec_type=="gru":
              layer_list.update({"bidirectional_gru_"+str(i+1):
                BiDirectionalGRU_PT(
                  input_size=2*rec[i-1],
                  hidden_size=rec[i])})
            elif rec_type=="lstm":
              layer_list.update({"bidirectional_lstm_"+str(i+1):
                BiDirectionalLSTM_PT(
                  input_size=2*rec[i-1],
                  hidden_size=rec[i])})
          layer_list.update({"unpackandmasking_"+str(i+1):UnPackAndMasking_PT(sequence_length=times)})
          if i!=(n_rec-1):
            layer_list.update({"rec_dropout_"+str(i+1):torch.nn.Dropout(p=rec_dropout)})
      else:
        for i in range(n_rec):
          layer_list.update({"packandmasking_"+str(i+1):PackAndMasking_PT()})
          if i==0:
            if rec_type=="gru":
              layer_list.update({"unidirectional_gru_"+str(i+1):
                UniDirectionalGRU_PT(
                  input_size=current_size,
                  hidden_size=rec[i])})
            elif rec_type=="lstm":
              layer_list.update({"unidirectional_lstm_"+str(i+1):
                UniDirectionalLSTM_PT(
                  input_size=current_size,
                  hidden_size=rec[i])})
          else:
            if rec_type=="gru":
              layer_list.update({"unidirectional_gru_"+str(i+1):
                UniDirectionalGRU_PT(
                  input_size=rec[i-1],
                  hidden_size=rec[i])})
            elif rec_type=="lstm":
              layer_list.update({"unidirectional_lstm_"+str(i+1):
                UniDirectionalLSTM_PT(
                  input_size=rec[i-1],
                  hidden_size=rec[i])})
          layer_list.update({"unpackandmasking_"+str(i+1):UnPackAndMasking_PT(sequence_length=times)})
          if i!=(n_rec-1):
            layer_list.update({"rec_dropout_"+str(i+1):torch.nn.Dropout(p=rec_dropout)})
      
    layer_list.update({"global_average_pooling":GlobalAveragePooling1D_PT()})
      
    if(n_rec>0):
      if rec_bidirectional==True:
        current_size_2=2*rec[len(rec)-1]
      else:
        current_size_2=rec[len(rec)-1]
    else:
      current_size_2=current_size
    
    #Adding standard layer
    if n_hidden>0:
      for i in range(n_hidden):
        if i==0:
          layer_list.update({"dense_"+str(i+1):
            torch.nn.Linear(
              in_features=current_size_2,
              out_features=hidden[i]
            )})
          layer_list.update({"dense_act_fct_"+str(i+1):
            torch.nn.GELU()})
        else:
          layer_list.update({"dense_"+str(i+1):
            torch.nn.Linear(
              in_features=hidden[i-1],
              out_features=hidden[i])})
          layer_list.update({"dense_act_fct_"+str(i+1):
            torch.nn.GELU()})
        if i!=(n_hidden-1):
          layer_list.update({"dense_dropout_"+str(i+1):torch.nn.Dropout(p=dense_dropout)})
    
    if n_hidden==0 and n_rec==0 and repeat_encoder==0 and times==1:
      last_in_features=features*times
    elif  n_hidden>0:
      last_in_features=hidden[len(hidden)-1]
    elif n_rec>0:
      if rec_bidirectional==True:
        last_in_features=2*rec[len(rec)-1]
      else:
        last_in_features=rec[len(rec)-1]
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
shiny_app_active=False):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    model.to(device,dtype=float)
  else:
    model.to(device,dtype=torch.double)
  
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
    history_loss=torch.zeros(size=(3,epochs),requires_grad=False)
    history_acc=torch.zeros(size=(3,epochs),requires_grad=False)
    history_bacc=torch.zeros(size=(3,epochs),requires_grad=False)
  else:
    history_loss=torch.zeros(size=(2,epochs),requires_grad=False)
    history_acc=torch.zeros(size=(2,epochs),requires_grad=False)
    history_bacc=torch.zeros(size=(2,epochs),requires_grad=False)
  
  best_bacc=float('-inf')
  best_val_loss=float('inf')
  
  #GUI ProgressBar
  if shiny_app_active is True:
    current_step=0
    total_epochs=epochs
    total_steps=len(trainloader)
    r.py_update_aifeducation_progress_bar_steps(value=0,total=total_steps,title=("Batch/Step: "+str(0)+"/"+str(total_steps)))
    r.py_update_aifeducation_progress_bar_epochs(value=0,total=epochs,title=("Epoch: "+str(0)+"/"+str(epochs)))

  for epoch in range(epochs):
    
    #Training------------------------------------------------------------------
    train_loss=0.0
    n_matches_train=0
    n_total_train=0
    confusion_matrix_train=torch.zeros(size=(n_classes,n_classes))
    confusion_matrix_train=confusion_matrix_train.to(device,dtype=torch.double)
    
    #Gui ProgressBar
    if shiny_app_active is True:
      current_step=0
    
    model.train(True)
    for batch in trainloader:
      inputs=batch["input"]
      labels=batch["labels"]

      sample_weights=batch["sample_weights"]
      sample_weights=torch.reshape(input=sample_weights,shape=(sample_weights.size(dim=0),1))

      inputs = inputs.to(device)
      labels=labels.to(device)
      sample_weights=sample_weights.to(device)
      
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
      
      #Update Progress Logger
      if shiny_app_active is True:
        current_step+=1
        r.py_update_aifeducation_progress_bar_steps(value=current_step,total=total_steps,title=("Batch/Step: "+str(current_step)+"/"+str(total_steps)))
    
    acc_train=n_matches_train/n_total_train
    bacc_train=torch.sum(torch.diagonal(confusion_matrix_train)/torch.sum(confusion_matrix_train,dim=1))/n_classes

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

        inputs = inputs.to(device)
        labels=labels.to(device)
      
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

    acc_val=n_matches_val/n_total_val
    bacc_val=torch.sum(torch.diagonal(confusion_matrix_val)/torch.sum(confusion_matrix_val,dim=1))/n_classes
    
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

          inputs = inputs.to(device)
          labels=labels.to(device)
        
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
  
      acc_test=n_matches_test/n_total_test
      bacc_test=torch.sum(torch.diagonal(confusion_matrix_test)/torch.sum(confusion_matrix_test,dim=1))/n_classes
    
    
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
    else:
      history_loss[0,epoch]=train_loss/len(trainloader)
      history_loss[1,epoch]=val_loss/len(valloader)
      
      history_acc[0,epoch]=acc_train
      history_acc[1,epoch]=acc_val
      
      history_bacc[0,epoch]=bacc_train
      history_bacc[1,epoch]=bacc_val

    #Trace---------------------------------------------------------------------
    if trace>=1:
      if test_data is None:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss/len(trainloader),
          acc_train,
          bacc_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val))
      else:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f} | Test Loss: {:.4f} ACC: {:.4f} BACC: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss/len(trainloader),
          acc_train,
          bacc_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val,
          test_loss/len(testloader),
          acc_test,
          bacc_test))
          
    #Update ProgressBar in Shiny
    if shiny_app_active is True:
        r.py_update_aifeducation_progress_bar_epochs(value=epoch+1,total=epochs,title=("Epoch: "+str(epoch+1)+"/"+str(epochs)))
        
    
    #Callback-------------------------------------------------------------------
    if use_callback==True:
      if bacc_val>best_bacc:
        if trace>=1:
          print("Val Balanced Accuracy increased from {:.4f} to {:.4f}".format(best_bacc,bacc_val))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_val_loss=val_loss/len(valloader)
      if bacc_val==best_bacc and val_loss/len(valloader)<best_val_loss:
        if trace>=1:
          print("Val Loss decreased from {:.4f} to {:.4f}".format(best_val_loss,val_loss/len(valloader)))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_val_loss=val_loss/len(valloader)
  
  #Finalize--------------------------------------------------------------------
  if use_callback==True:
    if trace>=1:
      print("Load Best Weights from {}".format(filepath))
    model.load_state_dict(torch.load(filepath))


  history={
    "loss":history_loss.numpy(),
    "accuracy":history_acc.numpy(),
    "balanced_accuracy":history_bacc.numpy()} 
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
      
      
    
    
