import torch
import numpy as np
import math

def calc_SquaredCovSum(x):
  times=x.size(dim=1)
  cov_sum=0.0

  for i in range(times):
    current_time_point=torch.squeeze(x[:,i,:])
    if current_time_point.dim()>1:
      current_cases_index=torch.nonzero(torch.sum(current_time_point,axis=1))
      if current_cases_index.size(dim=0)>1:
        current_cases=torch.squeeze(current_time_point[current_cases_index])
        covariance=torch.cov(torch.transpose(current_cases,dim0=0,dim1=1))
        covariance=torch.square(covariance)
        cov_sum=cov_sum+(torch.sum(covariance)-torch.sum(torch.diag(covariance)))/current_cases.size(dim=0)
  cov_sum=cov_sum/times
  return cov_sum

class LSTMAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self,times, features_in,features_out,noise_factor):
      super().__init__()
      self.features_in=features_in
      self.features_out=features_out
      self.sequence_length=times
      self.noise_factor=noise_factor
      self.difference=self.features_in-self.features_out
      
      self.PackAndMasking_PT=PackAndMasking_PT()
      self.UnPackAndMasking_PT=UnPackAndMasking_PT(sequence_length=self.sequence_length)
      
      self.encoder_1=torch.nn.LSTM(
        input_size=self.features_in,
        hidden_size=math.ceil(self.features_in-self.difference*(1/2)),
        batch_first=True,
        bias=True)
        
      self.latent_space=torch.nn.LSTM(
        input_size=math.ceil(self.features_in-self.difference*(1/2)),
        hidden_size=self.features_out,
        batch_first=True,
        bias=True)
        
      self.decoder_1=torch.nn.LSTM(
        input_size=self.features_out,
        hidden_size=math.ceil(self.features_in-self.difference*(1/2)),
        batch_first=True,
        bias=True)
      
      self.output=torch.nn.LSTM(
        input_size=math.ceil(self.features_in-self.difference*(1/2)),
        hidden_size=self.features_in,
        batch_first=True,
        bias=True)
        
    def forward(self, x, encoder_mode=False, return_scs=False):
      if encoder_mode==False:
        if self.training==True:
          mask=self.get_mask(x)
          x=x+self.noise_factor*torch.rand(size=x.size())
          x=~mask*x
        x=self.PackAndMasking_PT(x)
        x=self.encoder_1(x)[0]
        latent_space=self.latent_space(x)[0]
        x=self.decoder_1(latent_space)[0]
        x=self.output(x)[0]
        x=self.UnPackAndMasking_PT(x)
        if return_scs==False:
          return x
        else:
          return x, calc_SquaredCovSum(self.UnPackAndMasking_PT(latent_space))
      elif encoder_mode==True:
        x=self.PackAndMasking_PT(x)
        x=self.encoder_1(x)[0]
        x=self.latent_space(x)[0]
        x=self.UnPackAndMasking_PT(x)
        return x
    def get_mask(self,x):
      device=('cuda' if torch.cuda.is_available() else 'cpu')
      time_sums=torch.sum(x,dim=2)
      mask=(time_sums==0)
      mask_long=torch.reshape(torch.repeat_interleave(mask,repeats=self.features_in,dim=1),(x.size(dim=0),x.size(dim=1),self.features_in))
      mask_long=mask_long.to(device)
      return mask_long
      
class DenseAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self, features_in,features_out,noise_factor):
      super().__init__()
      self.features_in=features_in
      self.features_out=features_out
      self.noise_factor=noise_factor
      self.difference=self.features_in-self.features_out
      
      self.param_w1=torch.nn.Parameter(torch.randn(math.ceil(self.features_in-self.difference*(2/3)),self.features_in))
      self.param_w2=torch.nn.Parameter(torch.randn(math.ceil(self.features_in-self.difference*(1/3)),math.ceil(self.features_in-self.difference*(2/3))))
      self.param_w3=torch.nn.Parameter(torch.randn(self.features_out,math.ceil(self.features_in-self.difference*(1/3))))
      
      torch.nn.utils.parametrizations.orthogonal(self, "param_w1",orthogonal_map="householder")
      torch.nn.utils.parametrizations.orthogonal(self, "param_w2",orthogonal_map="householder")
      torch.nn.utils.parametrizations.orthogonal(self, "param_w3",orthogonal_map="householder")

    def forward(self, x, encoder_mode=False, return_scs=False):
      if encoder_mode==False:
        #Add noise
        if self.training==True:
          mask=self.get_mask(x)
          x=x+self.noise_factor*torch.rand(size=x.size())
          x=~mask*x
        
        #Encoder
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=self.param_w1))
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=self.param_w2))
        
        #Latent Space
        latent_space=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=self.param_w3))
        
        #Decoder
        x=torch.nn.functional.tanh(torch.nn.functional.linear(latent_space,weight=torch.transpose(self.param_w3,dim0=1,dim1=0)))
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=torch.transpose(self.param_w2,dim0=1,dim1=0)))
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=torch.transpose(self.param_w1,dim0=1,dim1=0)))
        
        if return_scs==False:
          return x
        else:
          return x, calc_SquaredCovSum(latent_space)
      elif encoder_mode==True:
        #Encoder
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=self.param_w1))
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=self.param_w2))
        
        #Latent Space
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=self.param_w3))
        return x
      
    def get_mask(self,x):
      device=('cuda' if torch.cuda.is_available() else 'cpu')
      time_sums=torch.sum(x,dim=2)
      mask=(time_sums==0)
      mask_long=torch.reshape(torch.repeat_interleave(mask,repeats=self.features_in,dim=1),(x.size(dim=0),x.size(dim=1),self.features_in))
      mask_long=mask_long.to(device)
      return mask_long
    
class ConvAutoencoder_with_Mask_PT(torch.nn.Module):
    def __init__(self, features_in,features_out,noise_factor):
      super().__init__()
      self.features_in=features_in
      self.features_out=features_out
      self.noise_factor=noise_factor
      self.difference=self.features_in-self.features_out
      self.stride=1
      self.kernel_size=2
      
      self.param_w1=torch.nn.Parameter(torch.randn(math.ceil(self.features_in-self.difference*(1/2)),self.features_in,self.kernel_size))
      self.param_w2=torch.nn.Parameter(torch.randn(self.features_out,math.ceil(self.features_in-self.difference*(1/2)),self.kernel_size))
      
      self.sequence_reduction=torch.nn.AvgPool1d(kernel_size=(self.kernel_size),stride=1,padding=0)

      torch.nn.utils.parametrizations.orthogonal(self, "param_w1",orthogonal_map="householder")
      torch.nn.utils.parametrizations.orthogonal(self, "param_w2",orthogonal_map="householder")

    def forward(self, x, encoder_mode=False, return_scs=False):
      if encoder_mode==False:
        #Add noise
        if self.training==True:
          mask=self.get_mask(x)
          x=x+self.noise_factor*torch.rand(size=x.size())
          x=~mask*x
        
        #Change position of time and features
        x=torch.transpose(x, dim0=1, dim1=2)
        
        #Encoder
        x=torch.nn.functional.tanh(torch.nn.functional.conv1d(x,weight=self.param_w1,stride=self.stride,padding='same'))

        #Latent Space
        latent_space=torch.nn.functional.tanh(torch.nn.functional.conv1d(x,weight=self.param_w2,stride=self.stride,padding='same'))
        latent_space=torch.transpose(latent_space, dim0=1, dim1=2)
        latent_space=~self.get_mask(latent_space)*latent_space
        latent_space=torch.transpose(latent_space, dim0=1, dim1=2)

        #Decoder
        x=torch.nn.functional.tanh(torch.nn.functional.conv_transpose1d(latent_space,weight=self.param_w2,stride=self.stride,padding=0,output_padding=0))
        x=self.sequence_reduction(x)
        x=torch.nn.functional.tanh(torch.nn.functional.conv_transpose1d(x,weight=self.param_w1,stride=self.stride,padding=0,output_padding=0))
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
    
def AutoencoderTrain_PT_with_Datasets(model,epochs, trace,batch_size,
train_data,val_data,filepath,use_callback,
log_dir=None, log_write_interval=10, log_top_value=0, log_top_total=1, log_top_message="NA"):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    dtype=float
    model.to(device,dtype=dtype)
  else:
    model.to(device,dtype=torch.double)
  
  optimizer=torch.optim.Adam(params=model.parameters(),weight_decay=0)
  
  loss_fct=torch.nn.MSELoss()

  trainloader=torch.utils.data.DataLoader(
    train_data,
    batch_size=batch_size,
    shuffle=True)
    
  valloader=torch.utils.data.DataLoader(
    val_data,
    batch_size=batch_size,
    shuffle=False)
    
  #Tensor for Saving Training History
  history_loss=torch.ones(size=(2,epochs),requires_grad=False)*-100

  best_val_loss=float('inf')
  
  #Log file
  if not (log_dir is None):
    log_file=log_dir+"/aifeducation_state.log"
    log_file_loss=log_dir+"/aifeducation_loss.log"
    last_log=None
    last_log_loss=None
    current_step=0
    total_epochs=epochs
    total_steps=len(trainloader)+len(valloader)

  for epoch in range(epochs):
    #logging
    current_step=0
    
    #Training------------------------------------------------------------------
    train_loss=0.0

    model.train(True)
    for batch in trainloader:
      inputs=batch["input"]
      labels=batch["labels"]

      inputs = inputs.to(device=device,dtype=dtype)
      labels=labels.to(device=device,dtype=dtype)

      optimizer.zero_grad()
      
      outputs=model(inputs,encoder_mode=False,return_scs=True)
      loss=loss_fct(outputs[0],labels)+outputs[1]
      loss.backward()
      optimizer.step()
      train_loss +=loss.item()
      
      #Update log file
      if not (log_dir is None):
        current_step+=1
        last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                  total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                  message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
        last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)
    
    #Validation----------------------------------------------------------------
    val_loss=0.0

    model.eval()
    with torch.no_grad():
      for batch in valloader:
        inputs=batch["input"]
        labels=batch["labels"]

        inputs = inputs.to(device=device,dtype=dtype)
        labels=labels.to(device=device,dtype=dtype)
      
        outputs=model(inputs,encoder_mode=False,return_scs=True)
        
        loss=loss_fct(outputs[0],labels)+outputs[1]
        val_loss +=loss.item()
        
        #Update log file
        if not (log_dir is None):
          current_step+=1
          last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                    total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                    message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
          last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)
    
    #Record History------------------------------------------------------------
    history_loss[0,epoch]=train_loss/len(trainloader)
    history_loss[1,epoch]=val_loss/len(valloader)
    
    #Trace---------------------------------------------------------------------
    if trace>=1:
      print("Epoch: {}/{} Train Loss: {:.8f} | Val Loss: {:.8f}".format(
        epoch+1,
        epochs,
        train_loss/len(trainloader),
        val_loss/len(valloader)))
          
    #Callback-------------------------------------------------------------------
    if use_callback==True:
      if (val_loss/len(valloader))<best_val_loss:
        if trace>=1:
          print("Val Loss decreased from {:.8f} to {:.8f}".format(best_val_loss,val_loss/len(valloader)))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_val_loss=val_loss/len(valloader)

  #Finalize--------------------------------------------------------------------
  if use_callback==True:
    if trace>=1:
      print("Load Best Weights from {}".format(filepath))
    model.load_state_dict(torch.load(filepath))

  history={"loss":history_loss.numpy()} 
    
  return history

def TeFeatureExtractorBatchExtract(model,dataset,batch_size):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    dtype=float
    model.to(device,dtype=dtype)
  else:
    model.to(device,dtype=torch.double)
    dtype=torch.double
    
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
      predictions=model(inputs,encoder_mode=True)
      
      if iteration==0:
        predictions_list=predictions.to("cpu")
      else:
        predictions_list=torch.concatenate((predictions_list,predictions.to("cpu")), axis=0, out=None)
      iteration+=1
  
  return predictions_list
