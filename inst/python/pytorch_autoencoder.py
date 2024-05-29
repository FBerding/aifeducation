import torch
import numpy as np
import math

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
        
    def forward(self, x, encoder_mode=False):
      if encoder_mode==False:
        if self.training==True:
          mask=self.get_mask(x)
          x=x+self.noise_factor*torch.rand(size=x.size())
          x=~mask*x
        x=self.PackAndMasking_PT(x)
        x=self.encoder_1(x)[0]
        x=self.latent_space(x)[0]
        x=self.decoder_1(x)[0]
        x=self.output(x)[0]
        x=self.UnPackAndMasking_PT(x)
        return x
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

    def forward(self, x, encoder_mode=False):
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
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=self.param_w3))
        
        #Decoder
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=torch.transpose(self.param_w3,dim0=1,dim1=0)))
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=torch.transpose(self.param_w2,dim0=1,dim1=0)))
        x=torch.nn.functional.tanh(torch.nn.functional.linear(x,weight=torch.transpose(self.param_w1,dim0=1,dim1=0)))
        
        return x
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
      
def AutoencoderTrain_PT_with_Datasets(model,epochs, trace,batch_size,
train_data,val_data,filepath,use_callback,shiny_app_active=False):
  
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
  history_loss=torch.zeros(size=(2,epochs),requires_grad=False)

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

    #Gui ProgressBar
    if shiny_app_active is True:
      current_step=0
    
    model.train(True)
    for batch in trainloader:
      inputs=batch["input"]
      labels=batch["labels"]

      inputs = inputs.to(device=device,dtype=dtype)
      labels=labels.to(device=device,dtype=dtype)

      optimizer.zero_grad()
      
      outputs=model(inputs,encoder_mode=False)
      loss=loss_fct(outputs,labels)
      loss.backward()
      optimizer.step()
      train_loss +=loss.item()
      
      #Update Progress Logger
      if shiny_app_active is True:
        current_step+=1
        r.py_update_aifeducation_progress_bar_steps(value=current_step,total=total_steps,title=("Batch/Step: "+str(current_step)+"/"+str(total_steps)))
    
    #Validation----------------------------------------------------------------
    val_loss=0.0

    model.eval()
    with torch.no_grad():
      for batch in valloader:
        inputs=batch["input"]
        labels=batch["labels"]

        inputs = inputs.to(device=device,dtype=dtype)
        labels=labels.to(device=device,dtype=dtype)
      
        outputs=model(inputs,encoder_mode=False)
        
        loss=loss_fct(outputs,labels)
        val_loss +=loss.item()
    
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
          
    #Update ProgressBar in Shiny
    if shiny_app_active is True:
        r.py_update_aifeducation_progress_bar_epochs(value=epoch+1,total=epochs,title=("Epoch: "+str(epoch+1)+"/"+str(epochs)))
        
    
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
