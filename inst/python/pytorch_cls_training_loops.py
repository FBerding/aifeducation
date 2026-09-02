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

import os
import torch 
import torch.distributed as dist
import torch.multiprocessing as mp
from torch.nn.parallel import DistributedDataParallel as DDP
from torcheval.metrics.functional import multiclass_confusion_matrix
import numpy as np
import math
import safetensors
import datasets
import json
import sys
import importlib.util
import inspect 

class ModelTrainerManager():
  def __init__(self, model_type,ddp_use,train_args,tmp_dir,aife_dir):
    self.model_type=model_type
    self.tmp_dir=tmp_dir
    self.aife_dir=aife_dir
    self.ddp_use=ddp_use
    self.train_args=train_args
    self.backend_ddp="gloo"
    self.world_size=1
  
  @staticmethod
  def init_trainer(model_type,ddp_use,train_args):
    #Init Trainer
    trainer=ModelTrainer(model_type,ddp_use)
    #Add cnfig Information
    if model_type=="ClassifierStandard":
      trainer.config_for_StandardClassifier(**train_args)
    elif model_type=="ClassifierPrototype":
      trainer.config_for_ClassifierPrototype(**train_args)
    elif model_type=="TEFeatureExtractor":
      trainer.config_for_TEFeatureExtractor(**train_args)
    return trainer 
  
  def do_training(self):
    #If no ddp should be used
    if self.ddp_use==False:
      trainer=self.init_trainer(self.model_type,self.ddp_use,self.train_args)
      #Start Training
      trainer.do_training()
      #Return training history
      return trainer.metric_storage
    #If ddp should be used
    else:
      # Write config and weights
      self.train_args["model"].save_config(self.tmp_dir+"/nn_configs.json")
      torch.save(self.train_args["model"].state_dict(),self.tmp_dir+"/nn_weights.pt")
      self.train_args["train_data"].save_to_disk(self.tmp_dir+"train_data")
      self.train_args["val_data"].save_to_disk(self.tmp_dir+"val_data")
      
      reduced_args=self.train_args.copy()
      if "class_weights" in reduced_args:
        reduced_args["class_weights"]=reduced_args["class_weights"].numpy().tolist()
      
      reduced_args= {
        k: v
        for k, v in  reduced_args.items()
          if isinstance(v, (int, str, float, bool, list)) or v is None
      }
      
      if self.train_args["test_data"] is not None:
        self.train_args["test_data"].save_to_disk(self.tmp_dir+"test_data")
      
      #mp.spawn(
      # self.do_training_ddp,
      # args=(self.world_size,self.model_type,self.ddp_use,reduced_args,self.backend_ddp,self.tmp_dir,self.aife_dir),
      # nprocs=self.world_size,
      # join=True
      #)
      do_training_ddp(
        rank=0, 
        world_size=1,
        model_type=self.model_type,
        ddp_use=self.ddp_use,
        train_args=reduced_args,
        backend_ddp=self.backend_ddp,
        tmp_dir=self.tmp_dir,
        aife_dir=self.aife_dir
      )
  

  
  @staticmethod
  def setup_ddp(self,rank,world_size,backend_ddp):
    os.environ["MASTER_ADDR"] = "localhost"
    os.environ["MASTER_PORT"] = "12355" # Freier Port auf dem System
    if dist.is_initialized():
       dist.destroy_process_group()
    dist.init_process_group(
        backend=backend_ddp,
        init_method="env://",
        world_size=world_size,
        rank=rank
    )
    #torch.cuda.set_device(rank)  
    
class ModelTrainer():
  def __init__(self,model_type,ddp_use):
    self.model_type=model_type
    self.ddp_use=ddp_use
    self.amp_dtype=torch.bfloat16
    self.device_type = self.get_device_type()
    self.device = self.get_device()
    #self.device_type = "cpu"
    #self.device = "cpu"
    self.dtype = self.get_dtype()
    self.world_size=1
  #--------------  
  def set_rank_word_size(self,rank,world_size):
    self.device=rank
    self.world_size=world_size
  #------------------------------------------------------------------------------
  def config_for_StandardClassifier(self,model,features,times,final_dim,loss_cls_fct_name, optimizer_method,scheduler_type,amp, lr_rate,lr_min, lr_warm_up_ratio, epochs, trace,batch_size,
    train_data,val_data,filepath,use_callback,n_classes,class_weights,comp_use,comp_backend,comp_mode,test_data=None,
    log_dir=None, log_write_interval=10, log_top_value=0, log_top_total=1, log_top_message="NA"):
    self.model=model
    self.features=features
    self.times=times
    self.final_dim=final_dim
    self.loss_cls_fct_name=loss_cls_fct_name
    self.optimizer_method=optimizer_method
    self.scheduler_type=scheduler_type
    self.amp=amp
    self.lr_rate=lr_rate
    self.lr_min =lr_min
    self.lr_warm_up_ratio=lr_warm_up_ratio
    self.epochs=epochs
    self.trace=trace
    self.batch_size=batch_size
    self.train_data=train_data
    self.val_data=val_data
    self.filepath=filepath
    self.use_callback=use_callback
    self.n_classes=n_classes
    self.class_weights=class_weights
    self.comp_use=comp_use
    self.comp_backend=comp_backend
    self.comp_mode=comp_mode
    self.test_data=test_data
    self.log_dir= log_dir
    self.log_write_interval=log_write_interval
    self.log_top_value=log_top_value
    self.log_top_total =log_top_total
    self.log_top_message=log_top_message
    #Loss Function
    self.loss_fct=get_loss_cls_fct(
      name=self.loss_cls_fct_name,
      class_weights=self.class_weights
    )
  def config_for_ClassifierPrototype(self,model,features,times,final_dim,loss_pt_fct_name , optimizer_method, scheduler_type, amp,comp_use,comp_backend,comp_mode,lr_rate,lr_min, lr_warm_up_ratio, epochs, trace,Ns,Nq,
    loss_alpha, loss_margin, train_data,val_data,filepath,use_callback,n_classes,sampling_separate,sampling_shuffle,test_data=None,
    log_dir=None, log_write_interval=10, log_top_value=0, log_top_total=1, log_top_message="NA"):
    self.model=model
    self.features=features
    self.times=times
    self.final_dim=final_dim
    self.loss_pt_fct_name =loss_pt_fct_name
    self.optimizer_method =optimizer_method
    self.scheduler_type=scheduler_type
    self.amp=amp
    self.comp_use=comp_use
    self.comp_backend=comp_backend
    self.comp_mode=comp_mode
    self.lr_rate=lr_rate
    self.lr_min =lr_min
    self.lr_warm_up_ratio=lr_warm_up_ratio
    self.epochs=epochs
    self.trace=trace
    self.Ns=Ns
    self.Nq=Nq
    self.loss_alpha=loss_alpha
    self.loss_margin=loss_margin
    self.train_data=train_data
    self.val_data=val_data
    self.filepath=filepath
    self.use_callback=use_callback
    self.n_classes=n_classes
    self.sampling_separate=sampling_separate
    self.sampling_shuffle=sampling_shuffle
    self.test_data=test_data
    self.log_dir=log_dir
    self.log_write_interval=log_write_interval
    self.log_top_value=log_top_value
    self.log_top_total=log_top_total
    self.log_top_message=log_top_message
    #Loss Function
    self.loss_fct=get_loss_cls_pt_fct(
      name=self.loss_pt_fct_name,
      margin=self.loss_margin,
      alpha=self.loss_alpha
    )
  def config_for_TEFeatureExtractor(self,model,optimizer_method,times,features,scheduler_type,amp, lr_rate,lr_min, lr_warm_up_ratio, epochs, trace,batch_size,
    train_data,val_data,filepath,use_callback,comp_use,comp_backend,comp_mode,
    log_dir=None, log_write_interval=10, log_top_value=0, log_top_total=1, log_top_message="NA"):
    self.model=model
    self.optimizer_method=optimizer_method
    self.times=times
    self.features=features
    self.scheduler_type=scheduler_type
    self.amp=amp
    self.comp_use=comp_use
    self.comp_backend=comp_backend
    self.comp_mode=comp_mode
    self.lr_rate=lr_rate
    self.lr_min=lr_min
    self.lr_warm_up_ratio=lr_warm_up_ratio
    self.epochs=epochs
    self.trace=trace
    self.batch_size=batch_size
    self.train_data=train_data
    self.val_data=val_data
    self.test_data=None
    self.filepath=filepath
    self.use_callback=use_callback
    self.log_dir=log_dir
    self.log_write_interval=log_write_interval
    self.log_top_value=log_top_value
    self.log_top_total=log_top_total
    self.log_top_message=log_top_message
    #Loss
    self.loss_fct=torch.nn.MSELoss()
  def get_device(self):
    return 'cuda' if torch.cuda.is_available() else 'cpu'
  def get_device_type(self):
    return 'cuda' if torch.cuda.is_available() else 'cpu'
  
  def get_dtype(self):
    if self.device=="cpu":
      current_dtype=torch.float
    else:
      current_dtype=torch.float
      
  def prepare_dataloader(self):
    if self.model_type=="ClassifierStandard":
      self.trainloader, self.valloader, self.testloader = self.build_data_loaders(
        train_data=self.train_data,
        val_data=self.val_data,
        test_data=self.test_data,
        batch_size=self.batch_size,
        pin_memory = True if self.device=="cuda" else False,
        comp_use=self.comp_use
      )
    elif self.model_type=="ClassifierPrototype":
      self.trainloader,self.valloader,self.testloader=self.build_data_loaders_pt(
        train_data=self.train_data, 
        val_data=self.val_data, 
        sampling_shuffle=self.sampling_shuffle,
        sampling_separate=self.sampling_separate,
        Ns=self.Ns,
        Nq=self.Nq, 
        test_data=self.test_data, 
        pin_memory=True if self.device=="cuda" else False,
        comp_use=self.comp_use
      )
    elif self.model_type=="TEFeatureExtractor":
      self.trainloader, self.valloader, self.testloader = self.build_data_loaders(
        train_data=self.train_data,
        val_data=self.val_data,
        test_data=self.test_data,
        batch_size=self.batch_size,
        pin_memory = True if self.device=="cuda" else False,
        comp_use=self.comp_use
      )      
  def build_data_loaders(self,train_data, val_data, batch_size, test_data=None, pin_memory=False,comp_use=False,ddp_use=False):
    trainloader=torch.utils.data.DataLoader(
      train_data,
      batch_size=batch_size,
      pin_memory=pin_memory,
      drop_last =True,
      num_workers=0,
      sampler=get_sampler(train_data,ddp_use=ddp_use,rank=self.device,world_size=self.world_size),
      shuffle=not ddp_use)
    valloader=torch.utils.data.DataLoader(
      val_data,
      batch_size=batch_size,
      pin_memory=pin_memory,
      drop_last =True,
      num_workers=0,
      sampler=get_sampler(val_data,ddp_use=ddp_use,rank=self.device,world_size=self.world_size),
      shuffle=not ddp_use)
    if not (test_data is None):
      testloader=torch.utils.data.DataLoader(
        test_data,
        batch_size=batch_size,
        pin_memory=pin_memory,
        drop_last =True,
        num_workers=0,
        sampler=get_sampler(test_data,ddp_use=ddp_use,rank=self.device,world_size=self.world_size),
        shuffle= not ddp_use)
    else:
      testloader=None
    return trainloader, valloader, testloader
  
  def build_data_loaders_pt(self,train_data, val_data, Ns,Nq,sampling_separate,sampling_shuffle, test_data=None, pin_memory=False,comp_use=False):
    ProtoNetSampler_Train=MetaLernerBatchSampler(
    targets=train_data["labels"][range(0,len(train_data))],
    Ns=Ns,
    Nq=Nq,
    separate=sampling_separate,
    shuffle=sampling_shuffle)
    trainloader=torch.utils.data.DataLoader(
      train_data,
      pin_memory = pin_memory,
      batch_sampler=ProtoNetSampler_Train)
    valloader=torch.utils.data.DataLoader(
      val_data,
      pin_memory = pin_memory,
      batch_size=Ns+Nq,
      drop_last=comp_use,
      shuffle=False)
    if not (test_data is None):
      testloader=torch.utils.data.DataLoader(
        test_data,
        pin_memory =pin_memory,
        batch_size=Ns+Nq,
        drop_last=comp_use,
        shuffle=False)
    else:
      testloader=None
    return trainloader, valloader, testloader   
      
  def create_optimizer_scaler_scheduler(self):
    #Create optimizer
    self.optimizer=get_Optimizer(
      self.optimizer_method,
      params=self.model_w_loss.parameters(),
      lr_rate=self.lr_rate
    )
    #Create scheduler
    self.scheduler=get_lr_scheduler(
      optimizer=self.optimizer,
      scheduler_type=self.scheduler_type,
      lr_warm_up_ratio=self.lr_warm_up_ratio,
      total_epochs=self.epochs,
      batches_per_epoch=len(self.trainloader),
      max_lr=self.lr_rate,
      min_lr=self.lr_min
    )
    #Create Amp_Scaler
    self.amp_scaler=torch.amp.GradScaler(self.device ,enabled=self.amp)
  def create_static_container(self):
     if self.model_type=="ClassifierStandard":
        self.static_input=torch.randn((self.batch_size,self.times,self.features),device=self.device,dtype=self.dtype)
        self.static_label=torch.randn((self.batch_size,self.n_classes),device=self.device,dtype=self.dtype)
        self.static_sample_weights=torch.ones((self.batch_size,1),device=self.device,dtype=self.dtype)
     elif self.model_type=="ClassifierPrototype":
        self.static_sample_inputs=torch.randn((self.n_classes*self.Ns,self.times,self.features),device=self.device,dtype=self.dtype)
        self.static_query_inputs=torch.randn((self.n_classes*self.Nq,self.times,self.features),device=self.device,dtype=self.dtype)
        self.static_sample_classes=torch.randn((self.n_classes*self.Ns),device=self.device,dtype=self.dtype)
        self.static_query_classes=torch.randn((self.n_classes*self.Nq),device=self.device,dtype=self.dtype)
        self.static_class_labels=torch.randn((self.n_classes),device=self.device,dtype=self.dtype)
        
        self.static_input=torch.randn((self.Ns+self.Nq,self.times,self.features),device=self.device,dtype=self.dtype)
        self.static_label=torch.randn((self.Ns+self.Nq),device=self.device,dtype=self.dtype)  
     elif self.model_type=="TEFeatureExtractor":
        self.static_input=torch.randn((self.batch_size,self.times,self.features),device=self.device,dtype=self.dtype)
        self.static_label=torch.randn((self.batch_size,self.times,self.features),device=self.device,dtype=self.dtype)
  def create_metric_container(self):
    self.elc=0
    if self.model_type=="ClassifierStandard" or self.model_type=="ClassifierPrototype":
        #Numpys for Saving Training History
        self.metric_storage=create_metric_storage(
          metric_names=["loss","accuracy","balanced_accuracy","avg_iota","s_avg_iota"],
          epochs=self.epochs,
          inc_test=True if not (self.test_data is None) else False
        )
        # Init checkpoint values
        self.best_bacc=float('-inf')
        self.best_acc=float('-inf')
        self.best_val_loss=float('inf')
        self.best_val_avg_iota=float('-inf')
    elif self.model_type=="TEFeatureExtractor":
      #Numpys for Saving Training History
      self.metric_storage=create_metric_storage(
        metric_names=["loss"],
        epochs=self.epochs,
        inc_test=True if not (self.test_data is None) else False
      )
      self.best_val_loss=float('inf')

  def prepare_logger(self):
    self.PrgInd=ProgressLogger()
    self.PrgInd.set_start_time()
    total_steps=len(self.trainloader)+len(self.valloader)
    if not (self.test_data is None):
      total_steps=total_steps+len(self.testloader)
    self.logger=LogWriter(
      log_file=self.log_dir+"/aifeducation_state.log" if not (self.log_dir is None) else None,
      log_file_loss =self.log_dir+"/aifeducation_loss.log" if not (self.log_dir is None) else None,
      value_top = self.log_top_value, 
      value_middle = 0, 
      value_bottom = 0,
      total_top = self.log_top_total, 
      total_middle = self.epochs, 
      total_bottom = total_steps, 
      message_top = self.log_top_message, 
      message_middle = "Epoch",
      message_bottom = "Steps",
      last_log = None, 
      write_interval = self.log_write_interval
    )
  def create_ModelWithLoss(self):
    self.model_w_loss=ModelWithLoss(
      model=self.model,
      loss_fct=self.loss_fct,
      model_type=self.model_type
    )
  def prepare_ModelWithLoss(self):
    self.trainer=self.model_w_loss.to(dtype=self.dtype,device=self.device)
    #Compile
    if self.comp_use:
      if self.ddp_use:
        if self.trace: 
          print("Compile model with "+self.comp_backend+" and DDP.")
        #self.trainer=torch.compile(DDP(self.trainer, device_ids=[self.device]),backend=self.comp_backend,fullgraph=True,dynamic=False,mode=self.comp_mode)
        self.trainer=DDP(torch.compile(self.trainer,backend=self.comp_backend,fullgraph=True,dynamic=False,mode=self.comp_mode),device_ids=[self.device])
      else:
        if self.trace: 
          print("Compile model with "+self.comp_backend+".")
        self.trainer=torch.compile(self.trainer,backend=self.comp_backend,fullgraph=False,dynamic=False,mode=self.comp_mode)
    else:
      if self.ddp_use:
        if self.trace:
          print("Train model with DDP.")
        self.trainer=DDP(self.trainer, device_ids=[self.device])
      else:
        self.trainer=self.trainer
      
  def run_epoch_cls(self,cblock,epoch,dataloader):
    # Init Metrics
    total_loss=0.0
    confusion_matrix=torch.zeros(size=(self.n_classes,self.n_classes),device=self.device,dtype=self.dtype)
    prob_confusion_matrix=torch.zeros(size=(self.n_classes,self.n_classes),device=self.device,dtype=self.dtype)
    #Set state of the model
    if cblock=="train":
      self.trainer.train()
      ctx=torch.enable_grad()
    else:
      self.trainer.eval()
      ctx=torch.no_grad()
    # Run Batches
    for batch in dataloader:
      with ctx:
        if cblock=="train":
          self.optimizer.zero_grad(set_to_none=True)
        #Prepare Data
        inputs=batch["input"]
        labels=batch["labels"]
        inputs = inputs.to(device=self.device,dtype=self.dtype,non_blocking=True)
        labels = labels.to(device=self.device,dtype=self.dtype,non_blocking=True)
        self.static_input.copy_(inputs)
        self.static_label.copy_(labels)
        # If in batch.keys load the weights if not use the standard weights which
        #are one for every case
        if "sample_weights" in batch.keys():
          sample_weights=batch["sample_weights"]
          sample_weights=torch.reshape(input=sample_weights,shape=(sample_weights.size(dim=0),1))
          sample_weights=sample_weights.to(device=self.device,dtype=self.dtype)
          self.static_sample_weights.copy_(sample_weights) 
        #Forward
        with torch.autocast(device_type=self.device_type, dtype=self.amp_dtype, enabled=self.amp):
          loss,output=self.trainer(self.static_input,self.static_label,self.static_sample_weights)
        #Backward
        if cblock=="train":
          self.amp_scaler.scale(loss).backward()
          self.amp_scaler.unscale_(self.optimizer)
          torch.nn.utils.clip_grad_norm_(self.model.parameters(), max_norm=1.0,foreach=True)
          self.amp_scaler.step(self.optimizer)
          self.amp_scaler.update()
          if self.scheduler is not None:
            self.scheduler.step()
        #Calculate CLS Statistics
        loss=loss.detach()
        output=output.detach()
        total_loss +=loss
        label_idx=labels.max(dim=1).indices
        confusion_matrix+=multiclass_confusion_matrix(input=output,target=label_idx,num_classes=self.n_classes,normalize = None)
        prob_confusion_matrix+=create_p_confusion_matrix(torch.nn.Softmax(dim=1)(output),label_idx=label_idx,num_classes=self.n_classes)
      #Update log file
      self.logger.inc_value("bottom")
      self.logger.write_log()
      self.logger.write_history_log(self.metric_storage["loss"])
    #Calc final metrics for epoch
    results=calc_cls_performance_measures(
      confusion_matrix=confusion_matrix,
      prob_confusion_matrix=prob_confusion_matrix,
      n_classes=self.n_classes
    )
    results.update({"loss":total_loss/len(dataloader)})
    #Save metrics
    add_metrics(
      metrics=results,
      storage=self.metric_storage,
      cblock=cblock,
      epoch=epoch
    )
    return results
  
  def run_epoch_cls_pt(self,cblock,epoch,dataloader):
  # Init Metrics
    total_loss=0.0
    confusion_matrix=torch.zeros(size=(self.n_classes,self.n_classes),device=self.device,dtype=self.dtype)
    prob_confusion_matrix=torch.zeros(size=(self.n_classes,self.n_classes),device=self.device,dtype=self.dtype)
    if cblock=="train":
      self.trainer.train()
      ctx=torch.enable_grad()
    else:
      self.trainer.eval()
      ctx=torch.no_grad()

    for batch in dataloader:
      with ctx:
        inputs=batch["input"]
        labels=batch["labels"]
        if cblock=="train":
          sample_inputs=inputs[0:(self.n_classes*self.Ns)].clone()
          query_inputs=inputs[(self.n_classes*self.Ns):(self.n_classes*(self.Ns+self.Nq))].clone()
          sample_classes=labels[0:(self.n_classes*self.Ns)].clone()
          query_classes=labels[(self.n_classes*self.Ns):(self.n_classes*(self.Ns+self.Nq))].clone()
          
          sample_inputs = sample_inputs.to(self.device,dtype=self.dtype,non_blocking=True)
          query_inputs = query_inputs.to(self.device,dtype=self.dtype,non_blocking=True)
          sample_classes = sample_classes.to(self.device,dtype=self.dtype,non_blocking=True)
          query_classes = query_classes.to(self.device,dtype=self.dtype,non_blocking=True)
          
          class_labels=torch.unique(sample_classes,sorted=True)
          class_labels=class_labels.to(self.device,dtype=self.dtype,non_blocking=True)
          
          self.static_sample_inputs.copy_(sample_inputs)
          self.static_query_inputs.copy_(query_inputs)
          self.static_sample_classes.copy_(sample_classes)
          self.static_query_classes.copy_(query_classes)
          self.static_class_labels.copy_(class_labels)
          #Train Step
          with torch.autocast(device_type=self.device_type, dtype=self.amp_dtype, enabled=self.amp):
            loss, outputs=self.trainer(
                static_query_inputs=self.static_query_inputs,
                static_query_classes=self.static_query_classes,
                static_sample_inputs=self.static_sample_inputs,
                static_sample_classes=self.static_sample_classes,
                static_class_labels=self.static_class_labels
                )
          #Backward
          if cblock=="train":
            self.amp_scaler.scale(loss).backward()
            self.amp_scaler.unscale_(self.optimizer)
            torch.nn.utils.clip_grad_norm_(self.model.parameters(), max_norm=1.0,foreach=True)
            self.amp_scaler.step(self.optimizer)
            self.amp_scaler.update()
            if self.scheduler is not None:
              self.scheduler.step() 
          #Calculate CLS Statistics    
          loss=loss.detach()
          outputs=outputs
          #Metrics
          total_loss +=loss.item()
          pred_idx=outputs[0].detach().max(dim=1).indices.to(dtype=torch.long,device=self.device)
          label_idx=query_classes.to(dtype=torch.long,device=self.device)  
        else:
          inputs = inputs.to(self.device,dtype=self.dtype,non_blocking=True)
          labels = labels.to(self.device,dtype=self.dtype,non_blocking=True)
          self.static_input.copy_(inputs)
          self.static_label.copy_(labels)
          #Validation stept
          with torch.autocast(device_type=self.device_type, dtype=self.amp_dtype, enabled=self.amp):
            loss,outputs=self.trainer(
              static_query_inputs=self.static_input,
              static_query_classes=self.static_label,
              static_sample_inputs=None,
              static_sample_classes=None,
              static_class_labels=None
              )
      #Metrics
      total_loss +=loss.item()
      pred_idx=outputs[0].max(dim=1).indices.to(dtype=torch.long,device=self.device)
      label_idx=outputs[2].to(dtype=torch.long,device=self.device)
      
      confusion_matrix+=multiclass_confusion_matrix(input=pred_idx,target=label_idx,num_classes=self.n_classes,normalize = None)
      prob_confusion_matrix+=create_p_confusion_matrix(torch.nn.Softmax(dim=1)(outputs[0]),label_idx=label_idx,num_classes=self.n_classes)
      
      #Update log file
      self.logger.inc_value("bottom")
      self.logger.write_log()
      self.logger.write_history_log(self.metric_storage["loss"])
    
    #Calculate prototypes
    if cblock=="train":
      self.trainer.model.eval()
      with torch.no_grad():
        class_mean_prototypes,class_label=calc_trained_prototypes_batch(
          n_classes=self.n_classes,
          model=self.trainer.model,
          data_loader=dataloader,
          device=self.device,
          dtype=self.dtype
          )
        self.trainer.model.set_trained_prototypes(
          prototypes=class_mean_prototypes,
          class_lables=class_label
          )
    #Calc final metrics for epoch
    results=calc_cls_performance_measures(
      confusion_matrix=confusion_matrix,
      prob_confusion_matrix=prob_confusion_matrix,
      n_classes=self.n_classes
    )
    results.update({"loss":total_loss/len(dataloader)})
    #Save metrics
    add_metrics(
      metrics=results,
      storage=self.metric_storage,
      cblock=cblock,
      epoch=epoch
    )
    return results
  
  def run_epoch_autoencoder(self,cblock,epoch,dataloader):
    total_loss=0.0
    if cblock=="train":
      self.trainer.train()
      ctx=torch.enable_grad()
    else:
      self.trainer.eval()
      ctx=torch.no_grad()
    for batch in dataloader:
      with ctx:
        inputs=batch["input"]
        labels=batch["labels"]
        inputs = inputs.to(self.device,dtype=self.dtype)
        labels=labels.to(self.device,dtype=self.dtype)
        self.static_input.copy_(inputs,non_blocking=True)
        self.static_label.copy_(labels,non_blocking=True)
        #Forward
        with torch.autocast(device_type=self.device_type, dtype=self.amp_dtype, enabled=self.amp):
          loss,output=self.trainer.train_and_eval_feature_extractor(self.static_input,self.static_label)
        #Backward
        if cblock=="train":
          self.amp_scaler.scale(loss).backward()
          self.amp_scaler.unscale_(self.optimizer)
          torch.nn.utils.clip_grad_norm_(self.model.parameters(), max_norm=1.0,foreach=True)
          self.amp_scaler.step(self.optimizer)
          self.amp_scaler.update()
          if self.scheduler is not None:
            self.scheduler.step()
        #Calculate CLS Statistics
        loss=loss.detach()
        output=output.detach()
        #Metrics
        total_loss +=loss.item()
        #Update log file
        self.logger.inc_value("bottom")
        self.logger.write_log()
        self.logger.write_history_log(self.metric_storage["loss"])
    #Calc final metrics for epoch
    results={"loss":total_loss/len(dataloader)}
    #Save metrics
    add_metrics(
      metrics=results,
      storage=self.metric_storage,
      cblock=cblock,
      epoch=epoch
    )
    return results
  
  def check_and_set_checkpoints_cls(self,epoch,acc_val,bacc_val,avg_iota_val,val_loss):
    if self.use_callback==True:
        if (avg_iota_val>self.best_val_avg_iota) or (avg_iota_val==self.best_val_avg_iota and acc_val>self.best_acc) or (avg_iota_val==self.best_val_avg_iota and acc_val==self.best_acc and val_loss<self.best_val_loss):
          if isinstance(self.trainer.model, torch._dynamo.eval_frame.OptimizedModule):
            print("model is compiled")
            torch.save(self.trainer.model._orig_mod.state_dict(),self.filepath)
          else:
            torch.save(self.trainer.model.state_dict(),self.filepath)
          self.best_bacc=bacc_val
          self.best_val_avg_iota=avg_iota_val
          self.best_acc=acc_val
          self.best_val_loss=val_loss
          self.metric_storage["checkpoints"][epoch]=1
          self.elc=epoch+1
          
  def check_and_set_checkpoints_loss(self,epoch,val_loss):
    if self.use_callback==True:
      if val_loss<=self.best_val_loss:
        if isinstance(self.trainer.model, torch._dynamo.eval_frame.OptimizedModule):
          torch.save(self.trainer.model._orig_mod.state_dict(),self.filepath)
        else:
          torch.save(self.trainer.model.state_dict(),self.filepath)
        self.best_val_loss=val_loss
        self.metric_storage["checkpoints"][epoch]=1
        self.elc=epoch+1

  def check_convergence(self,train_results):
    if self.model_type=="ClassifierStandards":
      if train_results["loss"]<1e-3 and train_results["s_avg_iota"]>=.98:
        if trace:
          print("\n")
      return True
    
  def run_epochs(self):
    if self.model_type=="ClassifierStandard":
      for epoch in range(self.epochs):
        train_results=self.run_epoch_cls(cblock="train",epoch=epoch,dataloader=self.trainloader)
        val_results=self.run_epoch_cls(cblock="val",epoch=epoch,dataloader=self.valloader)
        if self.testloader is not None:
          test_results=self.run_epoch_cls("test",epoch=epoch,dataloader=self.testloader)
        #Update logger   
        self.logger.reset_value(level="bottom")
        self.logger.inc_value(level="middle")
        #Callback-------------------------------------------------------------------
        self.check_and_set_checkpoints_cls(
          epoch=epoch,
          acc_val=val_results["accuracy"],
          bacc_val=val_results["balanced_accuracy"],
          avg_iota_val=val_results["s_avg_iota"],
          val_loss=val_results["loss"]
        )
        #Trace---------------------------------------------------------------------
        self.PrgInd.print_epoch_results(
          trace=self.trace,
          loss_only=False,
          metric_storage=self.metric_storage,
          epoch=epoch,
          epochs=self.epochs,
          metric_criterion="s_avg_iota",
          best_metric=self.best_val_avg_iota,
          best_loss=self.best_val_loss,
          elc=self.elc
        )
        #Check if there are furhter information for training-----------------------
        # If there are no addtiononal information. Stop training and continue
        if self.check_convergence(train_results):
          break
    elif self.model_type=="ClassifierPrototype":
      for epoch in range(self.epochs):
        train_results=self.run_epoch_cls_pt(cblock="train",epoch=epoch,dataloader=self.trainloader)
        val_results=self.run_epoch_cls_pt(cblock="val",epoch=epoch,dataloader=self.valloader)
        if self.testloader is not None:
          test_results=self.run_epoch_cls_pt("test",epoch=epoch,dataloader=self.testloader)
        #Update logger   
        self.logger.reset_value(level="bottom")
        self.logger.inc_value(level="middle")
        #Callback-------------------------------------------------------------------
        self.check_and_set_checkpoints_cls(
          epoch=epoch,
          acc_val=val_results["accuracy"],
          bacc_val=val_results["balanced_accuracy"],
          avg_iota_val=val_results["s_avg_iota"],
          val_loss=val_results["loss"]
        )
        #Trace---------------------------------------------------------------------
        self.PrgInd.print_epoch_results(
          trace=self.trace,
          loss_only=False,
          metric_storage=self.metric_storage,
          epoch=epoch,
          epochs=self.epochs,
          metric_criterion="s_avg_iota",
          best_metric=self.best_val_avg_iota,
          best_loss=self.best_val_loss,
          elc=self.elc
        )
        #Check if there are furhter information for training-----------------------
        # If there are no addtiononal information. Stop training and continue
        if self.check_convergence(train_results):
          break
        
    elif self.model_type=="TEFeatureExtractor":
        for epoch in range(self.epochs):
          train_results=self.run_epoch_autoencoder(cblock="train",epoch=epoch,dataloader=self.trainloader)
          val_results=self.run_epoch_autoencoder(cblock="val",epoch=epoch,dataloader=self.valloader)
          #Update logger   
          self.logger.reset_value(level="bottom")
          self.logger.inc_value(level="middle")
          #Callback-------------------------------------------------------------------
          self.check_and_set_checkpoints_loss(
            epoch=epoch,
            val_loss=val_results["loss"]
          )
          #Trace---------------------------------------------------------------------
          self.PrgInd.print_epoch_results(
            trace=self.trace,
            loss_only=True,
            metric_storage=self.metric_storage,
            epoch=epoch,
            epochs=self.epochs,
            metric_criterion="loss",
            best_metric=None,
            best_loss=self.best_val_loss,
            elc=self.elc
          )
    else:
      print("Error")
  
  def print_final_performance(self):
    if self.model_type!="TEFeatureExtractor":
      self.PrgInd.print_final_performance(trace=self.trace,metric_storage=self.metric_storage,elc=self.elc)
    
  def do_training(self):
    # 1. Prepare Model
    self.create_ModelWithLoss()
    self.prepare_ModelWithLoss()
    # 2. Prepare Datasloader
    self.prepare_dataloader()
    # 3. Create Optimizer, Scheduler, and Scaler for AMP
    self.create_optimizer_scaler_scheduler()
    # 4. Create static objects for faster compilation
    self.create_static_container()
    # 5. Create objects for storing learning history
    self.create_metric_container()
    # 6. Prepare Logger
    self.prepare_logger()
    # 8. Run Epochs
    self.run_epochs()
    # 9. Finalize
    self.print_final_performance()
    if self.use_callback==True:
      self.model.load_state_dict(torch.load(self.filepath,weights_only=True))
    return self.metric_storage
    
#------------------------------------------------------------------------------


#Functions that are part of the training loop
def get_device():
  return 'cuda' if torch.cuda.is_available() else 'cpu'

def get_dtype(device):
  if device=="cpu":
    current_dtype=torch.float
  else:
    current_dtype=torch.float
    
def get_loss_cls_fct(name,class_weights):
  if name =="CrossEntropyLoss":
    loss_fct=torch.nn.CrossEntropyLoss(
        reduction="none",
        weight = class_weights)
  elif name =="FocalLoss":
    loss_fct=focal_loss(
      gamma=2,
      class_weights = class_weights,
      scale_level = "nominal"
    )
  elif name =="FocalLossOrdinal":
    loss_fct=focal_loss(
      gamma=2,
      class_weights = class_weights,
      scale_level = "ordinal"
    ) 
  elif name =="AEMLoss":
    loss_fct=aem_loss(
      eps=1e-6
    )    
  return loss_fct

def get_loss_cls_pt_fct(name,margin,alpha):
  if name=="MultiWayContrastiveLoss":
    fct=multi_way_contrastive_loss(
      alpha=alpha,
      margin=margin)
  elif name=="MultiWayContrastiveLossFC":
    fct=multi_way_contrastive_loss_fc(
      alpha=alpha,
      margin=margin,
      scale_level="nominal")
  elif name=="MultiWayContrastiveLossFCOrdinal":
    fct=multi_way_contrastive_loss_fc(
      alpha=alpha,
      margin=margin,
      scale_level="ordinal")    
  elif name=="FocalLoss":
    fct=focal_loss_pt(
      class_weights=None,
      gamma=2,
      scale_level="nominal"
    )
  elif name=="FocalLossOrdinal":
    fct=focal_loss_pt(
      class_weights=None,
      gamma=2,
      scale_level="ordinal"
    )
  elif name =="AEMLoss":
    fct=aem_loss_pt(
      eps=1e-6
    )      
  return fct

def get_sampler(dataset,ddp_use=False,rank=0,world_size=1):
  if ddp_use:
    return torch.utils.data.distributed.DistributedSampler(
    dataset, 
    num_replicas=world_size, 
    rank=rank, 
    shuffle=True, 
    seed=0, 
    drop_last=True)
  else:
    return None


def create_metric_storage(metric_names,epochs,inc_test):
  storage={}
  for metric in metric_names:
    if inc_test:
      tmp_metric_storage=np.ones((3,epochs))*-100
    else:
      tmp_metric_storage=np.ones((2,epochs))*-100
    storage[metric]=  tmp_metric_storage
  storage["checkpoints"]=np.zeros((epochs))
  return storage

prob=torch.from_numpy(np.array([[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]]))

def create_p_confusion_matrix(prob,label_idx,num_classes):
  with torch.no_grad():
    one_hot=torch.nn.functional.one_hot(label_idx, num_classes=num_classes) # B,T
    one_hot=torch.unsqueeze(one_hot,dim=2) # B, T, 1
    one_hot=one_hot.expand((one_hot.size(0),one_hot.size(1),one_hot.size(1))) #B,T,A

    prob_exp=torch.unsqueeze(prob,dim=1) # B,1,A
    prob_exp=prob_exp.expand(one_hot.size()) #B,T,A
    
    confusion_matrix=torch.sum(one_hot*prob_exp,dim=0) # T, A
  return confusion_matrix

def calc_cls_performance_measures(confusion_matrix,prob_confusion_matrix,n_classes):
  with torch.no_grad():
    diagonal=torch.diagonal(confusion_matrix) #(n_classes)
    total_sum=torch.sum(confusion_matrix) #()
    true_classes=torch.sum(confusion_matrix,dim=1) #(n_classes)
    col_sum=torch.sum(confusion_matrix,dim=0) #(n_classes)
  
    acc=torch.sum(diagonal)/total_sum
    bacc=torch.sum(diagonal/true_classes)/n_classes
    avg_iota=diagonal/(col_sum+true_classes-diagonal)
    avg_iota=torch.sum(avg_iota)/n_classes
    
    diagonal_p=torch.diagonal(prob_confusion_matrix) #(n_classes)
    true_classes_p=torch.sum(prob_confusion_matrix,dim=1) #(n_classes)
    col_sum_p=torch.sum(prob_confusion_matrix,dim=0) #(n_classes)
    
    avg_iota_p=diagonal_p/(col_sum_p+true_classes_p-diagonal_p)
    avg_iota_p=torch.sum(avg_iota_p)/n_classes
    
  return {"accuracy":acc, "balanced_accuracy":bacc, "avg_iota":avg_iota, "s_avg_iota":avg_iota_p}

def add_metrics(metrics,storage,cblock,epoch):
  if cblock=="train":
    idx=0
  elif cblock=="val":
    idx=1
  elif cblock=="test":
    idx=2
  for key in metrics.keys():
    storage[key][idx,epoch]=metrics[key]

#=============================================================

def calc_lr_rate_loss(model,device,current_dtype,optimizer,loss_fct,dataloader,n_classes=None,Ns=None,Nq=None,start_mode=True):
    loss_complete=0
    model.train()

    if isinstance(model,TEClassifierSequential) or isinstance(model,TEClassifierParallel) or isinstance(model,TEClassifierReferencePoint):
      for batch in dataloader:
        inputs=batch["input"]
        labels=batch["labels"]
        inputs = inputs.to(device,dtype=current_dtype)
        labels=labels.to(device,dtype=current_dtype)
        if "sample_weights" in batch.keys():
          sample_weights=batch["sample_weights"]
          sample_weights=torch.reshape(input=sample_weights,shape=(sample_weights.size(dim=0),1))
          sample_weights=sample_weights.to(device,dtype=current_dtype)
        else:
           sample_weights=torch.ones((inputs.size(0)),device=device,dtype=current_dtype)/inputs.size(0)
        if not start_mode:
          optimizer.zero_grad()
        outputs=model(inputs,prediction_mode=False)
        loss=loss_fct(outputs,labels)*sample_weights.detach()
        loss=loss.mean()
        if not start_mode:
          loss.backward()
          optimizer.step()
        loss_complete+=loss
    elif isinstance(model,TEClassifierPrototype):
      for batch in dataloader:
        inputs=batch["input"]
        labels=batch["labels"]
        sample_inputs=inputs[0:(n_classes*Ns)].clone()
        query_inputs=inputs[(n_classes*Ns):(n_classes*(Ns+Nq))].clone()
        sample_classes=labels[0:(n_classes*Ns)].clone()
        query_classes=labels[(n_classes*Ns):(n_classes*(Ns+Nq))].clone()
        sample_inputs = sample_inputs.to(device,dtype=current_dtype)
        query_inputs = query_inputs.to(device,dtype=current_dtype)
        sample_classes = sample_classes.to(device,dtype=current_dtype)
        query_classes = query_classes.to(device,dtype=current_dtype)
        if not start_mode:
          optimizer.zero_grad()
        outputs=model(
          input_q=query_inputs,
          classes_q=query_classes,
          input_s=sample_inputs,
          classes_s=sample_classes,
          prediction_mode=False
        )
        loss=loss_fct(
          classes_q=outputs[2],
          distance_matrix=outputs[1],
          metric_scale_factor=model.get_metric_scale_factor().detach(),
          logits=outputs[0]
        )
        if not start_mode:
          loss.backward()
          optimizer.step()      
        loss_complete+=loss
    else:
      for batch in dataloader:
        inputs=batch["input"]
        labels=batch["labels"]
        inputs = inputs.to(device,dtype=current_dtype)
        labels=labels.to(device,dtype=current_dtype)
        if not start_mode:
          optimizer.zero_grad()
        outputs=model(inputs,encoder_mode=False)
        loss=loss_fct(outputs,labels)
        loss=loss.mean()
        if not start_mode:
          loss.backward()
          optimizer.step()
        loss_complete+=loss
    return loss_complete

def calc_lr_rate(trace,model,epochs,filepath,optimizer_method,loss_fct_name,dataset,batch_size,class_weights,Ns=None,Nq=None,n_classes=None,separate=None,shuffle=None,alpha=None,margin=None):
  #Prepare objects
  device=get_device()
  current_dtype=get_dtype(device)
  model.to(device=device,dtype=current_dtype)
  
  if isinstance(model,TEClassifierPrototype):
    loss_fct=get_loss_cls_pt_fct(
      name=loss_fct_name,
      alpha=alpha,
      margin=margin
    )
  elif isinstance(model,TEClassifierSequential) or isinstance(model,TEClassifierParallel) or isinstance(model,TEClassifierReferencePoint):
    loss_fct=get_loss_cls_fct(name=loss_fct_name,class_weights=class_weights)
  elif isinstance(model,LSTMAutoencoder_with_Mask_PT) or isinstance(model,DenseAutoencoder_with_Mask_PT):
    loss_fct=torch.nn.MSELoss()
  
  loss_fct.to(device=device,dtype=current_dtype)  
  
  if isinstance(model,TEClassifierPrototype):
    ProtoNetSampler_Train=MetaLernerBatchSampler(
    targets=dataset["labels"][range(0,len(dataset))],
    Ns=Ns,
    Nq=Nq,
    separate=separate,
    shuffle=shuffle)
    dataloader=torch.utils.data.DataLoader(
      dataset,
      pin_memory = True if device=="cuda" else False,
      batch_sampler=ProtoNetSampler_Train
    )
  else:
    dataloader=torch.utils.data.DataLoader(
      dataset,
      batch_size=batch_size,
      pin_memory=True if device=="cuda" else False,
      shuffle=True
    )
  #Save model weights
  torch.save(model.state_dict(),filepath)
  
  counter=0
  learning_rates=np.zeros((30))
  for i in range(1,6):
    if i==0:
      tmp_range=range(0,3)
    else:
      tmp_range=range(0,4)
    for j in tmp_range:
      base=(j+1)/4
      learning_rates[counter]=base/(10**i)
      counter+=1

  results=np.zeros((4,30))

  #Set up logger
  PrgInd=ProgressLogger()
  PrgInd.set_start_time()
  total_iter=len(learning_rates)
  for j in range(0,total_iter):
    #Reset model
    model.load_state_dict(torch.load(filepath,weights_only=False))
    #set learning rate
    tmp_lr_rate=learning_rates[j]
    #Create a new Optimizer for every test
    optimizer=get_Optimizer(
      optimizer_method,
      params=model.parameters(),
      lr_rate=tmp_lr_rate
    )
    # Calculate start loss
    start_loss=calc_lr_rate_loss(
      device=device,
      current_dtype=current_dtype,
      Ns=Ns,
      Nq=Nq,
      n_classes=n_classes,
      model=model,
      optimizer=optimizer,
      loss_fct=loss_fct,
      dataloader=dataloader,
      start_mode=True
    )
    start_loss=start_loss/len(dataloader) 
    # Calculate tranining data
    epoch_loss_m=start_loss
    for i in range(0,epochs):
      epoch_loss=calc_lr_rate_loss(
        device=device,
        current_dtype=current_dtype,
        Ns=Ns,
        Nq=Nq,
        n_classes=n_classes,
        model=model,
        optimizer=optimizer,
        loss_fct=loss_fct,
        dataloader=dataloader,
        start_mode=False
      )
      epoch_loss=epoch_loss/len(dataloader)
      #Count improvments
      if(epoch_loss<=epoch_loss_m):
        results[1,j]+=1
      #Set current loss to as the other loss  
      epoch_loss_m=epoch_loss
    #Update logger  
    PrgInd.print_progress(trace=trace,epoch=j,epochs=total_iter)
    #Add final data
    results[0,j]=tmp_lr_rate
    results[2,j]=start_loss.detach()
    results[3,j]=epoch_loss.detach()
  return results

#=============================================================
def check_and_set_checkpoints_cls(use_callback,model,filepath,epoch,metric_storage,best_val_avg_iota,best_val_loss,best_acc,best_bacc,acc_val,bacc_val,avg_iota_val,val_loss,elc):
  if use_callback==True:
      if (avg_iota_val>best_val_avg_iota) or (avg_iota_val==best_val_avg_iota and acc_val>best_acc) or (avg_iota_val==best_val_avg_iota and acc_val==best_acc and val_loss<best_val_loss):
        if isinstance(model, torch._dynamo.eval_frame.OptimizedModule):
          print("model is compiled")
          torch.save(model._orig_mod.state_dict(),filepath)
        else:
          torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_val_avg_iota=avg_iota_val
        best_acc=acc_val
        best_val_loss=val_loss
        metric_storage["checkpoints"][epoch]=1
        elc=epoch+1
  return best_val_loss, best_acc,best_bacc,best_val_avg_iota,elc  

class ModelWithLoss(torch.nn.Module):
  def __init__(self,model,loss_fct,model_type):
      super().__init__()
      self.model=model
      self.loss_fct=loss_fct
      self.model_type=  model_type
      if self.model_type=="ClassifierStandard":
        self.forward=self.train_and_eval_standard
      elif self.model_type=="ClassifierPrototype":
        self.forward=self.train_and_eval_prototype
      elif self.model_type=="TEFeatureExtractor":
        self.forward=self.train_and_eval_feature_extractor

  def train_and_eval_standard(self,static_input,static_target,static_sample_weights=None):
    output=self.model(static_input,prediction_mode=False)
    loss=(self.loss_fct(output,static_target)*static_sample_weights.detach()).mean()
    return loss,output
  
  def train_and_eval_prototype(self,static_query_inputs,static_query_classes,static_sample_inputs,static_sample_classes,static_class_labels):
    outputs=self.model(
        input_q=static_query_inputs,
        classes_q=static_query_classes,
        input_s=static_sample_inputs,
        classes_s=static_sample_classes,
        class_labels=static_class_labels,
        prediction_mode=False
    )
    loss=self.loss_fct(
        classes_q=outputs[2],
        distance_matrix=outputs[1],
        metric_scale_factor=self.model.get_metric_scale_factor().detach(),
        logits=outputs[0]
    )    
    return loss, outputs
  def train_and_eval_feature_extractor(self,static_input,static_target):
    output=self.model(static_input,encoder_mode=False)
    loss=self.loss_fct(output,static_target).mean()
    return loss, output


def prepare_model(model):
  device=get_device()
  current_dtype=get_dtype(device)
  model.to(device=device,dtype=current_dtype)
  return model, device, current_dtype

def prepare_loss_function(loss_cls_fct_name,class_weights,device,current_dtype,type="prob_classification"):
  class_weights=class_weights.clone()
  class_weights=class_weights.to(device)
  loss_fct=get_loss_cls_fct(name=loss_cls_fct_name,class_weights=class_weights)
  loss_fct.to(device=device,dtype=current_dtype)
  return loss_fct
  

def calc_trained_prototypes_batch(n_classes,model,data_loader,device,dtype):
    model.eval()
    
    running_class_values=torch.zeros((n_classes,model.get_embedding_dim())).to(device)
    running_class_freq=torch.zeros(n_classes).to(device)
    
    for batch in data_loader:
      #assign colums of the batch
      inputs=batch["input"]
      labels=batch["labels"]
      
      inputs = inputs.to(device,dtype=dtype)
      labels=labels.to(device,dtype=dtype)
      labels_one_hot=torch.nn.functional.one_hot(labels.to(dtype=torch.long),num_classes=n_classes)

      embeddings=model.embed(inputs).to(device)

      running_class_values=running_class_values+torch.matmul(
        torch.transpose(labels_one_hot.to(dtype=embeddings.dtype),dim0=1,dim1=0),
        embeddings
      )
      running_class_freq=running_class_freq+torch.sum(labels_one_hot,dim=0)
      
    running_class_freq=torch.unsqueeze(running_class_freq,-1)
    running_class_freq=running_class_freq.repeat((1,model.get_embedding_dim()))
    
    class_mean_prototypes=running_class_values/running_class_freq
    
    class_labels=torch.arange(start=0, end=n_classes, step=1)
    return class_mean_prototypes, class_labels




if __name__ == "__main__":
  def do_training_ddp(rank,world_size,model_type,ddp_use,train_args,backend_ddp,tmp_dir,aife_dir):
    print(rank)
    #Load all py objects
    session_globals = globals()
    # Liste, um alle erfolgreich geladenen Module und Objekte zwischenzuspeichern
    geladene_module = []
    alle_objekte = {}

    # 1. SCHRITT: Alle Module normal laden und registrieren
    for file_name in os.listdir(aife_dir):
        if file_name.endswith(".py") and file_name != "__init__.py":
            modul_name = file_name[:-3]
            datei_pfad = os.path.join(aife_dir, file_name)
            try:
                spec = importlib.util.spec_from_file_location(
                    modul_name, datei_pfad
                )
                if spec and spec.loader:
                    modul = importlib.util.module_from_spec(spec)
                    sys.modules[modul_name] = modul
                    spec.loader.exec_module(modul)
                    geladene_module.append(modul)

                    # Alle gültigen Klassen und Funktionen dieses Moduls sammeln
                    for attr_name in dir(modul):
                        if not attr_name.startswith("__"):
                            objekt = getattr(modul, attr_name)

                            if (
                                inspect.isclass(objekt)
                                or inspect.isfunction(objekt)
                            ) and hasattr(objekt, "__name__"):
                                object_name = objekt.__name__.split(".")[
                                    -1
                                ]

                                # Modul-Zugehörigkeit für PyTorch/DDP säubern
                                #if inspect.isclass(objekt):
                                objekt.__module__ = "__main__"

                                alle_objekte[object_name] = objekt
            except Exception as e:
                print(
                    f"Error loading {file_name}: {e}", file=sys.stderr
                )

    # 2. SCHRITT: WICHTIG – Gegenseitige Kreuz-Injektion (Cross-Injection)
    # Wir schreiben jedes gefundene Objekt in den globalen Namensraum der aktuellen Session...
    for name, obj in alle_objekte.items():
        session_globals[name] = obj

        # ...UND in den Namensraum JEDES importierten Moduls aus dem Ordner!
        for modul in geladene_module:
            modul.__dict__[name] = obj
              
    # load model
    print(masking_layer)
    with open(tmp_dir+"/nn_configs.json", "r", encoding="utf-8") as file:
      model_config = json.load(file)
    model_definition= globals()[model_config["class_name"]]
    model_config.pop("class_name", None) 
    model=model_definition(**model_config)
    model.load_state_dict(torch.load(tmp_dir+"/nn_weights.pt", weights_only=True))
    train_args["model"]=model
    #Load datasets
    train_data=datasets.load_from_disk(tmp_dir+"train_data")
    val_data=datasets.load_from_disk(tmp_dir+"val_data")
    train_args["train_data"]=train_data
    train_args["val_data"]=val_data
    if os.path.isdir(tmp_dir+"test_data"):
       test_data=datasets.load_from_disk(tmp_dir+"test_data")
       train_args["test_data"]=test_data
    else:
      train_args["test_data"]=None
    if "class_weights" in train_args:
      train_args["class_weights"]=torch.from_numpy(np.array(train_args["class_weights"]))
    #Create trainer
    print(train_args)
    trainer=ModelTrainer(model_type,ddp_use)
    if model_type=="ClassifierStandard":
      trainer.config_for_StandardClassifier(**train_args)
    elif model_type=="ClassifierPrototype":
      trainer.config_for_ClassifierPrototype(**train_args)
    elif model_type=="TEFeatureExtractor":
      trainer.config_for_TEFeatureExtractor(**train_args)
    trainer.set_rank_word_size(rank=rank,world_size=world_size)
    #Set up ddp
    print(trainer)
    os.environ["MASTER_ADDR"] = "localhost"
    os.environ["MASTER_PORT"] = "12355" # Freier Port auf dem System
    if dist.is_initialized():
       dist.destroy_process_group()
    dist.init_process_group(
        backend=backend_ddp,
        init_method="env://",
        world_size=world_size,
        rank=rank
    )
    print("test")
    trainer.do_training()
