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

from .ModelTrainer import ModelTrainer

class ModelTrainerManager():
  def __init__(self, model_type,ddp_use,train_args,tmp_dir,aife_dir):
    self.model_type=model_type
    self.tmp_dir=tmp_dir
    self.aife_dir=aife_dir
    self.ddp_use=ddp_use
    self.train_args=train_args
    self.backend_ddp="nccl"
    self.world_size=torch.cuda.device_count()

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
  
  def calc_lr_rate(self,epochs):
      #Disable ddp
      self.ddp_use
      #Init Trainer
      trainer=self.init_trainer(self.model_type,self.ddp_use,self.train_args)
      #Start Estimation
      estimates=trainer.calc_lr_rate(epochs)
      return estimates
  
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
    
