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

class focal_loss(torch.nn.Module):
  def __init__(self,class_weights,gamma):
    super().__init__()
    self.class_weights=class_weights
    self.gamma=gamma
    
    self.cross_entropy=torch.nn.CrossEntropyLoss(
      reduction="none",
      weight = self.class_weights
    )
    self.softmax=torch.nn.Softmax(dim=1)
  
  def forward(self,prediction,target):
    #Shape (Batch)
    ce=self.cross_entropy(prediction,target)
    
    #Shape (Batch, n_classes)
    prob=self.softmax(prediction)
    #shape (Batch, n_classes)
    focal_factor=torch.pow(input=(1-prob),exponent=self.gamma)
    #shape(Batch)
    focal_factor=torch.sum(target*focal_factor,dim=1)
    #Shape (Batch)
    focal=focal_factor*ce
    return focal
    
class multi_way_contrastive_loss(torch.nn.Module):
  def __init__(self,alpha=0.2,margin=0.5):
    super().__init__()
    self.alpha=alpha
    self.margin=margin
  
  def forward(self,classes_q,distance_matrix,metric_scale_factor):
    #Total number of classes
    K=distance_matrix.size()[1]
    current_margin=metric_scale_factor*self.margin
    
    #Indikators for the classes as one hot for computing the values
    c_indikator=torch.nn.functional.one_hot(torch.Tensor.to(classes_q,dtype=torch.int64),num_classes=K)
    
    l_pull=torch.sum(c_indikator*torch.pow(input=distance_matrix,exponent=2),dim=1)
    l_pull=torch.sum(l_pull,dim=0)
    l_pull=self.alpha*l_pull
    
    margin_distance=current_margin-distance_matrix
    margin_accomplished=(margin_distance<0)
    margin_distance=(~margin_accomplished)*margin_distance
    
    l_push=torch.sum((1-c_indikator)*torch.pow(input=margin_distance,exponent=2),dim=1)
    l_push=torch.sum(l_push,dim=0)
    l_push=(1-self.alpha)*l_push
    loss=(l_pull+l_push)/K
    return loss
