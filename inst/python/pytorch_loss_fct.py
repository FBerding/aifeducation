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

def create_ordinal_weights(targets):
  with torch.no_grad():
    n_classes=targets.size(1)
    class_idx=torch.argmax(targets,dim=1)+1
    class_idx=torch.unsqueeze(class_idx,dim=1)
    class_idx=class_idx.expand((class_idx.size(0),n_classes))

    index_matrix=torch.arange(start=1,end=n_classes+1,step=1,dtype=class_idx.dtype, device=class_idx.device)
    index_matrix=torch.unsqueeze(index_matrix,dim=0)
    index_matrix=index_matrix.expand((class_idx.size(0),class_idx.size(1)))
    
    weights=torch.abs(index_matrix-class_idx)+1
    n_factors=torch.sum(weights,dim=1,keepdim=True)
    n_factors=n_factors.expand(weights.size())
    weights=weights/n_factors
    return weights
    

class focal_loss(torch.nn.Module):
  def __init__(self,class_weights,gamma,scale_level="nominal"):
    super().__init__()
    self.class_weights=class_weights
    self.gamma=gamma
    self.scale_level=scale_level
    
    self.cross_entropy=torch.nn.CrossEntropyLoss(
      reduction="none",
      weight = self.class_weights
    )
    self.softmax=torch.nn.Softmax(dim=1)
  
  def forward(self,prediction,target):
    if self.scale_level=="ordinal":
      prediction=(1/create_ordinal_weights(target))*prediction
    #Shape (Batch)
    ce=self.cross_entropy(prediction,target)
    #Shape (Batch, n_classes)
    with torch.no_grad():
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
  
  def forward(self,classes_q,distance_matrix,metric_scale_factor,logits=None):
    #Total number of classes
    K=distance_matrix.size()[1]
    current_margin=metric_scale_factor*self.margin
    
    #Indikators for the classes as one hot for computing the values
    c_indikator=torch.nn.functional.one_hot(torch.Tensor.to(classes_q,dtype=torch.int64),num_classes=K)
    
    l_pull=torch.sum(c_indikator.detach()*torch.pow(input=distance_matrix,exponent=2),dim=1)
    l_pull=torch.sum(l_pull,dim=0)
    l_pull=self.alpha*l_pull
    
    margin_distance=current_margin-distance_matrix
    margin_accomplished=(margin_distance<0)
    margin_distance=(~margin_accomplished)*margin_distance
    
    l_push=torch.sum((1-c_indikator).detach()*torch.pow(input=margin_distance,exponent=2),dim=1)
    l_push=torch.sum(l_push,dim=0)
    l_push=(1-self.alpha)*l_push
    loss=(l_pull+l_push)/K
    return loss

class multi_way_contrastive_loss_fc(torch.nn.Module):
  def __init__(self,alpha=0.2,margin=0.5,class_weights=None,gamma=2,scale_level="nominal"):
    super().__init__()
    self.alpha=alpha
    self.margin=margin
    self.class_weights=class_weights
    self.gamma=gamma
    
    self.mw_contrastive_loss=multi_way_contrastive_loss(alpha=self.alpha,margin=self.margin)
    self.focal_loss=focal_loss(class_weights=self.class_weights,gamma=self.gamma,scale_level=scale_level)
  
  def forward(self,classes_q,distance_matrix,metric_scale_factor,logits):
    loss_mw=self.mw_contrastive_loss(
      classes_q=classes_q,
      distance_matrix=distance_matrix,
      metric_scale_factor=metric_scale_factor
    )

    target_focal=torch.nn.functional.one_hot(classes_q.long(), num_classes=distance_matrix.size(1))
    loss_fc=self.focal_loss(
      prediction=logits,
      target=target_focal.float()
    ).mean()
    loss=(loss_mw+loss_fc)/2
    return loss
    
class focal_loss_pt(torch.nn.Module):
  def __init__(self,class_weights=None,gamma=2,scale_level="nomial"):
    super().__init__()
    self.class_weights=class_weights
    self.gamma=gamma
    
    self.focal_loss=focal_loss(class_weights=self.class_weights,gamma=self.gamma,scale_level=scale_level)
  
  def forward(self,classes_q,distance_matrix,metric_scale_factor,logits):
    target_focal=torch.nn.functional.one_hot(classes_q.long(), num_classes=distance_matrix.size(1))
    loss=self.focal_loss(
      prediction=logits,
      target=target_focal.float()
    ).mean()
    return loss

class aem_loss(torch.nn.Module):
  def __init__(self,eps=1e-6):
    super().__init__()
    self.activation= torch.nn.Softmax(dim=1)
    self.eps=eps
    
  def forward(self,prediction,target):
    n_classes=target.size(1)
    #Confusion Matrix
    target=torch.unsqueeze(target,dim=2) # B, T, 1
    target=target.expand((target.size(0),target.size(1),target.size(1))) #B,T,A

    prob=self.activation(prediction)
    prob_exp=torch.unsqueeze(prob,dim=1) # B,1,A
    prob_exp=prob_exp.expand(target.size()) #B,T,A
    
    prob_confusion_matrix=torch.sum(target*prob_exp,dim=0) # T, A
    eps_matrix=torch.zeros(prob_confusion_matrix.size(), dtype=prob_confusion_matrix.dtype,device=prob_confusion_matrix.device)
    eps_matrix.fill_diagonal_(self.eps)
    prob_confusion_matrix=prob_confusion_matrix+eps_matrix
    #Metric
    diagonal_p=torch.diagonal(prob_confusion_matrix) #(n_classes)
    true_classes_p=torch.sum(prob_confusion_matrix,dim=1) #(n_classes)
    col_sum_p=torch.sum(prob_confusion_matrix,dim=0) #(n_classes)
    
    avg_iota_p=diagonal_p/(col_sum_p+true_classes_p-diagonal_p)
    avg_iota_p=torch.sum(avg_iota_p)/n_classes
    avg_iota_p=1-avg_iota_p
    
    avg_iota_p=torch.unsqueeze(avg_iota_p,dim=0)
    avg_iota_p=avg_iota_p.expand(target.size(0),avg_iota_p.size(0))
    avg_iota_p=avg_iota_p/target.size(0)
    return avg_iota_p

class aem_loss_pt(torch.nn.Module):
  def __init__(self,eps=1e-6):
    super().__init__()
    self.aem_loss=aem_loss(eps=eps)
  
  def forward(self,classes_q,distance_matrix,metric_scale_factor,logits):
    targets=torch.nn.functional.one_hot(classes_q.long(), num_classes=distance_matrix.size(1))
    loss=self.aem_loss(
      prediction=logits,
      target=targets.float()
    ).mean()
    return loss    
