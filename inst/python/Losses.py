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
    n_classes = targets.size(1)
    class_idx = torch.argmax(targets, dim=1).detach()
    index_matrix = torch.arange(n_classes, dtype=class_idx.dtype, device=class_idx.device)
    weights = torch.abs(index_matrix.unsqueeze(0) - class_idx.unsqueeze(1))
    n_factors = torch.sum(weights, dim=1, keepdim=True)
    weights = weights / n_factors
    return weights.detach()
    

class FocalLoss(torch.nn.Module):
    def __init__(self, class_weights, gamma, scale_level="nominal"):
        super().__init__()
        self.class_weights = class_weights
        self.gamma = gamma
        self.scale_level = scale_level
        self.is_ordinal = (scale_level == "ordinal")
        self.cross_entropy = torch.nn.CrossEntropyLoss(
            reduction="none",
            weight=self.class_weights
        )
        self.softmax = torch.nn.Softmax(dim=1)
  
    def forward(self, prediction, target):
        prob = self.softmax(prediction)
        focal_factor = (1.0 - prob) ** self.gamma
        focal_factor = torch.sum(target * focal_factor, dim=1)
        if self.is_ordinal:
            ordinal_weights = create_ordinal_weights(target)
            penality = torch.sum(prob * ordinal_weights, dim=1)
        else:
            penality = 0.0
        ce = self.cross_entropy(prediction, target)
        focal = focal_factor * ce + penality
        return focal
    
class multi_way_contrastive_loss(torch.nn.Module):
    def __init__(self, alpha=0.2, margin=0.5):
        super().__init__()
        self.alpha = alpha
        self.margin = margin
  
    def forward(self, classes_q, distance_matrix, metric_scale_factor, logits=None):
        K = distance_matrix.shape[1]
        current_margin = metric_scale_factor * self.margin

        c_indikator = torch.nn.functional.one_hot(classes_q.to(torch.int64), num_classes=K)
        
        l_pull = torch.sum(c_indikator * torch.square(distance_matrix), dim=1)
        l_pull = torch.sum(l_pull, dim=0)
        l_pull = self.alpha * l_pull
        
        margin_distance = torch.clamp(current_margin - distance_matrix, min=0.0)
        
        l_push = torch.sum((1 - c_indikator) * torch.square(margin_distance), dim=1)
        l_push = torch.sum(l_push, dim=0)
        l_push = (1.0 - self.alpha) * l_push
        
        loss = (l_pull + l_push) / K
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
    def __init__(self, eps=1e-6):
        super().__init__()
        self.activation = torch.nn.Softmax(dim=1)
        self.eps = eps
    
    def forward(self, prediction, target):
        n_classes = target.shape[1]
        batch_size = target.shape[0]

        prob = self.activation(prediction) # (B, n_classes)
        
        prob_confusion_matrix = torch.sum(
            torch.bmm(target.unsqueeze(2), prob.unsqueeze(1)), 
            dim=0
        )
        
        eps_matrix = torch.eye(n_classes, dtype=prob_confusion_matrix.dtype, device=prob_confusion_matrix.device) * self.eps
        prob_confusion_matrix = prob_confusion_matrix + eps_matrix
        
        diagonal_p = torch.diag(prob_confusion_matrix) 
        true_classes_p = torch.sum(prob_confusion_matrix, dim=1) 
        col_sum_p = torch.sum(prob_confusion_matrix, dim=0) 
        
        avg_iota_p = diagonal_p / (col_sum_p + true_classes_p - diagonal_p)
        avg_iota_p = torch.sum(avg_iota_p) / n_classes
        loss_scalar = 1.0 - avg_iota_p
        
        loss_val = loss_scalar / batch_size
        avg_iota_p = torch.unsqueeze(loss_val,dim=0).expand((batch_size))
        
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

class feature_extractor_loss(torch.nn.Module):
  def __init__(self):
    super().__init__()
    self.mse_loss=torch.nn.MSELoss()
    self.cov_loss=calc_Correlation
  def forward(self,input,target,latent_space):
    input_n=torch.nn.functional.normalize(input, p=2.0, dim=2, eps=1e-12, out=None)
    target_n=torch.nn.functional.normalize(target, p=2.0, dim=2, eps=1e-12, out=None)
    loss=torch.sqrt(self.mse_loss(input_n,target_n)).mean()+self.cov_loss(latent_space)
    return(loss)

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
