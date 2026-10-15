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

from .DistanceFunctions import CosineDistance

def create_ordinal_weights(targets):
    n_classes = targets.size(1)
    class_idx = torch.argmax(targets, dim=1).detach()
    index_matrix = torch.arange(n_classes, dtype=class_idx.dtype, device=class_idx.device)
    weights = torch.abs(index_matrix.unsqueeze(0) - class_idx.unsqueeze(1))
    weights=weights*weights
    #n_factors = torch.sum(weights, dim=1, keepdim=True)
    #weights = weights / n_factors
    return weights.detach()
    

class focal_loss(torch.nn.Module):
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


class NormalizedMSELoss(torch.nn.Module):
    def __init__(self, eps=1e-8):
        super().__init__()
        self.eps = eps

    def forward(self, predictions, targets):
        # inputs: (B, T, F) - Shapes müssen bei CUDA Graphs statisch sein
        B, T, F = targets.size()
        
        # 1. Maskierungslogik (Rein mathematisch auf der GPU)
        max_abs_per_token = torch.max(torch.abs(targets), dim=-1, keepdim=True).values  # Shape: (B, T, 1)
        mask_expanded = torch.where(max_abs_per_token > 1e-5, 1.0, 0.0).detach()  # Shape: (B, T, 1)
        
        # 2. NEU: Feature-Varianz NUR über valide (nicht-gepaddete) Tokens berechnen
        # Summe der Features über Batch und Zeit (Shape: F)
        targets_masked = targets * mask_expanded
        sum_targets = torch.sum(targets_masked, dim=(0, 1))  # Shape: (F,)
        
        # Valide Anzahl an Tokens für den Teiler ermitteln
        num_valid_tokens = torch.sum(mask_expanded)
        divisor_stat = torch.clamp(num_valid_tokens, min=self.eps)
        
        # Mittelwert pro Feature (Shape: 1, 1, F)
        target_mean = (sum_targets / divisor_stat).view(1, 1, F)
        
        # Varianz pro Feature berechnen: E[(X - E[X])^2] unter Berücksichtigung der Maske
        diff_squared = ((targets - target_mean) * mask_expanded) ** 2
        target_variance = torch.sum(diff_squared, dim=(0, 1)) / divisor_stat  # Shape: (F,)
        
        # Reshape für Broadcasting (Shape: 1, 1, F)
        target_variance = target_variance.view(1, 1, F)
        
        # 3. Fehler (Zähler für MSE)
        error = torch.abs(predictions - targets)
        
        # 4. Fehler durch die feature-spezifische Varianz teilen
        nmse = error / (target_variance.detach() + self.eps)
        
        # 5. Maske anwenden (Gepaddete Elemente auf 0.0 setzen)
        nmse_masked = nmse * mask_expanded
        
        # 6. Mean-Reduction ohne CPU-GPU-Synchronisation (CUDA-Graph safe)
        num_valid_elements = num_valid_tokens * F
        divisor_loss = torch.clamp(num_valid_elements, min=self.eps)
        
        # Finaler mittlerer Loss
        return torch.sum(nmse_masked) / divisor_loss




def structure_loss(latent_space, target):
    # Entpacken mit Unterstützung für unterschiedliche Feature-Dimensionen
    B, T, F_latent = latent_space.shape
    _, _, F_target = target.shape
    
    # 1. Zusammenfassen von Batch- und Zeitdimension zu (B * T, F)
    N = B * T
    latent_flat = latent_space.reshape(N, F_latent)
    target_flat = target.reshape(N, F_target)
    
    # 2. Globale Token-Maske (CUDA-Graph-safe mit .any(dim=-1))
    token_mask = (target_flat != 0).any(dim=-1)
    
    # Paarweise Maske über alle N x N Kombinationen
    pairwise_mask = token_mask.unsqueeze(-1) & token_mask.unsqueeze(-2)
    
    # Obere Dreiecksmatrix
    triu_mask = torch.triu(torch.ones(N, N, dtype=torch.bool, device=target.device), diagonal=1)
    
    # Finale Maske kombiniert gültige Vektoren mit der Dreiecksmaske
    final_mask = pairwise_mask & triu_mask
    
    # 3. Globale paarweise Distanzen berechnen
    cos_distance_latent = CosineDistance(latent_flat, latent_flat)
    cos_distance_target = CosineDistance(target_flat, target_flat).detach()
    
    # Differenz berechnen
    diff = torch.abs(cos_distance_latent - cos_distance_target)/2
    
    
    # Maskierung auf die Differenzen anwenden
    float_mask = final_mask.to(diff.dtype)
    masked_diff = diff * float_mask
    
    # 4. Globalen Mittelwert bilden (CUDA-Graph-safe ohne if-Bedingung)
    loss_sum = torch.sum(masked_diff)
    valid_elements = torch.sum(float_mask)
    
    # torch.clamp verhindert Division durch 0, falls kein Element gültig ist.
    # Da loss_sum in dem Fall auch 0 ist, wird mathematisch korrekt 0.0 zurückgegeben.
    loss = loss_sum / torch.clamp(valid_elements, min=1.0).detach()
    
    return loss

class MaskedFeatureDeCorrelationLoss(torch.nn.Module):
    def __init__(self, pad_value=0.0, eps=1e-8):
        super(MaskedFeatureDeCorrelationLoss, self).__init__()
        self.pad_value = pad_value
        self.eps = eps

    def forward(self, x):
        # x-Shape erwartet: [B, T, F]
        b, t, f = x.size()

        # 1. Maske erstellen (Statische Shapes für CUDA-Graph-Kompatibilität)
        # True (1.0) für valide Schritte, False (0.0) für gepaddete Schritte
        is_pad = (x == self.pad_value).all(dim=-1, keepdim=True)  # [B, T, 1]
        mask = (~is_pad).float()  # [B, T, 1]

        # Anzahl valider Zeitschritte über alle Batches hinweg
        num_valid_elements = mask.sum()  # Skalar (Anzahl der validen B,T Kombinationen)

        # 2. Maskierte Features zentrieren (Mittelwert abziehen)
        x_masked = x * mask
        # Berechne den Mittelwert pro Feature über die Dimensionen B und T
        mean = x_masked.sum(dim=(0, 1), keepdim=True) / (num_valid_elements + self.eps)  # [1, 1, F]
        
        # Zentrieren und Padding-Elemente wieder auf 0 setzen
        x_centered = (x - mean) * mask  # [B, T, F]

        # 3. Features flachklopfen zu [N_valide, F], ohne dynamische Shapes zu nutzen
        # Wir berechnen die Kovarianzmatrix direkt über die Summe der äußeren Produkte (Matrixmultiplikation)
        # Umformen von [B, T, F] zu [B*T, F] für effiziente Matrixalgebra
        x_flat = x_centered.view(b * t, f)

        # Kreuzprodukt-Matrix berechnen: [F, B*T] x [B*T, F] -> [F, F]
        # Das entspricht der unskalierten Kovarianzmatrix über alle validen Zeitschritte
        covariance_matrix = torch.matmul(x_flat.t(), x_flat) / (num_valid_elements + self.eps)

        # 4. Standardabweichung für die Normalisierung (Kovarianz -> Korrelation)
        # Extrahiere die Diagonale (Varianz pro Feature)
        variance = torch.diagonal(covariance_matrix)  # [F]
        std = torch.sqrt(variance + self.eps)  # [F]

        # Erzeuge eine Normalisierungsmatrix über das äußere Produkt der Standardabweichungen
        std_matrix = torch.outer(std, std)  # [F, F]

        # Korrelationsmatrix berechnen (Werte liegen zwischen -1 und 1)
        correlation_matrix = covariance_matrix / (std_matrix + self.eps)

        # 5. Loss berechnen: Reduziere die Off-Diagonal-Elemente gegen 0
        # Wir quadrieren die Matrixelemente, um sowohl positive als auch negative Korrelationen zu bestrafen
        loss_matrix = correlation_matrix ** 2

        # Erzeuge eine Identitätsmatrix, um die Diagonale (Korrelation eines Features mit sich selbst = 1) zu ignorieren
        identity = torch.eye(f, device=x.device, dtype=x.dtype)
        
        # Maskiere die Diagonale aus, sodass nur die Kreuzkorrelationen übrig bleiben
        off_diagonal_loss = loss_matrix * (1.0 - identity)

        # Der finale Loss ist der Mittelwert aller Off-Diagonal-Fehler
        # f * (f - 1) ist die exakte Anzahl der Nicht-Diagonalelemente
        total_loss = off_diagonal_loss.sum() / torch.clamp(torch.tensor(f * (f - 1), device=x.device, dtype=x.dtype), min=1.0)

        return total_loss

#Kullback-Leibler-Divergenz
def KLDLoss(mu, logvar,mask):
    """
    Berechnet die KL-Divergenz zwischen dem gelernten latenten Raum 
    und einer Standardnormalverteilung N(0, 1).
    
    Argumente:
    mu     -- Mittelwert-Vektor aus dem Encoder (Batch_Size, Latent_Dim)
    logvar -- Log-Varianz-Vektor aus dem Encoder (Batch_Size, Latent_Dim)
    """
    # Formel: -0.5 * sum(1 + log(sigma^2) - mu^2 - sigma^2)
    valid_mask=~mask
    n_elements=torch.sum(valid_mask)*mu.size(2)
    kld_per_element=torch.clamp((-0.5*(1 + logvar - mu.pow(2) - logvar.exp())*valid_mask),min=0.0)
    kld=torch.sum(kld_per_element)/n_elements
    return kld


class calc_IsoScore(torch.nn.Module):
    def __init__(self, eps: float = 1e-8):
        super().__init__()
        self.eps = eps

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        # Da x (B, T, F) statisch ist, sind auch diese Werte zur Compile-Zeit konstant
        features = x.shape[-1]
        n_fixed = float(features)
        
        # 1. x auffalten zu (B*T, F)
        x_flat = x.reshape(-1, features)
        
        # 2. Maskierung (rein mathematisch, keine dynamic shapes)
        valid_mask = (torch.sum(torch.abs(x_flat), dim=1, keepdim=True) != 0).to(x_flat.dtype)
        x_selected = x_flat * valid_mask
        
        m_valid = torch.sum(valid_mask)
        m_minus_1 = torch.clamp(m_valid - 1.0, min=self.eps)
        
        x_sum = torch.sum(x_selected, dim=0, keepdim=True)
        x_mean = x_sum / torch.clamp(m_valid, min=self.eps)
        x_centered = (x_selected - x_mean) * valid_mask
        
        # 3. SVD liefert bei statischem Input eine exakt vorhersagbare Anzahl an Singulärwerten
        S = torch.linalg.svdvals(x_centered)
        
        # WICHTIG FÜR CUDA GRAPH: Keine Slicing-Zuweisung wie `Sigma_D[:num_elements] = ...`!
        # Da die Dimensionen von S und features feststehen, berechnen wir Sigma_D direkt 
        # und füllen (padden) es, falls S kleiner als features ist, rein mathematisch auf.
        Sigma_D_raw = (S ** 2) / m_minus_1
        
        # Sicherstellen, dass Sigma_D exakt die Länge 'features' hat
        if Sigma_D_raw.shape[0] < features:
            # Kommt vor, wenn B*T < F ist. Wir padden statisch mit Nullen.
            padding = torch.zeros(features - Sigma_D_raw.shape[0], device=x.device, dtype=x.dtype)
            Sigma_D = torch.cat([Sigma_D_raw, padding], dim=0)
        else:
            # Falls S größer oder gleich features ist, schneiden wir statisch ab
            Sigma_D = Sigma_D_raw[:features]

        # 4. Normierung
        norm_Sigma_D = torch.linalg.vector_norm(Sigma_D)
        is_norm_zero = norm_Sigma_D < self.eps
        safe_norm = torch.where(is_norm_zero, torch.tensor(1.0, device=x.device, dtype=x.dtype), norm_Sigma_D)
        
        ones_n = torch.ones(features, device=x.device, dtype=x.dtype)
        sqrt_n = torch.sqrt(torch.tensor(n_fixed, device=x.device, dtype=x.dtype))
        Sigma_D_hat = (sqrt_n * Sigma_D) / safe_norm

        # 5. Isotropy Defect
        denom_val = torch.sqrt(2 * (n_fixed - torch.sqrt(torch.tensor(n_fixed, device=x.device, dtype=x.dtype))))
        denominator_delta = torch.where(denom_val > self.eps, denom_val, torch.tensor(1.0, device=x.device, dtype=x.dtype))
        
        diff_norm = torch.linalg.vector_norm(Sigma_D_hat - ones_n)
        delta_x = diff_norm / denominator_delta

        # 6. Occupation
        numerator_phi = n_fixed - (delta_x ** 2) * (n_fixed - sqrt_n)
        phi_x = (numerator_phi ** 2) / (n_fixed ** 2)

        # 7. Transformation
        denom_iota = torch.tensor(n_fixed - 1.0, device=x.device, dtype=x.dtype)
        denom_iota = torch.where(denom_iota < 1.0, torch.tensor(1.0, device=x.device, dtype=x.dtype), denom_iota)
        iota_x = (n_fixed * phi_x - 1.0) / denom_iota
        
        # 8. Finale Verzweigung (komplett in Tensor-Operationen ohne Python-if)
        final_score = torch.where(is_norm_zero, torch.tensor(0.0, device=x.device, dtype=x.dtype), iota_x)
        final_score_clamped = torch.clamp(final_score, 0.0, 1.0)
        
        # Falls n_fixed <= 1, gib 1.0 zurück
        return torch.where(
            torch.tensor(n_fixed <= 1.0, device=x.device), 
            torch.tensor(1.0, device=x.device, dtype=x.dtype), 
            final_score_clamped
        )

class feature_extractor_loss(torch.nn.Module):
  def __init__(self,pad_value):
    super().__init__()
    self.mse_loss=NormalizedMSELoss()
    #self.structure_loss_target=structure_loss
    #self.cor_loss=MaskedFeatureDeCorrelationLoss(pad_value=0.0)

  def forward(self,input,target,latent_space,mu,log_var,mask,beta):
    #loss_structure_target=self.structure_loss_target(input,target)
    loss_mse=self.mse_loss(input,target)
    kld_loss=KLDLoss(mu,log_var,mask)
    loss=loss_mse+beta*kld_loss
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
