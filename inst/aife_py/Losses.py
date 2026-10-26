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


class NormalizedMAELoss(torch.nn.Module):
    def __init__(self, eps=1e-8):
        """
        Args:
            eps (float): Kleiner Wert zur Vermeidung von Division durch Null.
        """
        super().__init__()
        self.eps = eps

    def _compute_variance_per_feature(self, targets, mask_expanded, divisor_stat):
        """
        Berechnet die empirische Varianz separat für jedes Feature (F) 
        unter Berücksichtigung der validen (nicht-gepaddeten) Tokens.
        """
        # Summe pro Feature über die Batch- (B) und Zeitdimension (T) -> Shape: (F,)
        targets_masked = targets * mask_expanded
        sum_targets_per_feature = torch.sum(targets_masked, dim=(0, 1))
        
        F = targets.size(-1)
        target_mean_per_feature = (sum_targets_per_feature / divisor_stat).view(1, 1, F)
        
        # Quadratische Abweichung berechnen und maskieren
        diff_squared = ((targets - target_mean_per_feature) * mask_expanded) ** 2
        sum_diff_squared_per_feature = torch.sum(diff_squared, dim=(0, 1))
        
        # Lokale Batch-Varianz pro Feature -> Shape: (F,)
        variance_per_feature = sum_diff_squared_per_feature / divisor_stat
        return variance_per_feature.view(1, 1, F)

    def forward(self, predictions, targets, mask):
        # inputs: (B, T, F) - Shapes müssen bei CUDA Graphs statisch sein
        B, T, F = targets.size()
        
        # 1. Maskierungslogik (Invertieren: True -> 0.0, False -> 1.0)
        if mask.dim() == 2:
            mask = mask.unsqueeze(-1)
            
        mask_expanded = torch.where(mask, 0.0, 1.0).detach()  # Shape: (B, T, 1)
        
        # Valide Anzahl an Tokens ermitteln
        num_valid_tokens = torch.sum(mask_expanded)
        divisor_stat = torch.clamp(num_valid_tokens, min=self.eps)
        
        # 2. Ausgelagerte Berechnung der reinen Feature-Varianz auf dem Batch
        target_variance_broadcast = self._compute_variance_per_feature(
            targets, mask_expanded, divisor_stat
        )
        
        # 3. Echter MAE Fehler (Zähler für NMAE)
        error = torch.abs(predictions - targets)
        
        # 4. Fehler durch die feature-spezifische Varianz teilen
        nmae = error / torch.sqrt(target_variance_broadcast.detach() + self.eps)
        
        # 5. Maske anwenden (Gepaddete Elemente auf 0.0 setzen)
        nmae_masked = nmae * mask_expanded
        
        # 6. Mean-Reduction ohne CPU-GPU-Synchronisation (CUDA-Graph safe)
        num_valid_elements = num_valid_tokens * F
        divisor_loss = torch.clamp(num_valid_elements, min=self.eps)
        
        # Finaler mittlerer Loss
        return torch.sum(nmae_masked) / divisor_loss


def structure_loss(latent_space, target, mask_latent, mask_target):
    """
    Berechnet den Struktur-Verlust. Unterstützt identische oder unterschiedliche Sequenzlängen T.
    
    Args:
        latent_space: Tensor der Form [B, T_latent, F_latent]
        target: Tensor der Form [B, T_target, F_target]
        mask_latent: Tensor der Form [B, T_latent, 1] oder [B, T_latent] (True = Padding)
        mask_target: Tensor der Form [B, T_target, 1] oder [B, T_target] (True = Padding)
        CosineDistance: Funktion/Klasse zur Berechnung der paarweisen Cosine-Distanz
    """
    B, T_latent, F_latent = latent_space.shape
    B_target, T_target, F_target = target.shape
    
    # Validierung der Batch-Größe
    assert B == B_target, f"Batch-Größen müssen übereinstimmen: {B} vs {B_target}"
    
    # Sicherstellen, dass die Masken das Shape [B, T, 1] für Broad-Casting haben
    if mask_latent.dim() == 2:
        mask_latent = mask_latent.unsqueeze(-1)
    if mask_target.dim() == 2:
        mask_target = mask_target.unsqueeze(-1)
        
    # Invertieren: True soll für gültige Tokens (active) stehen
    active_latent = ~mask_latent
    active_target = ~mask_target
    
    # 1. Fallunterscheidung: Unterschiedliche Sequenzlängen T
    if T_latent != T_target:
        # Maskiertes Mean-Pooling über die Zeitdimension (Dim 1)
        latent_sums = torch.sum(latent_space * active_latent, dim=1)
        latent_counts = torch.clamp(torch.sum(active_latent, dim=1), min=1.0)
        latent_flat = latent_sums / latent_counts  # [B, F_latent]
        
        target_sums = torch.sum(target * active_target, dim=1)
        target_counts = torch.clamp(torch.sum(active_target, dim=1), min=1.0)
        target_flat = target_sums / target_counts  # [B, F_target]
        
        # Bestimmen, welche Batches überhaupt gültige (Nicht-Padding) Elemente enthalten
        batch_mask_latent = (latent_counts > 0).squeeze(-1)  # [B]
        batch_mask_target = (target_counts > 0).squeeze(-1)  # [B]
        global_mask = batch_mask_latent & batch_mask_target  # [B]
        
        N = B
        
    else:
        # Standard-Fall: Identische T -> Verflachen zu (B * T, F)
        N = B * T_latent
        latent_flat = latent_space.reshape(N, F_latent)
        target_flat = target.reshape(N, F_target)
        
        # Globale Token-Masken verflachen und kombinieren
        active_latent_flat = active_latent.reshape(N)
        active_target_flat = active_target.reshape(N)
        global_mask = active_latent_flat & active_target_flat  # [N]
    
    # 2. Paarweise Maske über alle N x N Kombinationen erstellen
    pairwise_mask = global_mask.unsqueeze(-1) & global_mask.unsqueeze(-2)
    
    # Obere Dreiecksmatrix (ohne Diagonale)
    triu_mask = torch.triu(torch.ones(N, N, dtype=torch.bool, device=target.device), diagonal=1)
    final_mask = pairwise_mask & triu_mask
    
    # 3. Globale paarweise Distanzen berechnen
    cos_distance_latent = CosineDistance(latent_flat, latent_flat)
    cos_distance_target = CosineDistance(target_flat, target_flat).detach()
    
    # Differenz berechnen
    diff = torch.abs(cos_distance_latent - cos_distance_target) / 2
    
    # Maskierung auf die Differenzen anwenden
    float_mask = final_mask.to(diff.dtype)
    masked_diff = diff * float_mask
    
    # 4. Globalen Mittelwert bilden (CUDA-Graph-safe)
    loss_sum = torch.sum(masked_diff)
    valid_elements = torch.sum(float_mask)
    
    loss = loss_sum / torch.clamp(valid_elements, min=1.0).detach()
    
    return loss


class MaskedFeatureDeCorrelationLoss(torch.nn.Module):
    def __init__(self, eps=1e-8):
        super(MaskedFeatureDeCorrelationLoss, self).__init__()
        self.eps = eps

    def forward(self, x, mask):
        """
        Args:
            x: Tensor der Form [B, T, F]
            mask: Tensor der Form [B, T, 1] oder [B, T] (True = Padding, False = Gültig)
        """
        # x-Shape erwartet: [B, T, F]
        b, t, f = x.size()

        # Sicherstellen, dass die Maske das Shape [B, T, 1] für Broad-Casting hat
        if mask.dim() == 2:
            mask = mask.unsqueeze(-1)

        # Invertieren: True (1.0) für valide Schritte, False (0.0) für gepaddete Schritte
        valid_mask = (~mask).float()  # [B, T, 1]

        # Anzahl valider Zeitschritte über alle Batches hinweg
        num_valid_elements = valid_mask.sum()  # Skalar (Anzahl der validen B,T Kombinationen)

        # 2. Maskierte Features zentrieren (Mittelwert abziehen)
        x_masked = x * valid_mask
        # Berechne den Mittelwert pro Feature über die Dimensionen B und T
        mean = x_masked.sum(dim=(0, 1), keepdim=True) / (num_valid_elements + self.eps)  # [1, 1, F]
        
        # Zentrieren und Padding-Elemente wieder auf 0 setzen
        x_centered = (x - mean) * valid_mask  # [B, T, F]

        # 3. Features flachklopfen zu [B*T, F] für effiziente Matrixalgebra
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

        # Erzeuge eine Identitätsmatrix, um die Diagonale zu ignorieren
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

class InfoVAEIndividualKLLoss(torch.nn.Module):
    def __init__(self):
        super().__init__()

    def forward(self, mu: torch.Tensor, logvar: torch.Tensor, mask: torch.Tensor) -> torch.Tensor:
        """
        Args:
            mu, logvar: Form [Batch, SeqLen, Latent_Dim] (Unter AMP meist Float16/BFloat16)
            mask: Form [Batch, SeqLen, 1] (True = Padding)
        """
        # AMP für den mathematisch kritischen Teil deaktivieren, um FP32-Präzision zu erzwingen
        with torch.amp.autocast(device_type=mu.device.type, enabled=False):
            # 1. UPCAST NACH FP32: Verhindert sofortige Overflows bei exp() und mu**2
            mu_f32 = mu.float()
            logvar_f32 = logvar.float()
            
            # 2. SAFETY CLAMP: Begrenzt extreme logvar-Werte nach oben (schützt exp() vor INF/NaN)
            # log(50000) ist ca. 10.82. Das hält torch.exp() sicher unter der FP16-Grenze von 65504
            logvar_clipped = torch.clamp(logvar_f32, max=10.5)
            
            # Gauß-KL-Divergenz stabil in FP32 berechnen
            # Summe über die Latent_Dim (letzte Dimension -> dim=-1)
            kld = -0.5 * torch.sum(1.0 + logvar_clipped - mu_f32 ** 2 - torch.exp(logvar_clipped), dim=-1)
            
            # Maske von [B, T, 1] auf [B, T] bringen
            active_steps = ~mask.squeeze(-1)
            kld_masked = kld * active_steps.to(kld.dtype)
            
            # Mitteln über alle validen Zeitschritte
            n_valid_steps = active_steps.sum()
            
            # Falls der gesamte Batch nur aus Padding besteht
            if n_valid_steps == 0:
                return torch.tensor(0.0, device=mu.device, dtype=mu.dtype, requires_grad=True)
                
            kld_mean = kld_masked.sum() / n_valid_steps
            
            # InfoVAE-spezifische Gewichtung anwenden
            loss_val =  kld_mean
            
        # 3. DOWNCAST: Zurückkonvertieren in den ursprünglichen AMP-Datentyp (z.B. FP16 oder BF16)
        return loss_val.to(mu.dtype)


class InfoVAEReconstructionLoss(torch.nn.Module):
    def __init__(self, distribution='gaussian'):
        """
        distribution='gaussian' -> Reiner MSE-Verlust
        distribution='bernoulli' -> Reiner BCE-Verlust
        """
        super().__init__()
        self.distribution = distribution

    def forward(self, x_recon: torch.Tensor, x: torch.Tensor, mask: torch.Tensor) -> torch.Tensor:
        """
        Args:
            x_recon, x: Form [Batch, SeqLen, Features]
            mask: Form [Batch, SeqLen, 1] (True = Padding)
        """
        if self.distribution == 'gaussian':
            # Mathematischer MSE: (y_pred - y_true)^2
            loss_elementwise = (x_recon - x) ** 2
        elif self.distribution == 'bernoulli':
            # Mathematischer BCE: -(y * log(p) + (1 - y) * log(1 - p))
            eps = 1e-12
            x_recon = torch.clamp(x_recon, eps, 1.0 - eps)
            loss_elementwise = -(x * torch.log(x_recon) + (1.0 - x) * torch.log(1.0 - x_recon))
        else:
            raise ValueError(f"Unbekannte Verteilung: {self.distribution}")

        active_elements = ~mask
        # Maskieren und aufsummieren
        loss_masked = loss_elementwise * active_elements.to(loss_elementwise.dtype)
        
        # Teilen durch die Anzahl der nicht-gepaddeten Elemente
        return loss_masked.sum() / torch.clamp(active_elements.sum(), min=1.0)

class InfoVAEAggregatedMMDLoss_back_up(torch.nn.Module):
    def __init__(self, alpha: float, lambd: float):
        super().__init__()
        self.alpha = alpha
        self.lambd = lambd

    def _compute_pairwise_distances(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        """Berechnet paarweise quadrierte euklidische Distanzen: ||x_i - y_j||^2"""
        x_norm = torch.sum(x ** 2, dim=-1, keepdim=True)
        y_norm = torch.sum(y ** 2, dim=-1, keepdim=True)
        cross_term = torch.matmul(x, y.transpose(-2, -1))
        
        # Absichern gegen minimale numerische Rundungsfehler unter 0.0
        distances = x_norm + y_norm.transpose(-2, -1) - 2.0 * cross_term
        return torch.clamp(distances, min=0.0)

    def _rbf_kernel_with_median_heuristic(self, x: torch.Tensor, y: torch.Tensor, z_combined: torch.Tensor) -> torch.Tensor:
        """Berechnet den RBF-Kernel unter Verwendung der Median-Distanz."""
        pairwise_distances = self._compute_pairwise_distances(x, y)
        
        # Median-Heuristik auf Basis der kombinierten Stichproben bestimmen
        # Wir berechnen dafür alle paarweisen Abstände der kombinierten Datenpunkte
        with torch.no_grad():
            combined_distances = self._compute_pairwise_distances(z_combined, z_combined)
            # Der Median benötigt einen flachen Tensor
            median_distance = torch.median(combined_distances.view(-1))
            
            # Absichern, falls alle Punkte identisch sind (Median wäre 0.0)
            bandwidth = torch.clamp(median_distance, min=1e-6)
            bandwidth=float(x.size(1)/2)
            
        # RBF-Formel: exp(-||x - y||^2 / (2 * sigma^2))
        # In der klassischen Median-Heuristik entspricht die ermittelte Median-Distanz direkt (2 * sigma^2)
        return torch.exp(-pairwise_distances / bandwidth)

    def forward(self, z: torch.Tensor, mask: torch.Tensor) -> torch.Tensor:
        """
        Args:
            z: Form [Batch, SeqLen, Latent_Dim]
            mask: Form [Batch, SeqLen, 1] (True = Padding)
        """
        latent_dim = z.size(-1)
        
        # Filtert Padding-Vektoren via flacher Maske heraus
        active_indices = ~mask.flatten()
        z_flat = z.view(-1, latent_dim)
        z_valid = z_flat[active_indices]
        
        # Falls der gesamte Batch nur aus Padding besteht
        if z_valid.size(0) == 0:
            return torch.tensor(0.0, device=z.device, requires_grad=True)

        # Generiert Prior-Samples mit purem torch.randn
        prior_samples = torch.randn(z_valid.shape, device=z.device, dtype=z.dtype)

        # Kombinierte Stichprobe für eine repräsentative Median-Schätzung erzeugen
        z_combined = torch.cat([z_valid, prior_samples], dim=0)

        # Matrizen über RBF-Kernel mit dynamischer Bandbreite berechnen
        k_zz = self._rbf_kernel_with_median_heuristic(z_valid, z_valid, z_combined)
        k_pp = self._rbf_kernel_with_median_heuristic(prior_samples, prior_samples, z_combined)
        k_zp = self._rbf_kernel_with_median_heuristic(z_valid, prior_samples, z_combined)

        # Standard MMD-Wert berechnen
        mmd = k_zz.mean() + k_pp.mean() - 2.0 * k_zp.mean()
        
        # Gewichtung laut InfoVAE-Formel anwenden
        weight = self.alpha + self.lambd - 1.0
        return weight * mmd

class InfoVAEAggregatedMMDLoss(torch.nn.Module):
    def __init__(self, kernel_type: str = 'gaussian', kernel_scales: list = None):
        super().__init__()
        self.kernel_type = kernel_type.lower()
        
        if self.kernel_type not in ['gaussian', 'cosine']:
            raise ValueError("Ungültiger kernel_type. Erlaubt sind 'gaussian' oder 'cosine'.")
        
        # Geometrische Skalen (wird nur für den Gauß-Kernel benötigt)
        self.kernel_scales = kernel_scales if kernel_scales is not None else [0.005,0.05, 0.1, 0.25, 0.5, 1.0, 2.0, 4.0, 8.0, 16.0,32.0,64.0,128.0]

    def _compute_pairwise_distances(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        """Berechnet paarweise quadrierte euklidische Distanzen in FP32."""
        x = x.float()
        y = y.float()
        
        x_norm = torch.sum(x ** 2, dim=-1, keepdim=True)
        y_norm = torch.sum(y ** 2, dim=-1, keepdim=True)
        cross_term = torch.matmul(x, y.transpose(-2, -1))
        
        distances = x_norm + y_norm.transpose(-2, -1) - 2.0 * cross_term
        return torch.clamp(distances, min=1e-6)

    def _multi_scale_rbf_kernel(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        """Berechnet den Multi-Scale RBF-Kernel stabil in FP32."""
        pairwise_distances = self._compute_pairwise_distances(x, y)
        
        latent_dim = x.size(-1)
        base_bandwidth = float(latent_dim)
        base_bandwidth = max(base_bandwidth, 1e-6)

        kernel_val = 0.0
        for scale in self.kernel_scales:
            current_bandwidth = base_bandwidth * scale
            kernel_val += torch.exp(-pairwise_distances / current_bandwidth)
            
        return kernel_val / len(self.kernel_scales)

    def _cosine_kernel(self, x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
        """Berechnet die paarweise Cosine-Ähnlichkeit stabil in FP32."""
        x = x.float()
        y = y.float()
        
        # L2-Normen berechnen (mit Epsilon gegen Division durch Null)
        x_norm = torch.norm(x, p=2, dim=-1, keepdim=True)
        y_norm = torch.norm(y, p=2, dim=-1, keepdim=True)
        
        x_normalized = x / torch.clamp(x_norm, min=1e-6)
        y_normalized = y / torch.clamp(y_norm, min=1e-6)
        
        # Paarweises Skalarprodukt (Ergebnis im Bereich [-1, 1])
        cosine_sim = torch.matmul(x_normalized, y_normalized.transpose(-2, -1))
        
        # Optional, aber empfohlen für MMD: Shiften in den Bereich, 
        # damit minimale Ähnlichkeit nicht negativ in den Schätzer einfließt.
        return cosine_sim + 1.0

    def forward(self, z: torch.Tensor, mask: torch.Tensor) -> torch.Tensor:
        """
        Args:
            z: Form [Batch, SeqLen, Latent_Dim] (Latent_Dim von 32 bis zu 256)
            mask: Form [Batch, SeqLen, 1] (True = Padding)
        """
        latent_dim = z.size(-1)
        
        # Filtert Padding-Vektoren via flacher Maske heraus
        active_indices = ~mask.flatten()
        z_flat = z.view(-1, latent_dim)
        z_valid = z_flat[active_indices]
        
        n_samples = z_valid.size(0)
        
        # Falls der gesamte Batch nur aus Padding besteht
        if n_samples == 0:
            return torch.tensor(0.0, device=z.device, dtype=z.dtype, requires_grad=True)

        # Generiert Prior-Samples mit Oversampling zur Reduzierung des stochastischen Rauschens
        prior_multiplier = 1
        prior_samples = torch.randn(n_samples * prior_multiplier, latent_dim, device=z.device, dtype=z.dtype)

        # AMP (Autocast) für den Kernel-Block explizit deaktivieren (Stabilität für Mixed Precision)
        with torch.amp.autocast(device_type=z.device.type, enabled=False):
            # Dynamische Auswahl des Kernels basierend auf init-Konfiguration
            if self.kernel_type == 'gaussian':
                kernel_fn = self._multi_scale_rbf_kernel
            else:
                kernel_fn = self._cosine_kernel

            k_zz = kernel_fn(z_valid, z_valid)
            k_pp = kernel_fn(prior_samples, prior_samples)
            k_zp = kernel_fn(z_valid, prior_samples)

            # STRIKT UNBIASED SCHÄTZER: Diagonale wird komplett ausgeschlossen
            if n_samples > 1:
                k_zz_mean = (k_zz.sum() - torch.diag(k_zz).sum()) / (n_samples * (n_samples - 1))
            else:
                k_zz_mean = k_zz.mean()

            n_prior = prior_samples.size(0)
            k_pp_mean = (k_pp.sum() - torch.diag(k_pp).sum()) / (n_prior * (n_prior - 1))
            k_zp_mean = k_zp.mean()

            # Mathematische Differenz berechnen
            mmd = k_zz_mean + k_pp_mean - 2.0 * k_zp_mean

            # CLAMP: Verhindert negative Werte bei dichten Clustern
            mmd = torch.clamp(mmd, min=0.0)

        # Zurückkonvertieren in den ursprünglichen AMP-Datentyp
        return mmd.to(z.dtype)








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

class OriginEuclideanDistanceLoss(torch.nn.Module):
    def __init__(self, weight: float = 1.0):
        """
        Args:
            weight: Gewichtung für das Matching der euklidischen Distanzrelationen.
        """
        super().__init__()
        self.weight = weight

    def _compute_origin_euclidean(self, x: torch.Tensor) -> torch.Tensor:
        """Berechnet den euklidischen Abstand jedes Punktes zum Ursprung (Norm)."""
        return torch.sqrt(torch.clamp(torch.sum(x ** 2, dim=-1), min=1e-6))

    def forward(self, latent_space: torch.Tensor, target: torch.Tensor, mask_latent: torch.Tensor, mask_target: torch.Tensor) -> torch.Tensor:
        # 1. Dimensionen prüfen und ggf. über die Zeitdimension (dim=1) mitteln
        if latent_space.shape != target.shape:
            latent_zeroed = latent_space * (~mask_latent).float()
            counts_latent = torch.clamp((~mask_latent).float().sum(dim=1, keepdim=True), min=1.0)
            latent_space = latent_zeroed.sum(dim=1) / counts_latent.squeeze(-1)

            target_zeroed = target * (~mask_target).float()
            counts_target = torch.clamp((~mask_target).float().sum(dim=1, keepdim=True), min=1.0)
            target = target_zeroed.sum(dim=1) / counts_target.squeeze(-1)
            
            latent_space = latent_space.unsqueeze(1)
            target = target.unsqueeze(1)

        B, T, F_latent = latent_space.shape
        N = B * T

        # 2. Flache Masken bestimmen (Ohne Neuzuweisung der Original-Masken)
        if T == 1:
            active_latent = torch.ones(N, dtype=torch.bool, device=target.device)
            active_target = torch.ones(N, dtype=torch.bool, device=target.device)
        else:
            active_latent = (~mask_latent).reshape(N)
            active_target = (~mask_target).reshape(N)

        global_valid_flat = active_latent & active_target

        latent_flat = latent_space.view(N, F_latent)
        target_flat = target.view(N, target.shape[-1])

        # 3. Rohe Abstände zum Ursprung berechnen (Völlig unnormiert)
        dist_orig = self._compute_origin_euclidean(target_flat)  # [N]
        dist_lat = self._compute_origin_euclidean(latent_flat)    # [N]

        # 4. RELATION DER URSPRUNDSABSTÄNDE: Paarweise Verhältnisse via Kreuzprodukt
        relation_lat = torch.outer(dist_lat, dist_orig)  # d_lat_i * d_orig_j
        relation_orig = torch.outer(dist_orig, dist_lat) # d_orig_i * d_lat_j

        # 5. Maske für Paarbeziehungen aufbauen (Obere Dreiecksmatrix ohne Diagonale)
        pairwise_mask = global_valid_flat.unsqueeze(-1) & global_valid_flat.unsqueeze(-2)
        triu_mask = torch.triu(torch.ones(N, N, dtype=torch.bool, device=target.device), diagonal=1)
        final_pairwise_mask = pairwise_mask & triu_mask
        num_pairs = torch.clamp(final_pairwise_mask.sum(), min=1.0)

        # 6. DISTANZ DER RELATIONEN (Robuster L1-Ansatz gegen Oszillationen)
        distance_of_relations = torch.abs(relation_lat - relation_orig)
        
        # KORREKTUR: .squeeze() stellt sicher, dass der Ausgang ein echter 0D-Skalar ist
        euclidean_loss = (torch.sum(distance_of_relations * final_pairwise_mask.float()) / num_pairs).squeeze()
        
        return self.weight * euclidean_loss

class PairwiseCosineDistanceLoss(torch.nn.Module):
    def __init__(self, weight: float = 1.0, eps: float = 1e-8):
        """
        Args:
            weight: Gewichtung für das Matching der paarweisen Cosine-Distanzen.
            eps: Numerisches Epsilon zur Vermeidung von Division durch Null.
        """
        super().__init__()
        self.weight = weight
        self.eps = eps

    def _compute_pairwise_cosine(self, x: torch.Tensor) -> torch.Tensor:
        """Berechnet paarweise Cosine-Distanzen (1.0 - Cosine-Similarity)."""
        x_norm = torch.norm(x, p=2, dim=-1, keepdim=True)
        x_normalized = x / torch.clamp(x_norm, min=self.eps)
        cosine_sim = torch.matmul(x_normalized, x_normalized.t())
        return 1.0 - cosine_sim

    def forward(self, latent_space: torch.Tensor, target: torch.Tensor, mask_latent: torch.Tensor, mask_target: torch.Tensor) -> torch.Tensor:
        # 1. Dimensionen prüfen und ggf. über die Zeitdimension (dim=1) mitteln
        if latent_space.shape != target.shape:
            # Maskiertes Mitteln unter Verwendung des originalen Formats [B, T, 1]
            latent_zeroed = latent_space * (~mask_latent).float()
            counts_latent = torch.clamp((~mask_latent).float().sum(dim=1, keepdim=True), min=1.0)
            latent_space = latent_zeroed.sum(dim=1) / counts_latent.squeeze(-1)

            target_zeroed = target * (~mask_target).float()
            counts_target = torch.clamp((~mask_target).float().sum(dim=1, keepdim=True), min=1.0)
            target = target_zeroed.sum(dim=1) / counts_target.squeeze(-1)
            
            latent_space = latent_space.unsqueeze(1)
            target = target.unsqueeze(1)

        B, T, F_latent = latent_space.shape
        N = B * T

        # 2. Flache Masken bestimmen (Keine Neuzuweisung von mask_latent oder mask_target)
        if T == 1:
            # Wenn gemittelt wurde, ist die Maske laut Vorgabe immer gültig (True)
            active_latent = torch.ones(N, dtype=torch.bool, device=target.device)
            active_target = torch.ones(N, dtype=torch.bool, device=target.device)
        else:
            # Wenn T > 1, nutzen wir die originalen Token-Masken direkt flachgeklopft
            active_latent = (~mask_latent).reshape(N)
            active_target = (~mask_target).reshape(N)

        global_valid_flat = active_latent & active_target

        latent_flat = latent_space.view(N, F_latent)
        target_flat = target.view(N, target.shape[-1])

        # Paarmasmaske für die obere Dreiecksmatrix generieren
        pairwise_mask = global_valid_flat.unsqueeze(-1) & global_valid_flat.unsqueeze(-2)
        triu_mask = torch.triu(torch.ones(N, N, dtype=torch.bool, device=target.device), diagonal=1)
        final_pairwise_mask = pairwise_mask & triu_mask
        num_pairs = torch.clamp(final_pairwise_mask.sum(), min=1.0)

        # Cosine-Distanzen ermitteln
        dist_orig_cos = self._compute_pairwise_cosine(target_flat)
        dist_lat_cos = self._compute_pairwise_cosine(latent_flat)

        # Loss berechnen
        cosine_loss = torch.sum(torch.abs(dist_orig_cos - dist_lat_cos) * final_pairwise_mask.float())/ num_pairs
        
        return self.weight * cosine_loss

class LatentScaleRegularizationLoss(torch.nn.Module):
    def __init__(self, weight: float = 0.1, target_scale: float = 0.5):
        """
        Args:
            weight: Gewichtung für die Regularisierung des latenten Mittelwerts.
            target_scale: Der Zielwert, gegen den der mittlere euklidische Abstand reguliert wird.
        """
        super().__init__()
        self.weight = weight
        self.target_scale = target_scale

    def forward(self, latent_space: torch.Tensor, mask_latent: torch.Tensor) -> torch.Tensor:
        """
        Args:
            latent_space: [B, T, F] Garantiert immer 3D.
            mask_latent: [B, T, 1] oder [B, T]
        """
        # 1. Bringt die Maske inline auf [B, T], ohne mask_latent zu verändern
        current_mask = mask_latent.squeeze(-1) if mask_latent.dim() == 3 else mask_latent
        valid_weight = (~current_mask).float()  # [B, T]
        num_tokens = torch.clamp(valid_weight.sum(), min=1.0)
        
        # 2. Berechne euklidische Norm für jeden einzelnen Token direkt
        token_norms = torch.sqrt(torch.clamp(torch.sum(latent_space ** 2, dim=-1), min=1e-6))  # [B, T]
        
        # 3. Gewichteter Mittelwert über alle gültigen Token (funktioniert für jedes T)
        mean_lat_eucl = (token_norms * valid_weight).sum() / num_tokens

        # 4. Quadratischer Fehler gegen die Zielskala (0.5)
        scale_regularization_loss = (mean_lat_eucl - self.target_scale) ** 2
        
        return self.weight * scale_regularization_loss


class feature_extractor_loss(torch.nn.Module):
  def __init__(self,pad_value,loss_type,alpha,p_lambda,features_in):
    super().__init__()
    self.mae_loss=NormalizedMAELoss(eps=1e-8)
    self.p_lambda=p_lambda
    self.alpha=alpha
    if loss_type=="InfoVAE":
      self.is_InfoVAE=True
      self.is_NMSE=False
      self.kld=InfoVAEIndividualKLLoss()
      self.mmd=InfoVAEAggregatedMMDLoss(
        kernel_type="gaussian"
        #kernel_type="cosine"
      )
    elif loss_type=="CosineStructure":
      self.is_InfoVAE=False
      self.is_NMSE=False
      self.euclidean=OriginEuclideanDistanceLoss(weight=1.0)
      self.cosine=PairwiseCosineDistanceLoss()
      self.mean_euc=LatentScaleRegularizationLoss(weight=1.0)
    elif loss_type=="MNSELoss":
      self.is_NMSE=True
      self.is_InfoVAE=False


  def forward(self,input,target,latent_space,mu,log_var,mask,mask_latent,beta):
    loss_mae=self.mae_loss(input,target,mask)
    if self.is_InfoVAE:
      loss_kld=self.kld(mu,log_var,mask_latent)
      loss_mmd=self.mmd(latent_space,mask_latent)
      loss=loss_mae+beta*((1.0 - self.alpha) *loss_kld+(self.alpha + self.p_lambda - 1.0)*loss_mmd)
      diagnostic=[loss_mae.detach(),loss_kld.detach(),loss_mmd.detach(),0.0,0.0,0.0]
    elif self.is_NMSE:
      loss=loss_mae
      diagnostic=[loss_mae.detach(),0.0,0.0,0.0,0.0,0.0]
    else:
      loss_magnitude=self.euclidean(latent_space=latent_space, target=target, mask_latent=mask_latent, mask_target=mask)
      loss_cosine=self.cosine(latent_space=latent_space, target=target, mask_latent=mask_latent, mask_target=mask)
      loss_scale=self.mean_euc(latent_space=latent_space, mask_latent=mask_latent)
      loss=loss_mae+self.p_lambda*((1-self.alpha)*loss_magnitude+self.alpha*loss_cosine+loss_scale)
      diagnostic=[loss_mse.detach(),0.0,0.0,loss_magnitude.detach(),loss_cosine.detach(),loss_scale.detach()]
    return loss, diagnostic

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
