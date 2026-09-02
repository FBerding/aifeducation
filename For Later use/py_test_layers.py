class layer_global_average_pooling_1d(torch.nn.Module):
  def __init__(self,mask_type="attention"):
    super().__init__()
    self.mask_type=mask_type
    self.is_attention = (mask_type == "attention")

  def forward(self, x, mask=None):
      # 1. GRAPH-BREAK-FIX: Mache die Maske intern obligatorisch, um Kontrollfluss zu verhindern.
      # Falls keine Maske übergeben wurde, erstellen wir dynamisch eine Maske aus lauter Einsen.
      # Da torch.ones die Symbole des Graphen erbt, bricht hier nichts auf.
      if mask is None:
          mask = torch.ones((x.size(0), x.size(1)), dtype=torch.bool, device=x.device)

      # 2. Masken-Auswahl ohne String-Vergleich
      applied_mask = mask if self.is_attention else ~mask
      
      # Form anpassen (view erzeugt einen View-Node im Graphen)
      mask_r = applied_mask.view(applied_mask.size(0), applied_mask.size(1), 1).to(x.dtype)
      
      # Nur gültige Werte behalten
      x_masked = x * mask_r
      
      # Berechne die Anzahl valider Token pro Batch-Zeile
      valid_tokens = torch.sum(mask_r, dim=1) # Form: [Batch, 1]
      
      # 3. COMPILER-FIX (Gegen Eq(2*u0, 0) Fehler): 
      # Wir berechnen die Kehrwerte sicher über torch.where.
      # Wichtig: Der Nenner im True-Pfad wird mit torch.clamp abgesichert, 
      # damit der Compiler bei ungenutzten Klassen/Zeilen keinen Zero-Division-Guard einbaut.
      inv_lengths = torch.where(
          valid_tokens > 0, 
          torch.reciprocal(torch.clamp(valid_tokens, min=1.0)), 
          torch.zeros_like(valid_tokens)
      )

      # Globale Summe über die Zeitachse (Dimension 1)
      sum_x = torch.sum(x_masked, dim=1) # Form: [Batch, Features]
      
      # Finale Mittelwertbildung über reine Multiplikation
      return sum_x * inv_lengths
