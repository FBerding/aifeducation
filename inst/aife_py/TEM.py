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


import transformers
import tokenizers
import torch

class IdentityTransformer(torch.nn.Module):
  def __init__(self,num_layer):
    super().__init__()
    self.num_layer=num_layer
  
  def forward(self,input_ids,attention_mask,token_type_ids=None):
    hidden_states_of_layers=()
    for i in range(self.num_layer+1):
      hidden_states_of_layers=hidden_states_of_layers+tuple(input_ids)
    hidden_states={"hidden_states": hidden_states_of_layers}
    return hidden_states

class TextEmbeddingModel(torch.nn.Module):
  def __init__(self,base_model,chunks, emb_layer_min, emb_layer_max, emb_pool_type, pad_value,sequence_mode):
    super().__init__()
    self.base_model=base_model
    self.chunks=chunks
    self.emb_layer_min=emb_layer_min
    self.emb_layer_max=emb_layer_max
    self.emb_pool_type=emb_pool_type
    self.pad_value=pad_value
    self.sequence_mode=sequence_mode
    self.n_layers=emb_layer_max-emb_layer_min+1

  @torch.inference_mode()
  def forward(self, input_ids, attention_mask, token_type_ids=None):
    #Select relevant chunks for the case that more chunks are available (e.g. long documents)
    n_chunks = min(input_ids.size(0), self.chunks)
    input_ids = input_ids[:n_chunks]
    attention_mask = attention_mask[:n_chunks]
    if token_type_ids is not None:
      token_type_ids = token_type_ids[:n_chunks]
    
    # Apply the model and receive the hidden states for calculating embeddings 
    if token_type_ids is None:
      embeddings = self.base_model(input_ids=input_ids, attention_mask=attention_mask, output_hidden_states=True)
    else:
      embeddings = self.base_model(input_ids=input_ids, attention_mask=attention_mask, token_type_ids=token_type_ids, output_hidden_states=True)
    hidden_states = embeddings["hidden_states"]

    # differentiate between hidden states which all have the same sequence length
    if self.sequence_mode == "equal":
      relevant_embeddings = torch.stack(hidden_states[self.emb_layer_min : self.emb_layer_max + 1], dim=1)
      #Continue depending on the chosen pooling method
      if self.emb_pool_type == "Average":
        mask_expanded = attention_mask.unsqueeze(1).unsqueeze(-1)
        sum_over_sequences = torch.sum(mask_expanded * relevant_embeddings, dim=2)
        sum_over_layers = torch.sum(sum_over_sequences, dim=1)
        n_elements = self.n_layers * attention_mask.sum(dim=1, keepdim=True)
        final_embeddings = sum_over_layers / n_elements
      elif self.emb_pool_type == "CLS":
        cls_tokens = relevant_embeddings[:, :, 0, :]
        final_embeddings = torch.sum(cls_tokens, dim=1) / self.n_layers
    else:
      if self.emb_pool_type == "CLS":
        relevant_states = torch.stack(hidden_states[self.emb_layer_min : self.emb_layer_max + 1], dim=1)
        final_embeddings = torch.sum(relevant_states[:, :, 0, :], dim=1) / self.n_layers

    #Add missing rows to ensure embeddings of the same shape
    if n_chunks < self.chunks:
      pad_size = (self.chunks - n_chunks, final_embeddings.size(1))
      additional_row = torch.full(pad_size, self.pad_value, device=final_embeddings.device, dtype=final_embeddings.dtype)
      final_embeddings = torch.cat((final_embeddings, additional_row), dim=0)
    
    return final_embeddings.unsqueeze(0)  
  
  
@torch.no_grad()    
def inject_mask_tokens(input_ids,mask_freq,mask_id,pad_id):
  B,T=input_ids.size()
  insert_mask=torch.zeros((B,T))
  mask_idx=torch.arange(start=1, end=T, step=mask_freq).long()
  insert_mask[:,mask_idx]=1
  insert_mask=insert_mask.masked_fill(mask=(input_ids==pad_id),value=0)
  insert_mask=insert_mask.bool()
  input_ids=input_ids.masked_fill(mask=insert_mask,value=mask_id)
  return input_ids
  
