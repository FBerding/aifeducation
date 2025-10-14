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
x=torch.rand((2,2))
y=x
# CosineDistance for all possible pairs
def CosineDistance(x,y,eps=1e-8):
  tmp_tensors=torch.cat((x,y),dim=0)
  x_expanded=torch.unsqueeze(tmp_tensors,dim=0)
  x_expanded=x_expanded.expand(tmp_tensors.size(0),tmp_tensors.size(0),tmp_tensors.size(1))
  
  y_expanded=torch.unsqueeze(tmp_tensors,dim=1)
  y_expanded=y_expanded.expand(tmp_tensors.size(0),tmp_tensors.size(0),tmp_tensors.size(1))
  
  similarity=torch.nn.functional.cosine_similarity(
    x1=x_expanded,
    x2=y_expanded,
    dim=-1,
    eps=eps
  )
  similarity=torch.index_select(
    input=similarity,
    dim=0,
    index=torch.arange(start=0,end=x.size(0))
  )
  similarity=torch.index_select(
    input=similarity,
    dim=1,
    index=torch.arange(start=x.size(0),end=(similarity.size(1)))
  )
  
  distance=1-similarity
  return distance

