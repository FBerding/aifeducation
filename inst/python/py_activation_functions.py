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

def get_act_fct(name):
  if name=="elu":
    return torch.nn.ELU()
  elif name="leakyrelu":
    return torch.nn.LeakyReLU()
  elif name="relu":
    return torch.nn.ReLU()
  elif name="gelu":
    return torch.nn.GELU()
  elif name="sigmoid":
    return torch.nn.Sigmoid()
  elif name="tanh":
    return torch.nn.TanH()
  elif name="prelu":
    return torch.nn.PReLU()
  
  
