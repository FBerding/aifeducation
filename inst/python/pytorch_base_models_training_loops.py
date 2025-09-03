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
import transformers
from transformers import TrainerCallback

class TorchProfilerCallback(transformers.TrainerCallback):
    def __init__(self, profiler):
        self.TorchProfiler = profiler

    def on_step_end(self, args, state, control, **kwargs):
        self.TorchProfiler.step()

def train_with_flops (trainer):
  
  if torch.cuda.is_available():
        activities=[
        torch.profiler.ProfilerActivity.CPU,
        torch.profiler.ProfilerActivity.CUDA
        ]
  else:
        activities=[torch.profiler.ProfilerActivity.CPU]
  
  with torch.profiler.profile(
    activities=activities, 
    record_shapes=False, 
    profile_memory=False, 
    with_stack=False, 
    with_flops=True, 
    with_modules=False,
    schedule=torch.profiler.schedule(skip_first=0, wait=0, warmup=1,active=1, repeat=0),
    experimental_config=None, 
    execution_trace_observer=None,
    acc_events=False, 
    custom_trace_id_callback=None
  ) as profiler:
    trainer.add_callback(TorchProfilerCallback(profiler=profiler))
    trainer.train()
  return profiler.key_averages()


  
