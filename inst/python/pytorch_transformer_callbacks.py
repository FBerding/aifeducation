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
import csv

def create_AIFETransformerCSVLogger_PT(loss_file,
                                       log_file, value_top, total_top, message_top, min_step,
                                       log_write_interval = 2):
  class AIFETransformerCSVLogger(transformers.TrainerCallback):
    def _write_to_log(self, value_epochs = 0, value_steps = 0):
      self.last_log = write_log_py(log_file, 
                                   value_top = value_top, total_top = total_top, message_top = message_top,
                                   value_middle = value_epochs, total_middle = self.total_epochs, message_middle = "Epochs",
                                   value_bottom = value_steps, total_bottom = self.total_steps, message_bottom = "Steps",
                                   last_log = self.last_log, write_interval = log_write_interval)
    
    def _write_loss(self):
      history = list()
      
      history.append(self.train_loss)
      history.append(self.eval_loss)
      
      if len(self.test_loss) != 0:
        history.append(self.test_loss)
      
      self.last_log_loss = write_log_performance_py(loss_file,
                                                    history = history, 
                                                    last_log = self.last_log_loss, write_interval = log_write_interval)
      
    def _calc_value_steps(self, global_step):
      return self.total_steps if global_step % self.total_steps == 0 else global_step % self.total_steps
      
    def on_train_begin(self, args, state, control, **kwargs):
      self.train_loss = list()
      self.eval_loss = list()
      self.test_loss = list()
      
      self.total_epochs = args.num_train_epochs
      self.total_steps = state.max_steps / self.total_epochs
      
      self.last_log = None
      self.last_log_loss = None
      
      self._write_to_log()
      
    def on_epoch_end(self, args, state, control, **kwargs):
      value_steps = self._calc_value_steps(state.global_step)
      
      self._write_to_log(value_epochs = state.epoch, value_steps = value_steps)
        
    def on_step_end(self, args, state, control, **kwargs):
      value_steps = self._calc_value_steps(state.global_step)
      
      if (state.global_step % min_step) == 0:
        self._write_to_log(value_epochs = state.epoch, value_steps = value_steps)
          
    def on_log(self, args, state, control, logs = None, **kwargs):
        if "loss" in logs:
          self.train_loss.append(logs["loss"])
        if "eval_loss" in logs:
          self.eval_loss.append(logs["eval_loss"])
        if "test_loss" in logs:
          self.test_loss.append(logs["test_loss"])
        
        if len(self.eval_loss) == len(self.train_loss):
          self._write_loss()
  
  return AIFETransformerCSVLogger()
    

class ReportAiforeducationShiny_PT(transformers.TrainerCallback):
    def on_train_begin(self, args, state, control, **kwargs):
        r.py_update_aifeducation_progress_bar_steps(
            value = 0,
            total = state.max_steps,
            title = ("Batch/Step: " + str(0) + "/" + str(int(state.max_steps/args.num_train_epochs))))
        r.py_update_aifeducation_progress_bar_epochs(
            value = 0,
            total = args.num_train_epochs,
            title = ("Epoch: " + str(0) + "/" + str(args.num_train_epochs)))
  
    def on_epoch_end(self, args, state, control, **kwargs):
        r.py_update_aifeducation_progress_bar_epochs(
            value = state.epoch,
            total = args.num_train_epochs,
            title = ("Epoch: " + str(int(state.epoch)) + "/" + str(args.num_train_epochs)))
    
    def on_step_end(self, args, state, control, **kwargs):
        r.py_update_aifeducation_progress_bar_steps(
            value = (state.global_step % (state.max_steps/args.num_train_epochs)),
            total = state.max_steps/args.num_train_epochs,
            title = ("Batch/Step: " + str((state.global_step % (state.max_steps/args.num_train_epochs))) + "/" + str(int(state.max_steps/args.num_train_epochs))))
