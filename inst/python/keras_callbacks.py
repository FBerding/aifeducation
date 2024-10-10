import tensorflow as tf
import keras
import csv

def create_AIFETransformerCSVLogger_TF(loss_file,
                                       log_file, value_top, total_top, message_top, min_step,
                                       log_write_interval = 2):
  class AIFETransformerCSVLogger(keras.callbacks.Callback):
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
        
      self.last_log_loss = write_log_performance_py(loss_file,
                                                    history = history, 
                                                    last_log = self.last_log_loss, write_interval = log_write_interval)
        
    def _write_loss(self):
      try:
        f = open(loss_file, "w", newline = "")
        writer = csv.writer(f, dialect = 'unix')
        writer.writerow(self.train_loss)
        writer.writerow(self.eval_loss)
        f.close()
      except:
        a = None
        
    def on_train_begin(self, logs = None):
      self.train_loss = list()
      self.eval_loss = list()
      
      self.global_step = 0
      self.prev_epoch = -1
      self.prev_step = -1
      
      self.total_epochs = self.params.get('epochs', -1)
      self.total_steps = self.params.get('steps')
      
      self.last_log = None
      self.last_log_loss = None
      
      self._write_to_log()
      
    def on_epoch_end(self, epoch, logs = None):
      self._write_to_log(value_epochs = epoch + 1, value_steps = self.prev_batch)
      self.prev_epoch = epoch + 1
      
      if "loss" in logs:
        self.train_loss.append(logs["loss"])
      if "val_loss" in logs:
        self.eval_loss.append(logs["val_loss"])
        
      self._write_loss()
      
    def on_train_batch_end(self, batch, logs = None):
      self.global_step = self.global_step + 1
      
      if (self.global_step % min_step) == 0:
        self._write_to_log(value_epochs = self.prev_epoch, value_steps = batch + 1)
        
      self.prev_batch = batch + 1
  
  return AIFETransformerCSVLogger()

class ReportAiforeducationShiny(keras.callbacks.Callback):
    def on_train_begin(self, logs = None):
        r.py_update_aifeducation_progress_bar_steps(
            value = 0, 
            total = self.params.get('steps'), 
            title = ("Batch/Step: " + str(0) + "/" + str(self.params.get('steps'))))
        r.py_update_aifeducation_progress_bar_epochs(
            value = 0, 
            total = self.params.get('epochs', -1), 
            title = ("Epoch: " + str(0) + "/" + str(self.params.get('epochs', -1))))
  
    def on_epoch_end(self, epoch, logs = None):
        r.py_update_aifeducation_progress_bar_epochs(
            value = epoch,
            total = self.params.get('epochs', -1),
            title = ("Epoch: " + str(epoch) + "/" + str(self.params.get('epochs', -1))))
    
    def on_train_batch_end(self, batch, logs = None):
        r.py_update_aifeducation_progress_bar_steps(
            value = batch,
            total = self.params.get('steps'),
            title = ("Batch/Step: " + str(batch) + "/" + str(self.params.get('steps'))))
