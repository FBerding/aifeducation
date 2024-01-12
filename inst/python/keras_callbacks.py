import tensorflow as tf
import keras

class ReportAiforeducationShiny(keras.callbacks.Callback):
  def on_train_begin(self, logs=None):
    r.py_update_aifeducation_progress_bar_steps(value=0,total=self.params.get('steps'),title=("Batch/Step: "+str(0)+"/"+str(self.params.get('steps'))))
    r.py_update_aifeducation_progress_bar_epochs(value=0,total=self.params.get('epochs', -1),title=("Epoch: "+str(0)+"/"+str(self.params.get('epochs', -1))))

  def on_epoch_end(self, epoch, logs=None):
    r.py_update_aifeducation_progress_bar_epochs(value=epoch,total=self.params.get('epochs', -1),title=("Epoch: "+str(epoch)+"/"+str(self.params.get('epochs', -1))))
  
  def on_train_batch_end(self, batch, logs=None):
    r.py_update_aifeducation_progress_bar_steps(value=batch,total=self.params.get('steps'),title=("Batch/Step: "+str(batch)+"/"+str(self.params.get('steps'))))
    
    

    
