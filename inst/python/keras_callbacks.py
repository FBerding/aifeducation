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

import tensorflow as tf
import keras

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
