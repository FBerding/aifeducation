"""
BERT            MPNet
0   - [PAD];    1     - <pad>
100 - [UNK];    3     - <unk>
101 - [CLS];    0     - <s>
102 - [SEP];    2     - </s>
103 - [MASK];   30526 - <mask>
"""

# ======================================================================================
# Imports
# ======================================================================================

import tensorflow as tf

from transformers import DataCollatorForLanguageModeling, \
                         DataCollatorForWholeWordMask, \
                         TFMPNetForMaskedLM

# ======================================================================================
# LossFunction for MLM and PLM tasks
# ======================================================================================

class MPLMLoss_TF(tf.keras.layers.Layer):
    def __init__(self, ignore_index = -100):
        super().__init__()
        """
        tf.keras.losses.SparseCategoricalCrossentropy as the equivalent of nn.CrossEntropyLoss in PyTorch
        """
        self.criterion_mlm = tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True, ignore_class=ignore_index)
        self.criterion_plm = tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True, ignore_class=ignore_index)
    
    def call(self, mlm_logits, plm_logits, mlm_labels, plm_labels):
        """
        Logits (tf.Tensor of shape (batch_size x sequence_length, vocab_size)):
            tf.reshape(mlm_logits, [-1, mlm_logits.shape[-1]])
            tf.reshape(plm_logits, [-1, plm_logits.shape[-1]])
        Targets (tf.Tensor of shape (batch_size x sequence_length)):
            tf.reshape(mlm_labels, [-1])
            tf.reshape(plm_labels, [-1])

            [0 <= targets[i] < vocab_size]
        """
        # call(y_true, y_pred)
        mlm_loss = self.criterion_mlm(tf.reshape(mlm_labels, [-1]), tf.reshape(mlm_logits, [-1, mlm_logits.shape[-1]]))
        plm_loss = self.criterion_plm(tf.reshape(plm_labels, [-1]), tf.reshape(plm_logits, [-1, plm_logits.shape[-1]]))
        return mlm_loss + plm_loss
    
# ======================================================================================
# Head of model for MLM and PLM tasks
# ======================================================================================

class MPNetForMPLM_TF(TFMPNetForMaskedLM):
    def __init__(self, config):
        super().__init__(config)

        self.plm_head = tf.keras.layers.Dense(config.vocab_size, input_shape = (config.hidden_size,))
    
    def call(self, 
             input_ids = None, 
             attention_mask = None, 
             mlm_labels = None, 
             plm_labels = None):
        """
        outputs is TFBaseModelOutputWithPooling with args:
        - last_hidden_state (tf.Tensor of shape (batch_size, sequence_length, hidden_size))
        - ...

        e.g. hidden_size = 768
        """
        outputs = self.mpnet(input_ids, 
                             attention_mask = attention_mask)
        
        """
        mlm_logits, plm_logits (tf.Tensor of shape (batch_size, sequence_length, vocab_size))

        e.g. vocab_size = 30527
        """
        mlm_logits = self.lm_head(outputs.last_hidden_state)
        plm_logits = self.plm_head(outputs.last_hidden_state)  # Computing PLM logits

        outputs = (mlm_logits, plm_logits) + outputs[2:]

        if mlm_labels is not None and plm_labels is not None:
            loss_fct = MPLMLoss_TF()
            loss = loss_fct(mlm_logits, plm_logits, mlm_labels, plm_labels)
            outputs = (loss,) + outputs

        return outputs
