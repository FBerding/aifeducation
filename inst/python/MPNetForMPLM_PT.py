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

import torch
from torch import nn

from transformers import DataCollatorForLanguageModeling, \
                         DataCollatorForWholeWordMask, \
                         MPNetForMaskedLM

# ======================================================================================
# LossFunction for MLM and PLM tasks
# ======================================================================================

class MPLMLoss_PT(nn.Module):
    def __init__(self, ignore_index = -100):
        super().__init__()
        """
        The combination of nn.LogSoftmax and nn.NLLLoss is equivalent to using nn.CrossEntropyLoss
        """
        self.criterion_mlm = nn.CrossEntropyLoss(ignore_index = ignore_index)
        self.criterion_plm = nn.CrossEntropyLoss(ignore_index = ignore_index)

    def forward(self, mlm_logits, plm_logits, mlm_labels, plm_labels):
        """
        Logits (torch.FloatTensor of shape (batch_size x sequence_length, vocab_size)):
            mlm_logits.view(-1, mlm_logits.size(-1))
            plm_logits.view(-1, plm_logits.size(-1))
        Targets (torch.FloatTensor of shape (batch_size x sequence_length)):
            mlm_labels.view(-1)
            plm_labels.view(-1)

            [0 <= targets[i] < vocab_size]
        """
        # forward(y_pred, y_true)
        mlm_loss = self.criterion_mlm(mlm_logits.view(-1, mlm_logits.size(-1)), mlm_labels.view(-1))
        plm_loss = self.criterion_plm(plm_logits.view(-1, plm_logits.size(-1)), plm_labels.view(-1))

        return mlm_loss + plm_loss

# ======================================================================================
# Head of model for MLM and PLM tasks
# ======================================================================================

class MPNetForMPLM_PT(MPNetForMaskedLM):
    def __init__(self, config):
        super().__init__(config)
        self.plm_head = nn.Linear(config.hidden_size, config.vocab_size)  # Adding PLM output layer

        self.init_weights()

    def forward(self, 
                input_ids = None, 
                attention_mask = None, 
                mlm_labels = None, 
                plm_labels = None):
        """
        outputs is BaseModelOutputWithPooling with args:
        - last_hidden_state (torch.FloatTensor of shape (batch_size, sequence_length, hidden_size))
        - ...

        e.g. hidden_size = 768
        """
        outputs = self.mpnet(input_ids, 
                             attention_mask=attention_mask)
        
        """
        mlm_logits, plm_logits (torch.FloatTensor of shape (batch_size, sequence_length, vocab_size))

        e.g. vocab_size = 30527
        """
        mlm_logits = self.lm_head(outputs.last_hidden_state)
        plm_logits = self.plm_head(outputs.last_hidden_state)  # Computing PLM logits

        outputs = (mlm_logits, plm_logits) + outputs[2:]

        if mlm_labels is not None and plm_labels is not None:
            loss_fct = MPLMLoss_PT()
            loss = loss_fct(mlm_logits, plm_logits, mlm_labels, plm_labels)
            outputs = (loss,) + outputs

        return outputs
