from transformers import DataCollatorForLanguageModeling, \
                         DataCollatorForWholeWordMask

# ======================================================================================
# DataCollator
# ======================================================================================

class CollatorMaker_TF:
    def __init__(self, tokenizer, mlm = True, mlm_probability = 0.15, plm_probability = 0.15, mask_whole_words = False):
        self.mask_whole_words = mask_whole_words
        self.collator = self.__make_collator()(tokenizer = tokenizer,
                                               mlm = mlm,
                                               mlm_probability = mlm_probability, 
                                               plm_probability = plm_probability)
        
    def __get_class_type(self):
        return DataCollatorForWholeWordMask if self.mask_whole_words \
               else DataCollatorForLanguageModeling
    
    def __make_collator(self):
        base_class = self.__get_class_type()

        class TFDataCollatorForMPLM(base_class):
            def __init__(self, tokenizer, mlm, mlm_probability, plm_probability):
                super().__init__(tokenizer = tokenizer, mlm = mlm, mlm_probability = mlm_probability, return_tensors='tf')
                self.tokenizer = tokenizer
                self.plm_probability = plm_probability

            def make_plm_labels(self, input_ids):
                plm_labels = tf.identity(input_ids)

                if len(tf.shape(plm_labels).numpy()) == 1: 
                    plm_labels = tf.expand_dims(plm_labels, 0)
                
                # Per each i-sample
                for i in range(input_ids.shape[0]):
                    # Indices of SEP tokens in i-sample
                    sep_pos_ids = tf.where(input_ids[i] == self.tokenizer.sep_token_id)[:, 0]

                    # Per each j-sentence in i-sample
                    for j in range(sep_pos_ids.shape[0]):
                        sent_start_id = 0 if j == 0 else sep_pos_ids[j - 1].numpy()
                        sent_end_id = sep_pos_ids[j].numpy()

                        # Get indices of all elements in j-th sentence (exclude sent_start_id and sent_end_id)
                        sent_ids = tf.range(sent_start_id + 1, sent_end_id)

                        sent_size = sent_ids.shape[0]

                        # Number of tokens to permute
                        k = ceil(sent_size * self.plm_probability)

                        if k <= 1:
                            continue
                        
                        # Randomly select K indices from all available indices
                        selected_indices = tf.random.shuffle(sent_ids)[:k]

                        # Get the values to permute
                        values_to_permute = tf.gather(input_ids[i], selected_indices)

                        # Permute those values
                        permuted_values = tf.random.shuffle(values_to_permute)

                        updated_indices = tf.stack([tf.cast(tf.repeat(i, k), tf.int64), selected_indices], axis = -1)

                        # Replace the values in the original tensor with the permuted values
                        plm_labels = tf.tensor_scatter_nd_update(plm_labels, updated_indices, permuted_values)
                        
                return plm_labels
            
            def collate_batch(self, examples):
                """
                examples: List(Dict)
                Every dictionary has the following keys:
                - input_ids
                - attention_mask

                examples
                [
                    {
                        'input_ids': tensor([]),
                        'attention_mask': tensor([])
                    },
                    {
                        'input_ids': tensor([]),
                        'attention_mask': tensor([])
                    }, ...
                ]
                """

                # self(examples) executes DataCollatorForLanguageModeling/DataCollatorForWholeWordMask
                # added masking <mask>/30526
                input_ids = self(examples)["input_ids"]
                # attention_mask = self(examples)["attention_mask"]
                
                """
                MPNet: Tokens with indices e.g. 30526 set to -100 are ignored (masked)
                """
                mlm_labels = tf.identity(input_ids)

                indices = tf.where(input_ids == self.tokenizer.mask_token_id)
                updates = tf.repeat(-100, indices.shape[0])

                mlm_labels = tf.tensor_scatter_nd_update(mlm_labels, indices, tf.cast(updates, tf.int64))

                # add permutation
                plm_labels = self.make_plm_labels(input_ids)

                return tf.data.Dataset.from_tensor_slices({
                    "input_ids": tf.constant([i['input_ids'].numpy().tolist() for i in examples]),
                    "attention_mask": tf.constant([i['attention_mask'].numpy().tolist() for i in examples]),
                    "mlm_labels": mlm_labels,
                    "plm_labels": plm_labels
                }).batch(len(examples))

        return TFDataCollatorForMPLM
