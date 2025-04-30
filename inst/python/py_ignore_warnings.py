import warnings

def ignore_data_collator_warnings():
  warnings.filterwarnings("ignore", message="DataCollatorForWholeWordMask is only suitable for BertTokenizer-like tokenizers")
