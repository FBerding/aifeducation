import warnings

def ignore_data_collator_warnings():
  warnings.filterwarnings(
    "ignore",
    message=r"^DataCollatorForWholeWordMask is only suitable for BertTokenizer-like tokenizers\.",
    category=UserWarning,
    module="transformers.data.data_collator")
