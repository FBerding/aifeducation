import transformers
import datetime

global current_batch
current_batch = 0
last_log = None

def tokenize_raw_text(
    dataset,
    tokenizer,
    truncation,
    padding,
    max_length,
    return_overflowing_tokens,
    return_length,
    return_special_tokens_mask,
    return_offsets_mapping,
    return_attention_mask,
    return_tensors,
    request_word_ids = False,
    log_file = None,
    value_top = 0, total_top = 1, message_top = "NA",
    total_middle = 1):
      
    global current_batch
    global last_log
      
    outputs = tokenizer(
        dataset["text"],
        truncation = truncation, 
        padding = padding,
        max_length = max_length,
        return_overflowing_tokens = return_overflowing_tokens,
        return_length = return_length,
        return_special_tokens_mask = return_special_tokens_mask,
        return_offsets_mapping = return_offsets_mapping,
        return_attention_mask = return_attention_mask,
        return_tensors = return_tensors)
  
    padding_collator = transformers.DataCollatorWithPadding(
        tokenizer = tokenizer,
        padding = "max_length",
        max_length = max_length,
        return_tensors = return_tensors
    )
  
    input_batch = []
    attention_masks_batch = []
    special_tokens_mask_batch = []
    length_batch = []

    for length, input_ids, attention_mask, special_tokens_mask in zip(outputs["length"], outputs["input_ids"], outputs["attention_mask"], outputs["special_tokens_mask"]):
        if not length == max_length:
            padded_output = padding_collator({ "input_ids": [input_ids],
                                               "attention_mask": [attention_mask],
                                               "special_tokens_mask": [special_tokens_mask] })
            input_batch.append(padded_output["input_ids"][0])
            attention_masks_batch.append(padded_output["attention_mask"][0])
            special_tokens_mask_batch.append(padded_output["special_tokens_mask"][0])
        else:
            input_batch.append(input_ids)
            attention_masks_batch.append(attention_mask)
            special_tokens_mask_batch.append(special_tokens_mask)
        length_batch.append(length)
        
        results = { "input_ids": input_batch,
                    "attention_mask": attention_masks_batch,
                    "special_tokens_mask": special_tokens_mask_batch,
                    "labels": input_batch.copy(),
                    "length": length_batch }
                        
        if request_word_ids == True:
            word_ids_batch=[]
            for i in range(len(outputs["input_ids"])):
                word_ids_batch.append(outputs.word_ids(i)) 
            results["word_ids"] = word_ids_batch
    
    current_batch = current_batch + 1
    
    if current_batch == total_middle: # this is the last iteration
      last_log = None
    last_log = write_log_py(log_file,
                            value_top = value_top, total_top = total_top, message_top = message_top,
                            value_middle = current_batch,
                            total_middle = total_middle,
                            message_middle = "Batches",
                            last_log = last_log, write_interval = 0.5)

    return results
