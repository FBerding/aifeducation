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
import datetime

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
    report_to_aifeducation_studio = False):
      
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
            
    
    if report_to_aifeducation_studio == True:
        report_time = datetime.datetime.now()
        r.py_update_aifeducation_progress_bar_steps(
            value = 0,
            total = 1,
            title = ("Add: " + str(len(results)) + " Chunks. Last Update: " + report_time.strftime("%c")))
    
    return results
