#'Function for training a gpt2 model
#'
#'Insert Description
#'
#'@export
train_gpt2_model<-function(
  model_dir,
  output_dir,
  raw_texts,
  vocab_size=30000,
  context_length=128,
  test_split=0.1,
  batch_size=2,
  n_epoch=1,
  n_workers=1,
  multi_process=FALSE){


transformer = reticulate::import('transformers')
tf = reticulate::import('tensorflow')
datasets=reticulate::import("datasets")

print(paste(date(),"Creating Tokenizer"))
old_tokenizer=transformer$AutoTokenizer$from_pretrained(model_dir)
tokenizer=old_tokenizer$train_new_from_iterator(
  text_iterator = raw_texts,
  vocab_size = as.integer(vocab_size))
tokenizer$pad_token=tokenizer$eos_token

print(paste(date(),"Initializing New Model"))
model_config = transformer$AutoConfig$from_pretrained(
  pretrained_model_name_or_path = model_dir,
  vocab_size=as.integer(length(tokenizer)),
  n_ctx=as.integer(context_length),
  bos_token_id=tokenizer$bos_token_id,
  eos_token_id=tokenizer$eos_token_id
  )
model = transformer$TFGPT2LMHeadModel(model_config)

print(paste(date(),"Preparing Data Set"))
tokenized_data=tokenizer(
  raw_texts,
  truncation = TRUE,
  max_length = as.integer(model$max_length),
  return_overflowing_tokens=TRUE,
  return_length=TRUE,
  return_tensors="np",
  padding=TRUE)
tokenized_data_set=datasets$Dataset$from_dict(tokenized_data)

data_collator=transformer$DataCollatorForLanguageModeling(
  tokenizer = tokenizer,
  mlm=FALSE)

tokenized_data_set<-tokenized_data_set$train_test_split(test_split)

tf_train_dataset=model$prepare_tf_dataset(
  tokenized_data_set$train,
  collate_fn=data_collator,
  shuffle=TRUE,
  batch_size=as.integer(batch_size)
)
tf_test_dataset=model$prepare_tf_dataset(
  tokenized_data_set$test,
  collate_fn=data_collator,
  shuffle=TRUE,
  batch_size=as.integer(batch_size)
)

print(paste(date(),"Preparing Training of the Model"))
adam<-tf$keras$optimizers$Adam

if(dir.exists(paste0(output_dir,"/checkpoints"))==FALSE){
  print(paste(date(),"Creating Checkpoint Directory"))
  dir.create(paste0(output_dir,"/checkpoints"))
}
callback_checkpoint=tf$keras$callbacks$ModelCheckpoint(
  filepath = paste0(output_dir,"/checkpoints/"),
  monitor="val_loss",
  verbose=1L,
  mode="auto",
  save_best_only=TRUE,
  save_freq="epoch",
  save_weights_only= TRUE
)

print(paste(date(),"Compile Model"))
model$compile(optimizer=adam(3e-5))

print(paste(date(),"Start Fine Tuning"))
model$fit(x=tf_train_dataset,
              validation_data=tf_test_dataset,
              epochs=as.integer(n_epoch),
              workers=as.integer(n_workers),
              use_multiprocessing=multi_process,
              callbacks=list(callback_checkpoint))

print(paste(date(),"Load Weights From Best Checkpoint"))
model$load_weights(paste0(output_dir,"/checkpoints/"))

print(paste(date(),"Saving Bert Model"))
model$save_pretrained(save_directory=output_dir)

print(paste(date(),"Saving Tokenizer"))
tokenizer$save_pretrained(output_dir)

print(paste(date(),"Done"))
}
