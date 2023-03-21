#' @title Question Completation Models
#'
#'@export
train_qa_completer<-function(
  output_dir,
  encoder_dir,
  decoder_dir,
  prefix_question,
  prefix_context,
  context_incomplete=TRUE,
  training_data,
  p_min=0.05,
  p_max=0.25,
  test_size=0.1,
  batch_size=1,
  n_epoch,
  n_workers=1,
  multi_process=FALSE){

  transformer = reticulate::import('transformers')
  tf = reticulate::import('tensorflow')
  datasets=reticulate::import("datasets")

  print(paste(date(),"Creating Model"))

  encoder_tokenizer=transformer$AutoTokenizer$from_pretrained(encoder_dir)

  #decoder_tokenizer=transformer$AutoTokenizer$from_pretrained(decoder_dir)

  model=transformer$TFEncoderDecoderModel$from_encoder_decoder_pretrained(
    encoder_dir,
    decoder_dir)

  model$config$decoder_start_token_id = encoder_tokenizer$cls_token_id
  model$config$pad_token_id = encoder_tokenizer$pad_token_id
  model$decoder$config$use_cache = FALSE
  #model$config$vocab_size = model$config$decoder$vocab_size
  #model$config$vocab_size = 30000

  print(paste(date(),"Preparing Data"))
  if(context_incomplete==TRUE){
    context_tokens_incomplet<-NULL
    print(paste(date(),"Creating Incomplete Context"))
    context_tokens<-quanteda::tokens(training_data$context)
    for(i in 1:length(context_tokens)){
      p_sample=sample(x=seq(from=p_min,to=p_max,by=0.01),size = 1)
      len_sequence=length(context_tokens[[i]])
      com_sequence=seq(from=1,to=len_sequence,by=1)
      selected_index<-sample(com_sequence,
                             size=ceiling(len_sequence*p_sample))
      remaining<-setdiff(com_sequence,selected_index)
      context_tokens_incomplet[i]=list(paste(context_tokens[[i]][remaining],collapse = " "))
    }
  }
  if(context_incomplete==TRUE){
    context=context_tokens_incomplet
  } else {
    context=training_data$context
  }

  input_text=matrix(nrow = nrow(training_data),ncol=2)
  for(i in 1:nrow(training_data)){
    input_text[i,1]<-paste(prefix_question,training_data[i,"question"],
                              prefix_context,context[i])
    input_text[i,2]<-training_data[i,"answer"]
  }

  input_ids=encoder_tokenizer(input_text[,1],
                              return_tensors="np",
                              padding=TRUE,
                              truncation=TRUE,
                              add_special_tokens=TRUE)
  labels=encoder_tokenizer(input_text[,2],
                           return_tensors="np",
                           padding=TRUE,
                           truncation=TRUE)

  tokenized_data_set=datasets$Dataset$from_dict(input_ids)
  labels=datasets$Dataset$from_dict(labels)
  tokenized_data_set=tokenized_data_set$add_column("labels",labels["input_ids"])
  tokenized_data_set=tokenized_data_set$train_test_split(test_size)

  tf_train_dataset=model$prepare_tf_dataset(
    tokenized_data_set$train,
    #collate_fn=data_collator,
    shuffle=TRUE,
    batch_size=as.integer(batch_size)
  )
  tf_test_dataset=model$prepare_tf_dataset(
    tokenized_data_set$test,
    #collate_fn=data_collator,
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
  encoder_tokenizer$save_pretrained(output_dir)
  if(dir.exists(paste0(output_dir,"/decoder"))==FALSE){
    print(paste(date(),"Creating Checkpoint Directory"))
    dir.create(paste0(output_dir,"/decoder"))
  }
  decoder_tokenizer$save_pretrained(paste0(output_dir,"/decoder"))

  print(paste(date(),"Done"))
}

