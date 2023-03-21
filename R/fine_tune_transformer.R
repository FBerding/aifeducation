#'Function for creating a fine tuned transformer
#'
#'Insert Description
#'
#'@export
fine_tune_bert_model=function(output_dir,
                              bert_model_dir_path,
                              raw_texts,
                              vocab_draft,
                              aug_vocab_by=100,
                              p_mask=0.15,
                              whole_word=TRUE,
                              test_size=0.1,
                              n_epoch=1,
                              batch_size=12,
                              chunk_size=250,
                              n_workers=1,
                              multi_process=FALSE,
                              trace=TRUE){
  transformer = reticulate::import('transformers')
  tf = reticulate::import('tensorflow')
  datasets=reticulate::import("datasets")
  tok<-reticulate::import("tokenizers")

  mlm_model=transformer$TFBertForMaskedLM$from_pretrained(bert_model_dir_path)
  tokenizer<-transformer$BertTokenizerFast$from_pretrained(bert_model_dir_path)

  print(paste(date(),"Tokenize Raw Texts"))
  prepared_texts<-quanteda::tokens(
    x = raw_texts,
    what = "word",
    remove_punct = FALSE,
    remove_symbols = TRUE,
    remove_numbers = FALSE,
    remove_url = TRUE,
    remove_separators = TRUE,
    split_hyphens = FALSE,
    split_tags = FALSE,
    include_docvars = TRUE,
    padding = FALSE,
    verbose = trace)

  if(aug_vocab_by>0){
    print(paste(date(),"Augmenting vocabulary"))



    #Creating a new Tokenizer for Computing Vocabulary
    tok_new<-tok$Tokenizer(tok$models$WordPiece())
    tok_new$normalizer=tok$normalizers$BertNormalizer(lowercase= tokenizer$do_lower_case)
    tok_new$pre_tokenizer=tok$pre_tokenizers$BertPreTokenizer()
    tok_new$decode=tok$decoders$WordPiece()
    trainer<-tok$trainers$WordPieceTrainer(
      vocab_size=as.integer(length(tokenizer$get_vocab())+aug_vocab_by),
      show_progress=trace)

    #Calculating Vocabulary
    if(trace==TRUE){
      print(paste(date(),
                  "Start Computing Vocabulary"))
    }
    tok_new$train_from_iterator(raw_texts,trainer=trainer)
    new_tokens=names(tok_new$get_vocab())
    if(trace==TRUE){
      print(paste(date(),
                  "Start Computing Vocabulary - Done"))
    }

    print(paste(date(),"Adding",length(new_tokens),"New Tokens"))
    invisible(tokenizer$add_tokens(new_tokens = new_tokens))
    invisible(mlm_model$resize_token_embeddings(length(tokenizer)))
  }

  print(paste(date(),"Creating Text Chunks"))
  prepared_texts_chunks<-quanteda::tokens_chunk(
    x=prepared_texts,
    size=chunk_size,
    overlap = 0,
    use_docvars = FALSE)

  check_chunks_length=(quanteda::ntoken(prepared_texts_chunks)==chunk_size)

  prepared_texts_chunks<-quanteda::tokens_subset(
    x=prepared_texts_chunks,
    subset = check_chunks_length
  )

  prepared_text_chunks_strings<-lapply(prepared_texts_chunks,paste,collapse = " ")
  prepared_text_chunks_strings<-as.character(prepared_text_chunks_strings)
  print(paste(date(),length(prepared_text_chunks_strings),"Chunks Created"))

  print(paste(date(),"Creating Input"))
  tokenized_texts= tokenizer(prepared_text_chunks_strings,
                                                  truncation =TRUE,
                                                  padding= TRUE,
                                                  max_length=as.integer(chunk_size),
                                                  return_tensors="np")


  print(paste(date(),"Creating TensorFlow Dataset"))
  tokenized_dataset=datasets$Dataset$from_dict(tokenized_texts)

  if(whole_word==TRUE){
    print(paste(date(),"Using Whole Word Masking"))
    word_ids=matrix(nrow = length(prepared_texts_chunks),
                    ncol=(chunk_size-2))
    for(i in 0:(nrow(word_ids)-1)){
      word_ids[i,]<-as.vector(unlist(tokenized_texts$word_ids(as.integer(i))))
    }
    word_ids<-reticulate::dict("word_ids"=word_ids)
    word_ids<-datasets$Dataset$from_dict(word_ids)
    tokenized_dataset=tokenized_dataset$add_column(name="word_ids",column=word_ids)
    data_collator=transformer$DataCollatorForWholeWordMask(
      tokenizer = tokenizer,
      mlm = TRUE,
      mlm_probability = p_mask)
  } else {
    print(paste(date(),"Using Token Masking"))
    data_collator=transformer$DataCollatorForLanguageModeling(
      tokenizer = tokenizer,
      mlm = TRUE,
      mlm_probability = p_mask
    )
  }

  tokenized_dataset=tokenized_dataset$train_test_split(test_size=test_size)

  tf_train_dataset=mlm_model$prepare_tf_dataset(
    dataset = tokenized_dataset$train,
    batch_size = as.integer(batch_size),
    collate_fn = data_collator,
    shuffle = TRUE
  )
  tf_test_dataset=mlm_model$prepare_tf_dataset(
    dataset = tokenized_dataset$test,
    batch_size = as.integer(batch_size),
    collate_fn = data_collator,
    shuffle = TRUE
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
  mlm_model$compile(optimizer=adam(3e-5))

  print(paste(date(),"Start Fine Tuning"))
  mlm_model$fit(x=tf_train_dataset,
                validation_data=tf_test_dataset,
                epochs=as.integer(n_epoch),
                workers=as.integer(n_workers),
                use_multiprocessing=multi_process,
                callbacks=list(callback_checkpoint))

  print(paste(date(),"Load Weights From Best Checkpoint"))
  mlm_model$load_weights(paste0(output_dir,"/checkpoints/"))

  print(paste(date(),"Saving Bert Model"))
  mlm_model$save_pretrained(save_directory=output_dir)

  print(paste(date(),"Saving Tokenizer"))
  tokenizer$save_pretrained(output_dir)

  print(paste(date(),"Done"))

}
