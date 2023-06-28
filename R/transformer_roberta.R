#'Function for creating a new transformer based on RoBERTa
#'
#'This function creates a transformer configuration based on the RoBERTa base architecture
#'and a vocabulary based on Byte-Pair Encoding (BPE) tokenizer by using
#'the python libraries 'transformers' and 'tokenizers'.
#'
#'@param model_dir \code{string} Path to the directory where the model should be saved.
#'@param vocab_raw_texts \code{vector} containing the raw texts for creating the
#'vocabulary.
#'@param vocab_size \code{int} Size of the vocabulary.
#'@param add_prefix_space \code{bool} \code{TRUE} if an additional space should be insert
#'to the leading words.
#'@param max_position_embeddings \code{int} Number of maximal position embeddings. This parameter
#'also determines the maximum length of a sequence which can be processed with the model.
#'@param hidden_size \code{int} Number of neurons in each layer. This parameter determines the
#'dimensionality of the resulting text embedding.
#'@param num_hidden_layer \code{int} Number of hidden layers.
#'@param num_attention_heads \code{int} Number of attention heads.
#'@param intermediate_size \code{int} Number of neurons in the intermediate layer of
#'the attention mechanism.
#'@param hidden_act \code{string} name of the activation function.
#'@param hidden_dropout_prob \code{double} Ratio of dropout
#'@param trace \code{bool} \code{TRUE} if information about the progress should be
#'printed to the console.
#'@return This function does not return an object. Instead the configuration
#'and the vocabulary of the new model are saved on disk.
#'@note To train the model, pass the directory of the model to the function
#'\link{train_tune_roberta_model}.
#'
#'@references
#'Liu, Y., Ott, M., Goyal, N., Du, J., Joshi, M., Chen, D., Levy, O.,
#'Lewis, M., Zettlemoyer, L., & Stoyanov, V. (2019). RoBERTa: A Robustly
#'Optimized BERT Pretraining Approach.
#'\url{https://doi.org/10.48550/arXiv.1907.11692}
#'
#'@references Hugging Face Documentation
#'\url{https://huggingface.co/docs/transformers/model_doc/roberta#transformers.RobertaConfig}
#'
#'@export
create_roberta_model<-function(
    model_dir,
    vocab_raw_texts=NULL,
    vocab_size=30522,
    add_prefix_space=FALSE,
    max_position_embeddings=512,
    hidden_size=768,
    num_hidden_layer=12,
    num_attention_heads=12,
    intermediate_size=3072,
    hidden_act="gelu",
    hidden_dropout_prob=0.1,
    trace=TRUE){

  #argument checking-----------------------------------------------------------
  if(max_position_embeddings>512){
    warning("Due to a quadratic increase in memory requirments it is not
            recommended to set max_position_embeddings above 512.
            If you want to analyse long documents please split your document
            into several chunks with an object of class TextEmbedding Model or
            use another transformer (e.g. longformer).")
  }

  if((hidden_act %in% c("gelu", "relu", "silu","gelu_new"))==FALSE){
    stop("hidden_act must be gelu, relu, silu or gelu_new")
  }

  #Creating a new Tokenizer for Computing Vocabulary
  tok_new<-tok$ByteLevelBPETokenizer()
  tok_new$enable_truncation(max_length = as.integer(max_position_embeddings))
  tok_new$enable_padding(pad_token = "<pad>")
  #Calculating Vocabulary
  if(trace==TRUE){
    print(paste(date(),
                "Start Computing Vocabulary"))
  }
  tok_new$train_from_iterator(
    iterator = vocab_raw_texts,
    vocab_size = as.integer(vocab_size),
    special_tokens=c("<s>","<pad>","</s>","<unk>","<mask>"))
  if(trace==TRUE){
    print(paste(date(),
                "Start Computing Vocabulary - Done"))
  }

  if(dir.exists(model_dir)==FALSE){
    print(paste(date(),"Creating Model Directory"))
    dir.create(model_dir)
  }

  #Saving files
  tok_new$save_model(model_dir)

  if(trace==TRUE){
    print(paste(date(),
                "Creating Tokenizer"))
  }
  tokenizer=transformers$RobertaTokenizerFast(vocab_file = paste0(model_dir,"/","vocab.json"),
                                              merges_file = paste0(model_dir,"/","merges.txt"),
                                              bos_token = "<s>",
                                              eos_token = "</s>",
                                              sep_token = "</s>",
                                              cls_token = "<s>",
                                              unk_token = "<unk>",
                                              pad_token = "<pad>",
                                              mask_token = "<mask>",
                                              add_prefix_space = add_prefix_space)

  if(trace==TRUE){
    print(paste(date(),
                "Creating Tokenizer - Done"))
  }

  configuration=transformers$RobertaConfig(
    vocab_size=as.integer(vocab_size),
    max_position_embeddings=as.integer(max_position_embeddings),
    hidden_size=as.integer(hidden_size),
    num_hidden_layer=as.integer(num_hidden_layer),
    num_attention_heads=as.integer(num_attention_heads),
    intermediate_size=as.integer(intermediate_size),
    hidden_act=hidden_act,
    hidden_dropout_prob=hidden_dropout_prob
  )

  roberta_model=transformers$TFRobertaModel(configuration)

  if(trace==TRUE){
    print(paste(date(),
                "Saving Roberta Model"))
  }
  roberta_model$save_pretrained(model_dir)

  if(trace==TRUE){
    print(paste(date(),
                "Saving Tokenizer Model"))
  }
  tokenizer$save_pretrained(model_dir)
  if(trace==TRUE){
    print(paste(date(),
                "Done"))
  }
}



#'Function for training and fine-tuning a RoBERTa model
#'
#'This function can be used to train or fine-tune a transformer
#'based on BERT architecture with the help of the python libraries 'transformers',
#''datasets', and 'tokenizers'.
#'
#'@param output_dir \code{string} Path to the directory where the final model
#'should be saved. If the directory does not exist, it will be created.
#'@param model_dir_path \code{string} Path to the directory where the original
#'model is stored.
#'@param raw_texts \code{vector} containing the raw texts for training.
#'@param p_mask \code{double} Ratio determining the number of words/tokens for masking.
#'@param val_size \code{double} Ratio determining the amount of token chunks used for
#'validation.
#'@param n_epoch \code{int} Number of epochs for training.
#'@param batch_size \code{int} Size of batches.
#'@param chunk_size \code{int} Size of every chunk for training.
#'@param learning_rate \code{bool} Learning rate for adam optimizer.
#'@param n_workers \code{int} Number of workers.
#'@param multi_process \code{bool} \code{TRUE} if multiple processes should be activated.
#'@param trace \code{bool} \code{TRUE} if information on the progress should be printed
#'to the console.
#'@return This function does not return an object. Instead the trained or fine-tuned
#'model is saved to disk.
#'@note Pre-Trained models which can be fine-tuned with this function are available
#'at \url{https://huggingface.co/}. New models can be created via the function
#'\link{create_roberta_model}.
#'@note Training of this model makes use of dynamic masking.
#'
#'@references
#'Liu, Y., Ott, M., Goyal, N., Du, J., Joshi, M., Chen, D., Levy, O.,
#'Lewis, M., Zettlemoyer, L., & Stoyanov, V. (2019). RoBERTa: A Robustly
#'Optimized BERT Pretraining Approach.
#'\url{https://doi.org/10.48550/arXiv.1907.11692}
#'
#'@references Hugging Face Documentation
#'\url{https://huggingface.co/docs/transformers/model_doc/roberta#transformers.RobertaConfig}
#'
#'
#'@export
train_tune_roberta_model=function(output_dir,
                               model_dir_path,
                               raw_texts,
                               p_mask=0.15,
                               val_size=0.1,
                               n_epoch=1,
                               batch_size=12,
                               chunk_size=250,
                               learning_rate=3e-2,
                               n_workers=1,
                               multi_process=FALSE,
                               trace=TRUE){

  mlm_model=transformers$TFRobertaForMaskedLM$from_pretrained(model_dir_path)
  tokenizer<-transformers$RobertaTokenizerFast$from_pretrained(model_dir_path)

  #argument checking------------------------------------------------------------
  if(chunk_size>(mlm_model$config$max_position_embeddings)){
    stop(paste("Chunk size is",chunk_size,". This value is not allowed to exceed",
               mlm_model$config$max_position_embeddings))
  }
  if(chunk_size<3){
    stop("Chunk size must be at least 3.")
  }

  #adjust chunk size. To elements are needed for begin and end of sequence
  chunk_size=chunk_size-2

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

  print(paste(date(),"Creating Text Chunks"))
  prepared_texts_chunks<-quanteda::tokens_chunk(
    x=prepared_texts,
    size=chunk_size,
    overlap = 0,
    use_docvars = FALSE)

  check_chunks_length=(quanteda::ntoken(prepared_texts_chunks)==chunk_size)

  prepared_texts_chunks<-quanteda::tokens_subset(
    x=prepared_texts_chunks,
    subset = check_chunks_length)

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

  print(paste(date(),"Using Token Masking"))
  data_collator=transformers$DataCollatorForLanguageModeling(
    tokenizer = tokenizer,
    mlm = TRUE,
    mlm_probability = p_mask)

  tokenized_dataset=tokenized_dataset$train_test_split(test_size=val_size)

  tf_train_dataset=mlm_model$prepare_tf_dataset(
    dataset = tokenized_dataset$train,
    batch_size = as.integer(batch_size),
    collate_fn = data_collator,
    shuffle = TRUE)
  tf_test_dataset=mlm_model$prepare_tf_dataset(
    dataset = tokenized_dataset$test,
    batch_size = as.integer(batch_size),
    collate_fn = data_collator,
    shuffle = TRUE)

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
    save_weights_only= TRUE)

  print(paste(date(),"Compile Model"))
  mlm_model$compile(optimizer=adam(learning_rate))

  #Clear session to provide enough resources for computations
  tf$keras$backend$clear_session()

  print(paste(date(),"Start Fine Tuning"))
  mlm_model$fit(x=tf_train_dataset,
                validation_data=tf_test_dataset,
                epochs=as.integer(n_epoch),
                workers=as.integer(n_workers),
                use_multiprocessing=multi_process,
                callbacks=list(callback_checkpoint))

  print(paste(date(),"Load Weights From Best Checkpoint"))
  mlm_model$load_weights(paste0(output_dir,"/checkpoints/"))

  print(paste(date(),"Saving RoBERTa Model"))
  mlm_model$save_pretrained(save_directory=output_dir)

  print(paste(date(),"Saving Tokenizer"))
  tokenizer$save_pretrained(output_dir)

  print(paste(date(),"Done"))

}

