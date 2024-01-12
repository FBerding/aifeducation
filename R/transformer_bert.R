#'Function for creating a new transformer based on BERT
#'
#'This function creates a transformer configuration based on the BERT base architecture
#'and a vocabulary based on WordPiece by using
#'the python libraries 'transformers' and 'tokenizers'.
#'
#'@param ml_framework \code{string} Framework to use for training and inference.
#'\code{ml_framework="tensorflow"} for 'tensorflow' and \code{ml_framework="pytorch"}
#'for 'pytorch'.
#'@param model_dir \code{string} Path to the directory where the model should be saved.
#'@param vocab_raw_texts \code{vector} containing the raw texts for creating the
#'vocabulary.
#'@param vocab_size \code{int} Size of the vocabulary.
#'@param vocab_do_lower_case \code{bool} \code{TRUE} if all words/tokens should be lower case.
#'@param max_position_embeddings \code{int} Number of maximal position embeddings. This parameter
#'also determines the maximum length of a sequence which can be processed with the model.
#'@param hidden_size \code{int} Number of neurons in each layer. This parameter determines the
#'dimensionality of the resulting text embedding.
#'@param num_hidden_layer \code{int} Number of hidden layers.
#'@param num_attention_heads \code{int} Number of attention heads.
#'@param intermediate_size \code{int} Number of neurons in the intermediate layer of
#'the attention mechanism.
#'@param hidden_act \code{string} name of the activation function.
#'@param hidden_dropout_prob \code{double} Ratio of dropout.
#'@param attention_probs_dropout_prob \code{double} Ratio of dropout for attention
#'probabilities.
#'
#'@param sustain_track \code{bool} If \code{TRUE} energy consumption is tracked
#'during training via the python library codecarbon.
#'@param sustain_iso_code \code{string} ISO code (Alpha-3-Code) for the country. This variable
#'must be set if sustainability should be tracked. A list can be found on
#'Wikipedia: \url{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}.
#'@param sustain_region Region within a country. Only available for USA and
#'Canada See the documentation of codecarbon for more information.
#'\url{https://mlco2.github.io/codecarbon/parameters.html}
#'@param sustain_interval \code{integer} Interval in seconds for measuring power
#'usage.
#'
#'@param trace \code{bool} \code{TRUE} if information about the progress should be
#'printed to the console.
#'@return This function does not return an object. Instead the configuration
#'and the vocabulary of the new model are saved on disk.
#'@note To train the model, pass the directory of the model to the function
#'\link{train_tune_bert_model}.
#'@note This models uses a WordPiece Tokenizer like BERT and can be trained with
#'whole word masking. Transformer library may show a warning which can be ignored.
#'
#'@references
#'Devlin, J., Chang, M.‑W., Lee, K., & Toutanova, K. (2019). BERT:
#'Pre-training of Deep Bidirectional Transformers for Language
#'Understanding. In J. Burstein, C. Doran, & T. Solorio (Eds.),
#'Proceedings of the 2019 Conference of the North (pp. 4171--4186).
#'Association for Computational Linguistics.
#'\doi{10.18653/v1/N19-1423}
#'
#'@references Hugging Face documentation
#'\url{https://huggingface.co/docs/transformers/model_doc/bert#transformers.TFBertForMaskedLM}
#'
#'@importFrom reticulate py_module_available
#'@family Transformer
#'@export
create_bert_model<-function(
    ml_framework=aifeducation_config$get_framework(),
    model_dir,
    vocab_raw_texts=NULL,
    vocab_size=30522,
    vocab_do_lower_case=FALSE,
    max_position_embeddings=512,
    hidden_size=768,
    num_hidden_layer=12,
    num_attention_heads=12,
    intermediate_size=3072,
    hidden_act="gelu",
    hidden_dropout_prob=0.1,
    attention_probs_dropout_prob=0.1,
    sustain_track=TRUE,
    sustain_iso_code=NULL,
    sustain_region=NULL,
    sustain_interval=15,
    trace=TRUE){

  #Set Shiny Progress Tracking
  pgr_max=10
  update_aifeducation_progress_bar(value = 0,
                                   total = pgr_max,
                                   title = "BERT Model")

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

  if((ml_framework %in%c("pytorch","tensorflow","not_specified"))==FALSE){
    stop("ml_framework must be 'tensorflow' or 'pytorch'.")
  }

  if(ml_framework=="not_specified"){
    stop("The global machine learning framework is not set. Please use
             aifeducation_config$set_global_ml_backend() directly after loading
             the library to set the global framework. ")
  }

  if(sustain_track==TRUE){
    if(is.null(sustain_iso_code)==TRUE){
      stop("Sustainability tracking is activated but iso code for the
               country is missing. Add iso code or deactivate tracking.")
    }
  }

  update_aifeducation_progress_bar(value = 1,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Start Sustainability Tracking-----------------------------------------------
  if(sustain_track==TRUE){
    if(trace==TRUE){
      message(paste(date(),
                "Start Sustainability Tracking"))
    }
    sustainability_tracker<-codecarbon$OfflineEmissionsTracker(
      country_iso_code=sustain_iso_code,
      region=sustain_region,
      tracking_mode="machine",
      log_level="warning",
      measure_power_secs=sustain_interval,
      save_to_file=FALSE,
      save_to_api=FALSE
    )
    sustainability_tracker$start()
  } else {
    if(trace==TRUE){
      message(paste(date(),
                "Start without Sustainability Tracking"))
    }
  }

  update_aifeducation_progress_bar(value = 2,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Creating a new Tokenizer for Computing Vocabulary---------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Creating Tokenizer Draft"))
  }

  special_tokens=c("[CLS]","[SEP]","[PAD]","[UNK]","[MASK]")
  tok_new<-tok$Tokenizer(tok$models$WordPiece())
  tok_new$normalizer=tok$normalizers$BertNormalizer(
    lowercase=vocab_do_lower_case,
    clean_text = TRUE,
    handle_chinese_chars = TRUE,
    strip_accents = vocab_do_lower_case)
  tok_new$pre_tokenizer=tok$pre_tokenizers$BertPreTokenizer()
  tok_new$post_processor<-tok$processors$BertProcessing(
    sep=reticulate::tuple(list("[SEP]",as.integer(1))),
    cls=reticulate::tuple(list("[CLS]",as.integer(0)))
  )

  tok_new$decode=tok$decoders$WordPiece()

  trainer<-tok$trainers$WordPieceTrainer(
    vocab_size=as.integer(vocab_size),
    special_tokens = special_tokens,
    show_progress=trace)

  update_aifeducation_progress_bar(value = 3,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Calculating Vocabulary------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Start Computing Vocabulary"))
  }
  tok_new$train_from_iterator(vocab_raw_texts,trainer=trainer)
  if(trace==TRUE){
    message(paste(date(),
              "Start Computing Vocabulary - Done"))
  }

  update_aifeducation_progress_bar(value = 4,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Saving Tokenizer Draft------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Saving Draft"))
  }

  if(dir.exists(model_dir)==FALSE){
    message(paste(date(),"Creating Directory"))
    dir.create(model_dir)
  }

  write(c(special_tokens,names(tok_new$get_vocab())),
        file=paste0(model_dir,"/","vocab.txt"))

  update_aifeducation_progress_bar(value = 5,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Final Tokenizer-------------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Creating Tokenizer"))
  }

  tokenizer=transformers$PreTrainedTokenizerFast(
    tokenizer_object=tok_new,
    unk_token="[UNK]",
    sep_token="[SEP]",
    pad_token="[PAD]",
    cls_token="[CLS]",
    mask_token="[MASK]",
    bos_token = "[CLS]",
    eos_token = "[SEP]")

  if(trace==TRUE){
    message(paste(date(),
              "Creating Tokenizer - Done"))
  }

  update_aifeducation_progress_bar(value = 6,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Creating Transformer Model--------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Creating Transformer Model"))
  }

  configuration=transformers$BertConfig(
    vocab_size=as.integer(length(tokenizer$get_vocab())),
    max_position_embeddings=as.integer(max_position_embeddings),
    hidden_size=as.integer(hidden_size),
    num_hidden_layer=as.integer(num_hidden_layer),
    num_attention_heads=as.integer(num_attention_heads),
    intermediate_size=as.integer(intermediate_size),
    hidden_act=hidden_act,
    hidden_dropout_prob=hidden_dropout_prob,
    attention_probs_dropout_prob=attention_probs_dropout_prob,
    pad_token_id = tokenizer$pad_token_id,
    type_vocab_size =as.integer(2),
    initializer_range=0.02,
    layer_norm_eps=1e-12,
    position_embedding_type="absolute",
    is_decoder=FALSE,
    use_cache=TRUE
  )

  if(ml_framework=="tensorflow"){
    bert_model=transformers$TFBertModel(configuration)
  } else {
    bert_model=transformers$BertModel(configuration)
  }

  update_aifeducation_progress_bar(value = 7,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Saving Model----------------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Saving Bert Model"))
  }
  if(ml_framework=="tensorflow"){
    bert_model$save_pretrained(save_directory=model_dir)
  } else {
    bert_model$save_pretrained(save_directory=model_dir,
                               safe_serilization=reticulate::py_module_available("safetensors"))
  }



  update_aifeducation_progress_bar(value = 8,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Saving Tokenizer------------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Saving Tokenizer Model"))
  }
  tokenizer$save_pretrained(model_dir)

  update_aifeducation_progress_bar(value = 9,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Stop Sustainability Tracking if requested------------------------------------
  if(sustain_track==TRUE){
    sustainability_tracker$stop()
    sustainability_data<-summarize_tracked_sustainability(sustainability_tracker)
    sustain_matrix=t(as.matrix(unlist(sustainability_data)))

    if(trace==TRUE){
      message(paste(date(),
                "Saving Sustainability Data"))
    }

    write.csv(
      x=sustain_matrix,
      file=paste0(model_dir,"/sustainability.csv"),
      row.names = FALSE)
  }

  update_aifeducation_progress_bar(value = 10,
                                   total = pgr_max,
                                   title = "BERT Model")

  #Finish----------------------------------------------------------------------

  if(trace==TRUE){
    message(paste(date(),
              "Done"))
  }

}



#'Function for training and fine-tuning a BERT model
#'
#'This function can be used to train or fine-tune a transformer
#'based on BERT architecture with the help of the python libraries 'transformers',
#''datasets', and 'tokenizers'.
#'
#'@param ml_framework \code{string} Framework to use for training and inference.
#'\code{ml_framework="tensorflow"} for 'tensorflow' and \code{ml_framework="pytorch"}
#'for 'pytorch'.
#'@param output_dir \code{string} Path to the directory where the final model
#'should be saved. If the directory does not exist, it will be created.
#'@param model_dir_path \code{string} Path to the directory where the original
#'model is stored.
#'@param raw_texts \code{vector} containing the raw texts for training.
#'@param p_mask \code{double} Ratio determining the number of words/tokens for masking.
#'@param whole_word \code{bool} \code{TRUE} if whole word masking should be applied.
#'If \code{FALSE} token masking is used.
#'@param val_size \code{double} Ratio determining the amount of token chunks used for
#'validation.
#'@param n_epoch \code{int} Number of epochs for training.
#'@param batch_size \code{int} Size of batches.
#'@param chunk_size \code{int} Size of every chunk for training.
#'@param full_sequences_only \code{bool} \code{TRUE} for using only chunks
#'with a sequence length equal to \code{chunk_size}.
#'@param min_seq_len \code{int} Only relevant if \code{full_sequences_only=FALSE}.
#'Value determines the minimal sequence length for inclusion in training process.
#'@param learning_rate \code{double} Learning rate for adam optimizer.
#'@param n_workers \code{int} Number of workers. Only relevant if \code{ml_framework="tensorflow"}.
#'@param multi_process \code{bool} \code{TRUE} if multiple processes should be activated.
#'Only relevant if \code{ml_framework="tensorflow"}.
#'
#'@param sustain_track \code{bool} If \code{TRUE} energy consumption is tracked
#'during training via the python library codecarbon.
#'@param sustain_iso_code \code{string} ISO code (Alpha-3-Code) for the country. This variable
#'must be set if sustainability should be tracked. A list can be found on
#'Wikipedia: \url{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}.
#'@param sustain_region Region within a country. Only available for USA and
#'Canada See the documentation of codecarbon for more information.
#'\url{https://mlco2.github.io/codecarbon/parameters.html}
#'@param sustain_interval \code{integer} Interval in seconds for measuring power
#'usage.
#'
#'@param trace \code{bool} \code{TRUE} if information on the progress should be printed
#'to the console.
#'@param keras_trace \code{int} \code{keras_trace=0} does not print any
#'information about the training process from keras on the console.
#'\code{keras_trace=1} prints a progress bar. \code{keras_trace=2} prints
#'one line of information for every epoch. Only relevant if \code{ml_framework="tensorflow"}.
#'
#'@return This function does not return an object. Instead the trained or fine-tuned
#'model is saved to disk.
#'@note This models uses a WordPiece Tokenizer like BERT and can be trained with
#'whole word masking. Transformer library may show a warning which can be ignored.
#'
#'@note Pre-Trained models which can be fine-tuned with this function are available
#'at \url{https://huggingface.co/}.
#'@note New models can be created via the function \link{create_bert_model}.
#'@note Training of the model makes use of dynamic masking in contrast to the
#'original paper where static masking was applied.
#'@references
#'Devlin, J., Chang, M.‑W., Lee, K., & Toutanova, K. (2019). BERT:
#'Pre-training of Deep Bidirectional Transformers for Language
#'Understanding. In J. Burstein, C. Doran, & T. Solorio (Eds.),
#'Proceedings of the 2019 Conference of the North (pp. 4171--4186).
#'Association for Computational Linguistics.
#'\doi{10.18653/v1/N19-1423}
#'
#'@references Hugging Face documentation
#'\url{https://huggingface.co/docs/transformers/model_doc/bert#transformers.TFBertForMaskedLM}
#'
#'@family Transformer
#'
#'@importFrom utils write.csv
#'@importFrom utils read.csv
#'@importFrom reticulate py_module_available
#'
#'@export
train_tune_bert_model=function(ml_framework=aifeducation_config$get_framework(),
                               output_dir,
                               model_dir_path,
                               raw_texts,
                               p_mask=0.15,
                               whole_word=TRUE,
                               val_size=0.1,
                               n_epoch=1,
                               batch_size=12,
                               chunk_size=250,
                               full_sequences_only=FALSE,
                               min_seq_len=50,
                               learning_rate=3e-3,
                               n_workers=1,
                               multi_process=FALSE,
                               sustain_track=TRUE,
                               sustain_iso_code=NULL,
                               sustain_region=NULL,
                               sustain_interval=15,
                               trace=TRUE,
                               keras_trace=1,
                               pytorch_trace=1){

  #Set Shiny Progress Tracking
  pgr_max=10
  update_aifeducation_progress_bar(value = 0, total = pgr_max, title = "BERT Model")

  #argument checking-----------------------------------------------------------
  if((ml_framework %in%c("pytorch","tensorflow","not_specified"))==FALSE){
    stop("ml_framework must be 'tensorflow' or 'pytorch'.")
  }

  if(ml_framework=="not_specified"){
    stop("The global machine learning framework is not set. Please use
             aifeducation_config$set_global_ml_backend() directly after loading
             the library to set the global framework. ")
  }

  if(ml_framework=="tensorflow"){
    if(file.exists(paste0(model_dir_path,"/tf_model.h5"))){
      from_pt=FALSE
    } else if (file.exists(paste0(model_dir_path,"/pytorch_model.bin"))){
      from_pt=TRUE
    } else {
      stop("Directory does not contain a tf_model.h5 or pytorch_model.bin file.")
    }
  } else {
    if(file.exists(paste0(model_dir_path,"/pytorch_model.bin"))){
      from_tf=FALSE
    } else if (file.exists(paste0(model_dir_path,"/tf_model.h5"))){
      from_tf=TRUE
    } else {
      stop("Directory does not contain a tf_model.h5 or pytorch_model.bin file.")
    }
  }

  if(sustain_track==TRUE){
    if(is.null(sustain_iso_code)==TRUE){
      stop("Sustainability tracking is activated but iso code for the
               country is missing. Add iso code or deactivate tracking.")
    }
  }

  update_aifeducation_progress_bar(value = 1, total = pgr_max, title = "BERT Model")

  #Start Sustainability Tracking-----------------------------------------------
  if(sustain_track==TRUE){
    if(trace==TRUE){
      message(paste(date(),
                "Start Sustainability Tracking"))
    }
    sustainability_tracker<-codecarbon$OfflineEmissionsTracker(
      country_iso_code=sustain_iso_code,
      region=sustain_region,
      tracking_mode="machine",
      log_level="warning",
      measure_power_secs=sustain_interval,
      save_to_file=FALSE,
      save_to_api=FALSE
    )
    sustainability_tracker$start()
  } else {
    if(trace==TRUE){
      message(paste(date(),
                "Start without Sustainability Tracking"))
    }
  }

  update_aifeducation_progress_bar(value = 2, total = pgr_max, title = "BERT Model")

  #Loading existing model------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),
              "Loading Existing Model"))
  }

  if(ml_framework=="tensorflow"){
    mlm_model=transformers$TFBertForMaskedLM$from_pretrained(model_dir_path,from_pt=from_pt)
  } else {
    mlm_model=transformers$BertForMaskedLM$from_pretrained(model_dir_path,from_tf=from_tf)
  }

  tokenizer<-transformers$AutoTokenizer$from_pretrained(model_dir_path)

  update_aifeducation_progress_bar(value = 3, total = pgr_max, title = "BERT Model")

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

  update_aifeducation_progress_bar(value = 4, total = pgr_max, title = "BERT Model")

  #creating chunks of sequences------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),"Creating Chunks of Sequences for Training"))
  }

  if(full_sequences_only==FALSE){
    tokenized_texts=tokenizer(raw_texts,
                              truncation =TRUE,
                              padding= FALSE,
                              max_length=as.integer(chunk_size),
                              return_overflowing_tokens = TRUE,
                              return_length = TRUE,
                              return_special_tokens_mask=TRUE,
                              return_offsets_mapping = FALSE,
                              return_attention_mask = TRUE,
                              return_tensors="np")
    tokenized_dataset=datasets$Dataset$from_dict(tokenized_texts)
    relevant_indices=which(tokenized_dataset["length"]<=chunk_size & tokenized_dataset["length"]>=min_seq_len)


    tokenized_texts=tokenizer(raw_texts,
                              truncation =TRUE,
                              padding= TRUE,
                              max_length=as.integer(chunk_size),
                              return_overflowing_tokens = TRUE,
                              return_length = TRUE,
                              return_special_tokens_mask=TRUE,
                              return_offsets_mapping = FALSE,
                              return_attention_mask = TRUE,
                              return_tensors="np")
    tokenized_dataset=datasets$Dataset$from_dict(tokenized_texts)
    tokenized_dataset=tokenized_dataset$select(as.integer(relevant_indices-1))

  } else {
    tokenized_texts=tokenizer(raw_texts,
                              truncation =TRUE,
                              padding= FALSE,
                              max_length=as.integer(chunk_size),
                              return_overflowing_tokens = TRUE,
                              return_length = TRUE,
                              return_special_tokens_mask=TRUE,
                              return_offsets_mapping = FALSE,
                              return_attention_mask = TRUE,
                              return_tensors="np")
    tokenized_dataset=datasets$Dataset$from_dict(tokenized_texts)
    relevant_indices=which(tokenized_dataset["length"]==chunk_size)
    tokenized_dataset=tokenized_dataset$select(as.integer(relevant_indices-1))
  }

  n_chunks=tokenized_dataset$num_rows

  if(trace==TRUE){
    message(paste(date(),n_chunks,"Chunks Created"))
  }

  update_aifeducation_progress_bar(value = 5, total = pgr_max, title = "BERT Model")

  #Seeting up DataCollator and Dataset------------------------------------------

  if(dir.exists(paste0(output_dir))==FALSE){
    if(trace==TRUE){
      message(paste(date(),"Creating Output Directory"))
    }
    dir.create(paste0(output_dir))
  }

  if(dir.exists(paste0(output_dir,"/checkpoints"))==FALSE){
    if(trace==TRUE){
      message(paste(date(),"Creating Checkpoint Directory"))
    }
    dir.create(paste0(output_dir,"/checkpoints"))
  }

  if(ml_framework=="tensorflow"){

    if(whole_word==TRUE){
      if(trace==TRUE){
        message(paste(date(),"Using Whole Word Masking"))
      }
      word_ids=matrix(nrow = n_chunks,
                      ncol= chunk_size)

      if(trace==TRUE){
        message(paste(date(),"Adding Word Ids"))
      }

      for(i in 0:(n_chunks-1)){
        tmp=as.vector(unlist(tokenized_texts$word_ids(as.integer(i))))
        word_ids[i+1,2:(length(tmp)+1)]=tmp
      }

      word_ids<-reticulate::dict("word_ids"=word_ids)
      word_ids<-datasets$Dataset$from_dict(word_ids)
      tokenized_dataset=tokenized_dataset$add_column(name="word_ids",column=word_ids)
      data_collator=transformers$DataCollatorForWholeWordMask(
        tokenizer = tokenizer,
        mlm = TRUE,
        mlm_probability = p_mask,
        return_tensors = "tf")
    } else {
      if(trace==TRUE){
        message(paste(date(),"Using Token Masking"))
      }
      data_collator=transformers$DataCollatorForLanguageModeling(
        tokenizer = tokenizer,
        mlm = TRUE,
        mlm_probability = p_mask,
        return_tensors = "tf"
      )
    }

    tokenized_dataset=tokenized_dataset$add_column(name="labels",column=tokenized_dataset["input_ids"])
    tokenized_dataset$set_format(type="tensorflow")

    tokenized_dataset=tokenized_dataset$train_test_split(test_size=val_size)

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

    if(trace==TRUE){
      message(paste(date(),"Preparing Training of the Model"))
    }
    adam<-tf$keras$optimizers$Adam


    callback_checkpoint=tf$keras$callbacks$ModelCheckpoint(
      filepath = paste0(output_dir,"/checkpoints/best_weights.h5"),
      monitor="val_loss",
      verbose = as.integer(min(keras_trace,1)),
      mode="auto",
      save_best_only=TRUE,
      save_freq="epoch",
      save_weights_only= TRUE
    )

    #Add Callback if Shiny App is running
    if(require("shiny") & require("shinyWidgets")){
      if(shiny::isRunning()){
        shiny_app_active=TRUE
        reticulate::py_run_file(system.file("python/keras_callbacks.py",
                                            package = "aifeducation"))
        callback_checkpoint=list(callback_checkpoint,py$ReportAiforeducationShiny())
      }
    }

    if(trace==TRUE){
      message(paste(date(),"Compile Model"))
    }
    mlm_model$compile(optimizer=adam(learning_rate),
                      loss="auto")

    #Clear session to provide enough resources for computations
    tf$keras$backend$clear_session()

    update_aifeducation_progress_bar(value = 6, total = pgr_max, title = "BERT Model")

    #Start Training------------------------------------------------------------
    if(trace==TRUE){
      message(paste(date(),"Start Fine Tuning"))
    }
    mlm_model$fit(x=tf_train_dataset,
                  validation_data=tf_test_dataset,
                  epochs=as.integer(n_epoch),
                  workers=as.integer(n_workers),
                  use_multiprocessing=multi_process,
                  callbacks=list(callback_checkpoint),
                  verbose=as.integer(keras_trace))

    if(trace==TRUE){
      message(paste(date(),"Load Weights From Best Checkpoint"))
    }
    mlm_model$load_weights(paste0(output_dir,"/checkpoints/best_weights.h5"))
  } else {

    if(whole_word==TRUE){
      if(trace==TRUE){
        message(paste(date(),"Using Whole Word Masking"))
      }
      word_ids=matrix(nrow = n_chunks,
                      ncol= chunk_size)

      if(trace==TRUE){
        message(paste(date(),"Adding Word Ids"))
      }

      for(i in 0:(n_chunks-1)){
        tmp=as.vector(unlist(tokenized_texts$word_ids(as.integer(i))))
        word_ids[i+1,2:(length(tmp)+1)]=tmp
      }

      word_ids<-reticulate::dict("word_ids"=word_ids)
      word_ids<-datasets$Dataset$from_dict(word_ids)
      tokenized_dataset=tokenized_dataset$add_column(name="word_ids",column=word_ids)
      data_collator=transformers$DataCollatorForWholeWordMask(
        tokenizer = tokenizer,
        mlm = TRUE,
        mlm_probability = p_mask,
        return_tensors = "pt")
    } else {
      if(trace==TRUE){
        message(paste(date(),"Using Token Masking"))
      }
      data_collator=transformers$DataCollatorForLanguageModeling(
        tokenizer = tokenizer,
        mlm = TRUE,
        mlm_probability = p_mask,
        return_tensors = "pt"
      )
    }

    tokenized_dataset=tokenized_dataset$add_column(name="labels",column=tokenized_dataset["input_ids"])
    tokenized_dataset$set_format(type="torch")

    tokenized_dataset=tokenized_dataset$train_test_split(test_size=val_size)

    training_args=transformers$TrainingArguments(
      output_dir = paste0(output_dir,"/checkpoints"),
      overwrite_output_dir=TRUE,
      evaluation_strategy = "epoch",
      num_train_epochs = as.integer(n_epoch),
      logging_strategy="epoch",
      save_strategy ="epoch",
      save_total_limit=as.integer(1),
      load_best_model_at_end=TRUE,
      optim = "adamw_torch",
      learning_rate = learning_rate,
      per_device_train_batch_size = as.integer(batch_size),
      per_device_eval_batch_size = as.integer(batch_size),
      save_safetensors=TRUE,
      auto_find_batch_size=FALSE,
      report_to="none",
      log_level="error",
      disable_tqdm=!pytorch_trace
    )

    trainer=transformers$Trainer(
      model=mlm_model,
      train_dataset = tokenized_dataset$train,
      eval_dataset = tokenized_dataset$test,
      args = training_args,
      data_collator = data_collator,
      tokenizer = tokenizer
    )
    trainer$remove_callback(transformers$integrations$CodeCarbonCallback)

    #Add Callback if Shiny App is running
    if(require("shiny") & require("shinyWidgets")){
      if(shiny::isRunning()){
        shiny_app_active=TRUE
        reticulate::py_run_file(system.file("python/pytorch_transformer_callbacks.py",
                                            package = "aifeducation"))
        trainer$add_callback(py$ReportAiforeducationShiny_PT())
      }
    }

    update_aifeducation_progress_bar(value = 6, total = pgr_max, title = "BERT Model")

    #Start Training------------------------------------------------------------
    if(trace==TRUE){
      message(paste(date(),"Start Fine Tuning"))
    }
    trainer$train()
  }

  update_aifeducation_progress_bar(value = 7, total = pgr_max, title = "BERT Model")

  #Saving Model----------------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),"Saving Bert Model"))
  }
  if(ml_framework=="tensorflow"){
    mlm_model$save_pretrained(save_directory=output_dir)
  } else {
    mlm_model$save_pretrained(save_directory=output_dir,
                               safe_serilization=reticulate::py_module_available("safetensors"))
  }

  update_aifeducation_progress_bar(value = 8, total = pgr_max, title = "BERT Model")

  #Saving Tokenizer-------------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),"Saving Tokenizer"))
  }
  tokenizer$save_pretrained(output_dir)

  update_aifeducation_progress_bar(value = 9, total = pgr_max, title = "BERT Model")

  #Stop Sustainability Tracking if requested------------------------------------
  if(sustain_track==TRUE){
    sustainability_tracker$stop()
    sustainability_data<-summarize_tracked_sustainability(sustainability_tracker)
    sustain_matrix=t(as.matrix(unlist(sustainability_data)))

    if(trace==TRUE){
      message(paste(date(),
                "Saving Sustainability Data"))
    }

    sustainability_data_file_path_input=paste0(model_dir_path,"/sustainability.csv")
    sustainability_data_file_path=paste0(output_dir,"/sustainability.csv")

    if(file.exists(sustainability_data_file_path_input)){
      sustainability_data_chronic<-as.matrix(read.csv(sustainability_data_file_path_input))
      sustainability_data_chronic<-rbind(
        sustainability_data_chronic,
        sustain_matrix
      )

      write.csv(
        x=sustainability_data_chronic,
        file=sustainability_data_file_path,
        row.names = FALSE)

    } else {
      write.csv(
        x=sustain_matrix,
        file=sustainability_data_file_path,
        row.names = FALSE)
    }
  }

  update_aifeducation_progress_bar(value = 10, total = pgr_max, title = "BERT Model")

  #Finish----------------------------------------------------------------------
  if(trace==TRUE){
    message(paste(date(),"Done"))
  }
}

