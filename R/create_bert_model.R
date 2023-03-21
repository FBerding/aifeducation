#'Function for creating a new transformer based on BERT
#'
#'Insert Description
#'
#'@export
create_bert_model<-function(
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
    hidden_dropput_prob=0.1,
    trace=TRUE){

  transformer<-reticulate::import("transformers")
  datasets<-reticulate::import("datasets")
  tok<-reticulate::import("tokenizers")

  #Creating a new Tokenizer for Computing Vocabulary
  tok_new<-tok$Tokenizer(tok$models$WordPiece())
  tok_new$normalizer=tok$normalizers$BertNormalizer(lowercase=vocab_do_lower_case)
  tok_new$pre_tokenizer=tok$pre_tokenizers$BertPreTokenizer()
  tok_new$decode=tok$decoders$WordPiece()
  trainer<-tok$trainers$WordPieceTrainer(
    vocab_size=as.integer(vocab_size),
    show_progress=trace)

  #Calculating Vocabulary
  if(trace==TRUE){
    print(paste(date(),
                "Start Computing Vocabulary"))
  }
  tok_new$train_from_iterator(vocab_raw_texts,trainer=trainer)
  if(trace==TRUE){
    print(paste(date(),
                "Start Computing Vocabulary - Done"))
  }

  special_tokens=c("[PAD]","[CLS]","[SEP]","[UNK]","[MASK]")

  if(dir.exists(model_dir)==FALSE){
    print(paste(date(),"Creating Checkpoint Directory"))
    dir.create(model_dir)
  }

  write(c(special_tokens,names(tok_new$get_vocab)),
        file=paste0(model_dir,"/","vocab.txt"))

  if(trace==TRUE){
    print(paste(date(),
                "Creating Tokenizer"))
  }
  tokenizer=transformer$BertTokenizerFast(vocab_file = paste0(model_dir,"/","vocab.txt"),
                                      do_lower_case=vocab_do_lower_case)

  if(trace==TRUE){
    print(paste(date(),
                "Creating Tokenizer - Done"))
  }

  configuration=transformer$BertConfig(
    vocab_size=as.integer(vocab_size),
    max_position_embeddings=as.integer(max_position_embeddings),
    hidden_size=as.integer(hidden_size),
    num_hidden_layer=as.integer(num_hidden_layer),
    num_attention_heads=as.integer(num_attention_heads),
    intermediate_size=as.integer(intermediate_size),
    hidden_act=hidden_act,
    hidden_dropput_prob=hidden_dropput_prob
  )

  bert_model=transformer$TFBertModel(configuration)

  if(trace==TRUE){
    print(paste(date(),
                "Saving Bert Model"))
  }
  bert_model$save_pretrained(model_dir)

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
