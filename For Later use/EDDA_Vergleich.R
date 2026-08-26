devtools::load_all()
prepare_session()

data_path="Trial/Edda_Training"
text_data=load(file.path(data_path,"dataset_texts.rda"))


codings=text_data=read.csv(file.path(data_path,"dataset_codings.csv"))

category="process_orientation"
cat_levels=names(table(codings[category]))
cat_codings=factor(codings[,category],levels=cat_levels)
names(cat_codings)=codings$ID

cat_codings=na.omit(cat_codings)

condition=text_data_frame$id%in%names(cat_codings)
text_subset=subset(text_data_frame,condition)
datasets_texts=LargeDataSetForText$new()
datasets_texts$add_from_data.frame(text_subset)



base_model_eurobert <- BaseModelEuroBert$new()
base_model_eurobert$create_from_hf(
  model_dir = "vignettes/examples/EuroBERT-210m",
  tokenizer_dir = "vignettes/examples/EuroBERT-210m"
)
total_max_seq_len=512L
total_max_seq_len

seq_len=total_max_seq_len
overlap=total_max_seq_len/4

chunk_quantile = base_model_eurobert$Tokenizer$calc_quantiles(
  text_dataset = datasets_texts,
  batch_size = 32L,
  seq_len_tokens = seq_len,
  token_overlap=overlap,
  trace=TRUE
)
print(chunk_quantile)

chunks=chunk_quantile["99%"]
chunks=4
num_layers=base_model_eurobert$get_n_layers()
num_layers
eurobert_min_layer=floor(0.5*num_layers)
eurobert_min_layer

eurobert_max_layer=ceiling(2/3*num_layers)
eurobert_max_layer

tem <- TextEmbeddingModel$new()
tem$configure(
  model_label = "Text Embedding via EuroBert - 210m",
  model_language = "english",
  max_length = seq_len,
  chunks = chunks,
  overlap = overlap,
  emb_layer_min = eurobert_min_layer,
  emb_layer_max = eurobert_max_layer,
  emb_pool_type = "Average",
  base_model = base_model_eurobert,
  emb_insert_mask_tokens=0.15
)
embeddings <- tem$embed_large(
  text_dataset = datasets_texts,
  trace = TRUE
)



devtools::load_all()
load_all_py_scripts()
batch_size=64
losses=c("FocalLoss")
results=list()

feat_size=192
loss="AEMLoss"
classifier <- TEClassifierParallelReferencePoint$new()
classifier$configure(
  label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = embeddings,
  feature_extractor = NULL,
  target_levels = cat_levels,
  skip_connection_type = "ResidualGate",
  metric_type = "CosineDistance",
  cls_input_normalize="BatchNorm",
  shared_feat_layer = FALSE,
  feat_act_fct = "Tanh",
  feat_size = feat_size,
  feat_bias = TRUE,
  feat_dropout = 0.00,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 3,
  ng_conv_bias = FALSE,
  ng_conv_dropout = .10,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "GELU",
  dense_n_layers = 1,
  dense_dropout = .20,
  dense_bias = FALSE,
  dense_parametrizations = "None",
  dense_normalization_type = "PowerNorm",
  dense_residual_type = "ResidualGate",
  rec_act_fct = "Tanh",
  rec_n_layers = 0,
  rec_type = "GRU",
  rec_bidirectional = TRUE,
  rec_dropout = .20,
  rec_bias = FALSE,
  rec_parametrizations = "None",
  rec_normalization_type = "PowerNorm",
  rec_residual_type = "ResidualGate",
  tf_act_fct = "SwiGLU",
  tf_dense_dim = ceiling(2.67 * feat_size),
  tf_n_layers = 0,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = .30,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 2,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate",
  merge_attention_type = "MultiHead",
  merge_num_heads = 2L,
  merge_normalization_type = "PowerNorm",
  merge_times_pooling_type = "WeightedAverage"
)

classifier$train(
  data_embeddings = embeddings,
  data_targets = cat_codings,
  data_folds = 10,
  data_val_size = 0.25,
  loss_cls_fct_name =  loss,
  loss_balance_class_weights = TRUE,
  loss_balance_sequence_length = FALSE,
  use_sc = FALSE,
  sc_method = "knnor",
  sc_min_k = 1,
  sc_max_k = 10,
  use_pl = FALSE,
  pl_max_steps = 3,
  pl_max = 1.00,
  pl_anchor = 1.00,
  pl_min = 0.00,
  sustain_track = TRUE,
  sustain_iso_code = "DEU",
  sustain_region = NULL,
  sustain_interval = 15,
  sustain_log_level = "error",
  epochs = 20,
  batch_size = 64,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 1e-5,
  lr_min = 1e-5,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  lr_epochs = 5L,
  optimizer = "AdamW",
  amp = TRUE,
  comp_use = TRUE
)


for( loss in losses){
  print(paste("Batch Size:",batch_size))
classifier <- TEClassifierSequentialReferencePoint$new()
classifier$configure(
  label = "ReferencePoint classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = embeddings,
  feature_extractor = NULL,
  target_levels = cat_levels,
  skip_connection_type = "ResidualGate",
  cls_times_pooling_type = "WeightedAverage",
  metric_type = "CosineDistance",
  feat_act_fct = "Tanh",
  feat_size = 192,
  feat_bias = TRUE,
  feat_dropout = 0.02,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 0,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = max(2,ceiling(chunks/4)),
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.1,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "ELU",
  dense_n_layers = 2,
  dense_dropout = 0.30,
  dense_bias = FALSE,
  dense_parametrizations = "None",
  dense_normalization_type = "PowerNorm",
  dense_residual_type = "ResidualGate",
  rec_act_fct = "Tanh",
  rec_n_layers = 0,
  rec_type = "GRU",
  rec_bidirectional = FALSE,
  rec_dropout = 0.2,
  rec_bias = FALSE,
  rec_parametrizations = "None",
  rec_normalization_type = "PowerNorm",
  rec_residual_type = "ResidualGate",
  tf_act_fct = "SwiGLU",
  tf_dense_dim = 3*192,
  tf_n_layers = 0,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.3,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 2,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate"
)

classifier$train(
  data_embeddings = embeddings,
  data_targets = cat_codings,
  data_folds = 10,
  data_val_size = 0.25,
  loss_cls_fct_name =  losses,
  loss_balance_class_weights = TRUE,
  loss_balance_sequence_length = TRUE,
  use_sc = FALSE,
  sc_method = "knnor",
  sc_min_k = 1,
  sc_max_k = 10,
  use_pl = FALSE,
  pl_max_steps = 3,
  pl_max = 1.00,
  pl_anchor = 1.00,
  pl_min = 0.00,
  sustain_track = TRUE,
  sustain_iso_code = "DEU",
  sustain_region = NULL,
  sustain_interval = 15,
  sustain_log_level = "error",
  epochs = 3000,
  batch_size = batch_size,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 0.0,
  lr_min = 0.0,
  lr_scheduler = "None",
  lr_epochs=10L,
  lr_warm_up_ratio = 0.05,
  optimizer = "AdamW",
  amp = TRUE
)

classifier$reliability$test_metric_mean
results[loss]=list(classifier)
}
