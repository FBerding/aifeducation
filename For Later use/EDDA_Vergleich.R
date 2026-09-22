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

chunks=chunk_quantile["90%"]

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

save_to_disk(embeddings,dir_path = "For Later use",folder_name = "embeddings_test")
embeddings=load_from_disk(file.path("For Later use","embeddings_test"))

feature_extractor <- TEFeatureExtractor$new()
feature_extractor$configure(
  label = "Feature extractor for Text Embeddings",
  text_embeddings = embeddings,
  method="Conv",
  features = 128,
  te_n_layers=8,
  times=4,
  orthogonal_method = "matrix_exp",
  noise_factor = 1e-4
)
feature_extractor$train(
  data_embeddings = embeddings,
  data_val_size = 0.25,
  sustain_track = TRUE,
  sustain_iso_code = "DEU",
  sustain_region = NULL,
  sustain_interval = 15,
  sustain_log_level = "error",
  epochs = 8000,
  batch_size = 2048,
  lr_rate = 1e-6,
  lr_min = 1e-6,
  lr_epochs = 30,
  lr_scheduler = "Linear",
  trace = TRUE,
  ml_trace = 1,
  optimizer = "AdamW",
  amp = TRUE,
  lr_warm_up_ratio = 0.10,
  comp_use=TRUE,
  comp_mode="reduce-overhead"
)
save_to_disk(feature_extractor,dir_path = "For Later use",folder_name = "feext_test_times")
feature_extractor=load_from_disk(file.path("For Later use","feext_test_times"))

abc=feature_extractor$extract_features_large(embeddings,batch_size = 1024L,trace = TRUE)
abcc=abc$convert_to_EmbeddedText()
cor_test=abcc$calc_feature_correlation()
cor_test$effect_sizes$mean

classifier <- TEClassifierSequential$new()
classifier$configure(
  label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = embeddings,
  feature_extractor = feature_extractor,
  target_levels = cat_levels,
  skip_connection_type = "ResidualGate",
  cls_pooling_features = 20,
  cls_pooling_type = "WeightedAverageTimes",
  cls_head_type = "Regular",
  cls_input_normalize="PowerNorm",
  final_normalization_type="RMSNorm",
  feat_act_fct = "None",
  feat_size = 128,
  feat_bias = FALSE,
  feat_dropout = 0.05,
  feat_parametrizations = "None",
  feat_normalization_type = "None",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 4,
  ng_conv_ks_min = 1,
  ng_conv_ks_max = 3,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.40,
  ng_conv_parametrizations = "WeightNorm",
  ng_conv_normalization_type = "RMSNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "GELU",
  dense_n_layers = 0,
  dense_dropout = 0.40,
  dense_bias = FALSE,
  dense_parametrizations = "WeightNorm",
  dense_normalization_type = "RMSNorm",
  dense_residual_type = "ResidualGate",
  rec_act_fct = "Tanh",
  rec_n_layers = 0,
  rec_type = "GRU",
  rec_bidirectional = FALSE,
  rec_dropout = 0.2,
  rec_bias = FALSE,
  rec_parametrizations = "None",
  rec_normalization_type = "RMSNorm",
  rec_residual_type = "ResidualGate",
  tf_act_fct = "SwiGLU",
  tf_dense_dim = ceiling(2.67 * 128),
  tf_n_layers = 4,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.30,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 2,
  tf_bias = FALSE,
  tf_parametrizations = "WeightNorm",
  tf_normalization_type = "RMSNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate"
)

classifier$train(
  data_embeddings = embeddings,
  data_targets = cat_codings,
  data_folds = 10,
  data_val_size = 0.25,
  loss_balance_class_weights = TRUE,
  loss_balance_sequence_length = TRUE,
  loss_cls_fct_name = "FocalLossOrdinal",
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
  batch_size = 1024,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 0.0,
  lr_min = 0.0,
  lr_scheduler = "Linear",
  lr_epochs = 30,
  lr_warm_up_ratio = 0.10,
  optimizer = "AdamW",
  amp = TRUE,
  comp_use=TRUE,
  comp_mode="reduce-overhead"
)
classifier$reliability$test_metric_mean


#-------------------------------------------------------------------------------
loss="FocalLossOrdinal"
classifier <- TEClassifierParallelReferencePoint$new()
classifier$configure(
  label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = embeddings,
  embedding_dim = 384,
  cls_n_ref_points=6,
  final_normalization_type="RMSNorm",
  feature_extractor = NULL,
  target_levels = cat_levels,
  skip_connection_type = "ResidualGate",
  metric_type = "CosineDistance",
  cls_input_normalize="PowerNorm",
  shared_feat_layer = FALSE,
  feat_act_fct = "None",
  feat_size = 384,
  feat_bias = FALSE,
  feat_dropout = 0.00,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 3,
  ng_conv_bias = FALSE,
  ng_conv_dropout = .15,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "None",
  dense_n_layers = 5,
  dense_dropout = .20,
  dense_bias = FALSE,
  dense_parametrizations = "None",
  dense_normalization_type = "RMSNorm",
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
  tf_dense_dim = ceiling(2.67 * 384),
  tf_n_layers = 1,
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
  epochs = 2000,
  batch_size = 1024*3,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 1e-3,
  lr_min = 1e-4,
  lr_scheduler = "Linear",
  lr_warm_up_ratio = 0.05,
  lr_epochs = 5L,
  optimizer = "AdamW",
  amp = TRUE,
  comp_use = TRUE
)

classifier$reliability$test_metric_mean



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
  ng_conv_dropout = 0.4,
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
