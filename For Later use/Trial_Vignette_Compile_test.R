
devtools::load_all()
Sys.setenv(CUDA_LAUNCH_BLOCKING = "1")
Sys.setenv(TORCH_USE_CUDA_DSA = "1")
prepare_session()
os=reticulate::import("os")
os$environ$setdefault("CUDA_LAUNCH_BLOCKING",1)
example_data <- imdb_movie_reviews
example_data$label <- as.character(example_data$label)
example_data$label[c(76:100)] <- NA
example_data$label[c(201:250)] <- NA
table(example_data$label)

example_data$bib_entry <- NA
example_data$license <- NA
colnames(example_data)
data_set_reviews_text <- LargeDataSetForText$new()
data_set_reviews_text$add_from_data.frame(example_data)
review_labels <- as.factor(example_data$label)
names(review_labels) <- example_data$id

base_model_eurobert <- BaseModelEuroBert$new()
base_model_eurobert$create_from_hf(
  model_dir = "vignettes/examples/EuroBERT-210m",
  tokenizer_dir = "vignettes/examples/EuroBERT-210m"
)
total_max_seq_len=base_model_eurobert$get_max_seq_len()
total_max_seq_len


seq_len=512L
overlap=128L
chunk_quantile = base_model_eurobert$Tokenizer$calc_quantiles(
  text_dataset = data_set_reviews_text,
  batch_size = 32L,
  seq_len_tokens = seq_len,
  token_overlap=overlap,
  trace=FALSE
)
print(chunk_quantile)
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
  chunks = chunk_quantile["99.9%"],
  overlap = overlap,
  emb_layer_min = eurobert_min_layer,
  emb_layer_max = eurobert_max_layer,
  emb_pool_type = "Average",
  base_model = base_model_eurobert,
  emb_insert_mask_tokens=0.15
)
review_embeddings <- tem$embed_large(
  text_dataset = data_set_reviews_text,
  trace = TRUE
)

#----------------


devtools::load_all()
load_all_py_scripts()
#prepare_session()
classifier <- TEClassifierSequentialReferencePoint$new()
classifier <- TEClassifierSequential$new()
classifier$configure(
  label = "ReferencePoint classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = NULL,
  target_levels = c("neg", "pos"),
  skip_connection_type = "ResidualGate",
  cls_pooling_type = "MinMax",
  cls_pooling_features = 50,
  #cls_times_pooling_type = "WeightedAverage",
  cls_input_normalize="BatchNorm",
 # metric_type = "CosineDistance",
  feat_act_fct = "Tanh",
  feat_size = 384,
  feat_bias = TRUE,
  feat_dropout = 0.02,
  feat_parametrizations = "None",
  feat_normalization_type = "LayerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 4,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.1,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "RMSNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "ELU",
  dense_n_layers = 1,
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
  tf_dense_dim = 3*384,
  tf_n_layers = 1,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.4,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 2,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate"
)
print(classifier$count_parameter())
classifier$train(
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 10,
  data_val_size = 0.25,
  loss_cls_fct_name =  "CrossEntropyLoss",
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
  epochs = 100,
  batch_size = 4,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 1e-4,
  lr_min = 1e-4,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  optimizer = "AdamW",
  amp = TRUE,
  comp_use=TRUE
)

classifier$reliability$test_metric_mean

com_classifier=classifier
com_classifier$last_training$learning_time

#--------------------------------------------------------------------------------------
classifier_prototype <- TEClassifierSequentialPrototype$new()
classifier_prototype$configure(
  label = "ProtoNet classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = NULL,
  target_levels = c("neg", "pos"),
  skip_connection_type = "ResidualGate",
  cls_pooling_features = 20,
  cls_pooling_type = "MaxTimes",
  projection_type = "Regular",
  metric_type = "Euclidean",
  feat_act_fct = "Tanh",
  feat_size = 384,
  feat_bias = TRUE,
  feat_dropout = 0.0,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = max_chunks,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.05,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "GELU",
  dense_n_layers = 2,
  dense_dropout = 0.30,
  dense_bias = FALSE,
  dense_parametrizations = "None",
  dense_normalization_type = "RMSNorm",
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
  tf_dense_dim = ceiling(2.67 * 64),
  tf_n_layers = 0,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.3,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 12,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Pre",
  tf_residual_type = "ResidualGate",
  embedding_dim = 2
)

classifier_prototype$train(
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 10,
  data_val_size = 0.25,
  loss_pt_fct_name = "MultiWayContrastiveLossFC",
  use_sc = TRUE,
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
  batch_size = 32,
  Ns = 18,
  Nq = 3,
  loss_alpha = 0.50,
  loss_margin = 0.05,
  sampling_separate = FALSE,
  sampling_shuffle = TRUE,
  trace = TRUE,
  ml_trace = 0,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 0.0,
  lr_min = 0.0,
  lr_epochs = 75,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  optimizer = "AdamW",
  amp = TRUE,
  comp_use=TRUE
)
