devtools::load_all()
prepare_session()
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

chunk_quantile = base_model_eurobert$Tokenizer$calc_quantiles(
  text_dataset = data_set_reviews_text,
  batch_size = 32L,
  seq_len_tokens = 512L,
  token_overlap=128L,
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
  max_length = 512,
  chunks = chunk_quantile["99.9%"],
  overlap = 128,
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

classifier <- TEClassifierSequential$new()
classifier$configure(
  label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = NULL,
  target_levels = c("neg", "pos"),
  skip_connection_type = "ResidualGate",
  cls_pooling_features = 50,
  cls_pooling_type = "MinMaxTimes",
  cls_head_type = "PairwiseOrthogonal",
  feat_act_fct = "ELU",
  feat_size = 384,
  feat_bias = TRUE,
  feat_dropout = 0.10,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "ELU",
  ng_conv_n_layers = 2,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 4,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.33,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "ELU",
  dense_n_layers = 2,
  dense_dropout = 0.33,
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
  tf_act_fct = "ELU",
  tf_dense_dim = 400,
  tf_n_layers = 0,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.3,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 1,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Pre",
  tf_residual_type = "ResidualGate"
)
batch_size=8
classifier$train(
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 5,
  data_val_size = 0.40,
  loss_balance_class_weights = TRUE,
  loss_balance_sequence_length = TRUE,
  loss_cls_fct_name = "FocalLoss",
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
  epochs = 1000,
  batch_size = batch_size,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 2^-10*batch_size,
  lr_min = 1e-5,
  lr_scheduler="Linear",
  lr_warm_up_ratio = 0.10,
  optimizer = "AdamW",
  amp = TRUE
)
classifier$reliability$test_metric_mean
