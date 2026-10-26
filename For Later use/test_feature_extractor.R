devtools::load_all()
prepare_session()

#Prepare Data
example_data <- imdb_movie_reviews_large
example_data$label <- replace(example_data$label,example_data$label==0,"neg")
example_data$label <- replace(example_data$label,example_data$label==1,"pos")
example_data$label <- as.character(example_data$label)
example_data$label[c(76:100)] <- NA
example_data$label[c(201:250)] <- NA
table(example_data$label)
example_data$bib_entry <- NA
example_data$license <- NA
colnames(example_data)
data_set_reviews_text <- LargeDataSetForText$new()
data_set_reviews_text$add_from_data.frame(example_data)
print(data_set_reviews_text)

#Prepare Labels
review_labels <- as.factor(example_data$label)
names(review_labels) <- example_data$id

# Load a BaseModel
base_model_eurobert <- BaseModelEuroBert$new()
base_model_eurobert$create_from_hf(
  model_dir = "vignettes/examples/EuroBERT-210m",
  tokenizer_dir = "vignettes/examples/EuroBERT-210m"
)

# Get information about the model and
# set configuration for the TextEmbeddingModel
total_max_seq_len <- base_model_eurobert$get_max_seq_len()
total_max_seq_len
chunk_quantile <- base_model_eurobert$Tokenizer$calc_quantiles(
  text_dataset = data_set_reviews_text,
  batch_size = 32L,
  seq_len_tokens = 512L,
  token_overlap = 128L,
  trace = FALSE
)
print(chunk_quantile)
max_chunks <- chunk_quantile["99.9%"]
num_layers <- base_model_eurobert$get_n_layers()
num_layers
eurobert_min_layer <- floor(0.5 * num_layers)
eurobert_min_layer

eurobert_max_layer <- ceiling(2 / 3 * num_layers)
eurobert_max_layer

#Create TextEmbeddingModel
tem <- TextEmbeddingModel$new()
tem$configure(
  model_label = "Text Embedding via EuroBert - 210m",
  model_language = "english",
  max_length = 512,
  chunks = max_chunks,
  overlap = 128,
  emb_layer_min = eurobert_min_layer,
  emb_layer_max = eurobert_max_layer,
  emb_pool_type = "Average",
  base_model = base_model_eurobert,
  emb_insert_mask_tokens = 0.15
)

# Create text embeddings
review_embeddings <- tem$embed_large(
  text_dataset = data_set_reviews_text,
  trace = TRUE
)

feature_extractor <- TEFeatureExtractor$new()
feature_extractor$configure(
  label = "Feature extractor for Text Embeddings",
  text_embeddings = review_embeddings,
  vae=TRUE,
  #vae=FALSE,
  encoder_dropout=0.10,
  decoder_dropout=0.10,
  decoder_share_weights = FALSE,
  decoder_linear=FALSE,
  fe_type="Dense",
  fe_normalization="LayerNorm",
  fe_act_fct="GELU",
  features = 128,
  te_n_layers=5,
  orthogonal_method = "None",
  noise_factor = 1e-4
)
print(feature_extractor)

epochs=5000
patience=epochs
feature_extractor$train(
  data_embeddings = review_embeddings,
  fe_loss_type="InfoVAE",
  #fe_loss_type="CosineStructure",
  fe_loss_alpha=.99,
  fe_loss_lambda=3,
  data_val_size = 0.25,
  sustain_track = TRUE,
  sustain_iso_code = "DEU",
  sustain_region = NULL,
  sustain_interval = 15,
  sustain_log_level = "error",
  epochs = epochs,
  batch_size = 256,
  lr_rate = 1e-3,
  lr_min = 1e-7,
  lr_epochs = 15,
  lr_scheduler = "CosineAnnealing",
  trace = TRUE,
  ml_trace = 1,
  optimizer = "AdamW",
  amp = TRUE,
  lr_warm_up_ratio = 0.05,
  comp_use=TRUE,
  comp_mode="reduce-overhead",
  patience=patience
)

embedding_statistics=feature_extractor$compare_embedding_statistics(
  data_embeddings=review_embeddings,
  batch_size=128,
  trace=TRUE
)
embedding_statistics

abc=review_embeddings$convert_to_EmbeddedText()
abc$embeddings[1:10,1:2,1:2]
test_extracts=feature_extractor$extract_features(review_embeddings,batch_size = 256)
test_extracts$embeddings[1:10,1:2,1:2]

feature_extractor$plot_training_history(measure="loss",ind_selected_model = TRUE)
feature_extractor$plot_training_history(measure="recon",ind_selected_model = TRUE)
feature_extractor$plot_training_history(measure="kld",ind_selected_model = TRUE)
feature_extractor$plot_training_history(measure="mmd",ind_selected_model = TRUE)
feature_extractor$plot_training_history(measure="magnitude",ind_selected_model = TRUE)
feature_extractor$plot_training_history(measure="pairwise_cosine",ind_selected_model = TRUE)
feature_extractor$plot_training_history(measure="mean_euc_distance",ind_selected_model = TRUE)

#-------------------
classifier <- TEClassifierSequential$new()
classifier$configure(
  label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = feature_extractor,
  target_levels = c("neg", "pos"),
  skip_connection_type = "ResidualGate",
  cls_pooling_features = 20,
  cls_pooling_type = "WeightedAverageTimes",
  cls_head_type = "Regular",
  cls_input_normalize="None",
  final_normalization_type="LayerNorm",
  feat_act_fct = "None",
  feat_size = 128,
  feat_bias = TRUE,
  feat_dropout = 0.00,
  feat_parametrizations = "None",
  feat_normalization_type = "None",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 3,
  ng_conv_bias = TRUE,
  ng_conv_dropout = 0.10,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "BatchNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "GELU",
  dense_n_layers = 3,
  dense_dropout = 0.50,
  dense_bias = TRUE,
  dense_parametrizations = "None",
  dense_normalization_type = "BatchNorm",
  dense_residual_type = "ResidualGate",
  rec_act_fct = "Tanh",
  rec_n_layers = 0,
  rec_type = "GRU",
  rec_bidirectional = TRUE,
  rec_dropout = 0.2,
  rec_bias = FALSE,
  rec_parametrizations = "None",
  rec_normalization_type = "LayerNorm",
  rec_residual_type = "ResidualGate",
  tf_act_fct = "SwiGLU",
  tf_dense_dim = ceiling(2.67 * 384),
  tf_n_layers = 0,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.3,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 4,
  tf_bias = TRUE,
  tf_parametrizations = "None",
  tf_normalization_type = "LayerNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate"
)

classifier$train(
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 10,
  data_val_size = 0.25,
  loss_balance_class_weights = TRUE,
  loss_balance_sequence_length = TRUE,
  loss_cls_fct_name = "CrossEntropyLoss",
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
  batch_size = 128,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 0.0,
  lr_min = 0.0,
  lr_scheduler = "CosineAnnealing",
  lr_epochs = 30,
  lr_warm_up_ratio = 0.10,
  optimizer = "AdamW",
  amp = TRUE,
  comp_use=FALSE,
  comp_mode="reduce-overhead",
  patience=300
)
classifier$reliability$test_metric_mean
