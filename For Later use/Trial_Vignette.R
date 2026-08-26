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

head_types=c(
  "Regular"#,
  #"PairwiseOrthogonalDense"#,
  #"PairwiseOrthogonal"
  )
cls_pooling_types=c(
  #"Max"#,
  #"Min",
  #"MinMax",
  "MaxTimes"#,
  #"MinMaxTimes"
  )
layer_types=list(
  #dense=list(
  #  dense_n_layers=1,
  #  rec_n_layers=0,
  #  tf_n_layers=0,
  #  ng_conv_n_layers=0
  #),
  #conv=list(
  #  dense_n_layers=2,
  #  rec_n_layers=0,
  #  tf_n_layers=0,
  #  ng_conv_n_layers=1
  #)#,
  tf=list(
    dense_n_layers=2,
    rec_n_layers=0,
    tf_n_layers=0,
    ng_conv_n_layers=1
  )#,
  #rec=list(
  #  dense_n_layers=0,
  #  rec_n_layers=1,
  #  tf_n_layers=0,
  #  ng_conv_n_layers=0
  #)
)
n_data_folds=10
pooling_features=c(50)
batch_size=32L
devtools::load_all()
prepare_session()
results=list()
for (head_type in head_types){
  for(cls_pooling_type in cls_pooling_types){
    for(layer_type in names(layer_types)){
      for(pooling_feature in pooling_features){
        current_layers=layer_types[[layer_type]]

        classifier <- TEClassifierSequential$new()
        classifier$configure(
          label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = review_embeddings,
          feature_extractor = NULL,
          target_levels = c("neg", "pos"),
          skip_connection_type = "ResidualGate",
          cls_pooling_features = pooling_feature,
          cls_pooling_type = cls_pooling_type,
          cls_head_type = head_type,
          feat_act_fct = "ELU",
          feat_size = 64,
          feat_bias = TRUE,
          feat_dropout = 0.0,
          feat_parametrizations = "None",
          feat_normalization_type = "PowerNorm",
          ng_conv_act_fct = "ELU",
          ng_conv_n_layers = current_layers$ng_conv_n_layers,
          ng_conv_ks_min = 2,
          ng_conv_ks_max = 4,
          ng_conv_bias = FALSE,
          ng_conv_dropout = .10,
          ng_conv_parametrizations = "None",
          ng_conv_normalization_type = "PowerNorm",
          ng_conv_residual_type = "ResidualGate",
          dense_act_fct = "ELU",
          dense_n_layers = current_layers$dense_n_layers,
          dense_dropout = .33,
          dense_bias = FALSE,
          dense_parametrizations = "None",
          dense_normalization_type = "PowerNorm",
          dense_residual_type = "ResidualGate",
          rec_act_fct = "Tanh",
          rec_n_layers = current_layers$rec_n_layers,
          rec_type = "GRU",
          rec_bidirectional = FALSE,
          rec_dropout = 0.2,
          rec_bias = FALSE,
          rec_parametrizations = "None",
          rec_normalization_type = "PowerNorm",
          rec_residual_type = "ResidualGate",
          tf_act_fct = "ELU",
          tf_dense_dim = 1.25*64,
          tf_n_layers = current_layers$tf_n_layers,
          tf_dropout_rate_1 = 0.1,
          tf_dropout_rate_2 = 0.3,
          tf_attention_type = "MultiHead",
          tf_positional_type = "absolute",
          tf_num_heads = 8,
          tf_bias = FALSE,
          tf_parametrizations = "None",
          tf_normalization_type = "PowerNorm",
          tf_normalization_position = "Post",
          tf_residual_type = "ResidualGate"
        )

        classifier$train(
          data_embeddings = review_embeddings,
          data_targets = review_labels,
          data_folds = n_data_folds,
          data_val_size = .25,
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
          sustain_log_level = "error",
          epochs = 2000,
          batch_size = batch_size,
          trace = TRUE,
          ml_trace = 1,
          log_dir = NULL,
          log_write_interval = 10,
          n_cores = auto_n_cores(),
          lr_rate = 0.0,
          lr_min = 0.0,
          lr_scheduler="None",
          lr_warm_up_ratio = 0.05,
          optimizer = "Adam",
          amp = TRUE
        )
        results[[length(results)+1]]=list(
          head_type=head_type,
          cls_pooling_type=cls_pooling_type,
          #layer_type=current_layers,
          metrics=list(classifier$reliability$test_metric_mean),
          cls_features=pooling_feature
        )
        save(results,file="test_performance.rda")
      }
    }
  }
}
classifier$reliability$test_metric_mean
classifier$plot_learning_rate()
save_to_disk(classifier,
             dir_path = "For Later Use",
             folder_name="test_cls")

#------------------------------------
devtools::load_all()
classifier=load_from_disk("For Later Use/test_cls")
classifier$plot_learning_rate()
classifier$reliability$test_metric_mean
classifier$plot_training_history(
  final_training = TRUE,
  pl_step = NULL,
  measure = "s_avg_iota",
  ind_best_model = FALSE,
  ind_selected_model = TRUE,
  x_min = NULL,
  x_max = NULL,
  y_min = NULL,
  y_max = 1L,
  add_min_max = FALSE,
  text_size = 10
)
classifier$plot_training_history(
  final_training = FALSE,
  pl_step = NULL,
  measure = "s_avg_iota",
  ind_best_model = FALSE,
  ind_selected_model = TRUE,
  x_min = NULL,
  x_max = NULL,
  y_min = NULL,
  y_max = 1L,
  add_min_max = FALSE,
  text_size = 10
)
#-----------------------------------------------------------------------------
devtools::load_all()
prepare_session()
        classifier <- TEClassifierSequential$new()
        classifier$configure(
          label = "ProtoNet classifier for Estimating a Postive or Negative Rating of Movie Reviews",
          text_embeddings = review_embeddings,
          feature_extractor = NULL,
          target_levels = c("neg", "pos"),
          skip_connection_type = "ResidualGate",
          cls_pooling_features = 10,
          cls_pooling_type = "MaxTimes",
          cls_head_type="Regular",
          #projection_type = "Regular",
          #metric_type = "Euclidean",
          #merge_attention_type = "MultiHead",
          #merge_num_heads = 10L,
          #merge_normalization_type = "PowerNorm",
          #merge_pooling_features = 50L,
          #merge_pooling_type = "MaxTimes",
          feat_act_fct = "ELU",
          feat_size = 96,
          feat_bias = TRUE,
          feat_dropout = 0.0,
          feat_parametrizations = "None",
          feat_normalization_type = "PowerNorm",
          ng_conv_act_fct = "ELU",
          ng_conv_n_layers = 1,
          ng_conv_ks_min = 2,
          ng_conv_ks_max = 4,
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
          tf_act_fct = "ELU",
          tf_dense_dim = 1.25*96,
          tf_n_layers = 0,
          tf_dropout_rate_1 = 0.1,
          tf_dropout_rate_2 = 0.3,
          tf_attention_type = "MultiHead",
          tf_positional_type = "absolute",
          tf_num_heads = 8,
          tf_bias = FALSE,
          tf_parametrizations = "None",
          tf_normalization_type = "PowerNorm",
          tf_normalization_position = "Post",
          tf_residual_type = "ResidualGate"
        )

        classifier$train(
          data_embeddings = review_embeddings,
          data_targets = review_labels,
          data_folds = 10,
          data_val_size = 0.25,
          loss_cls_fct_name =  "FocalLoss",
          loss_balance_class_weights = TRUE,
          loss_balance_sequence_length = TRUE,
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
          epochs = 4000,
          batch_size = 32,
          trace = TRUE,
          ml_trace = 1,
          log_dir = NULL,
          log_write_interval = 10,
          n_cores = auto_n_cores(),
          lr_rate = 0.0,
          lr_min = 0.0,
          lr_scheduler = "None",
          lr_warm_up_ratio = 0.05,
          optimizer = "AdamW",
          amp = TRUE
        )


classifier$reliability$test_metric_mean
classifier$plot_training_history(
  final_training = FALSE,
  pl_step = NULL,
  measure = "s_avg_iota",
  ind_best_model = FALSE,
  ind_selected_model = TRUE,
  x_min = NULL,
  x_max = NULL,
  y_min = NULL,
  y_max = NULL,
  add_min_max = FALSE,
  text_size = 10
)

#--------------------------------------------------------------------------------
devtools::load_all()
load_all_py_scripts()
start_time=Sys.time()
dim=128L
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
  feat_act_fct = "ELU",
  feat_size = dim,
  feat_bias = TRUE,
  feat_dropout = 0.0,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "ELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 4,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.25,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "ELU",
  dense_n_layers = 0,
  dense_dropout = 0.25,
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
  tf_dense_dim = 1.25 * dim,
  tf_n_layers = 1,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.3,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 4,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate",
  embedding_dim = dim
)

classifier_prototype$train(
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 2,
  data_val_size = 0.25,
  loss_pt_fct_name = "AEMLoss",
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
  epochs = 1500,
  batch_size = 32,
  Ns = 5,
  Nq = 3,
  loss_alpha = 0.75,
  loss_margin = 0.10,
  sampling_separate = FALSE,
  sampling_shuffle = TRUE,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 0.0,
  lr_min = 0.0,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  optimizer = "AdamW",
  amp = TRUE
)

feat_size=96
classifier_prototype <- TEClassifierParallelPrototype$new()
classifier_prototype$configure(
  label = "ProtoNet classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = NULL,
  shared_feat_layer =FALSE,
  target_levels = c("neg", "pos"),
  merge_attention_type = "MultiHead",
  merge_num_heads = 2L,
  merge_normalization_type = "PowerNorm",
  merge_pooling_features = 50L,
  merge_pooling_type = "MaxTimes",
  projection_type = "Regular",
  metric_type = "Euclidean",
  points_per_class=2,
  feat_act_fct = "Tanh",
  feat_size = feat_size,
  feat_bias = TRUE,
  feat_dropout = 0.00,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "ELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 4,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.30,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "ELU",
  dense_n_layers = 0,
  dense_dropout = 0.3,
  dense_bias = FALSE,
  dense_parametrizations = "None",
  dense_normalization_type = "PowerNorm",
  dense_residual_type = "ResidualGate",
  rec_act_fct = "Tanh",
  rec_n_layers = 2,
  rec_type = "GRU",
  rec_bidirectional = FALSE,
  rec_dropout = 0.3,
  rec_bias = FALSE,
  rec_parametrizations = "None",
  rec_normalization_type = "PowerNorm",
  rec_residual_type = "ResidualGate",
  tf_act_fct = "ELU",
  tf_dense_dim = 2 * feat_size,
  tf_n_layers = 1,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.30,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 4,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate",
  embedding_dim = 2
)

classifier_prototype$train(
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 2,
  data_val_size = 0.25,
  loss_pt_fct_name = "AEMLoss",
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
  epochs = 4000,
  batch_size = 32,
  Ns = 18,
  Nq = 3,
  loss_alpha = 0.75,
  loss_margin = 0.10,
  sampling_separate = FALSE,
  sampling_shuffle = TRUE,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 0.0,
  lr_min = 0.0,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  optimizer = "AdamW",
  amp = TRUE
)
end_time=Sys.time()
classifier_prototype$plot_learning_rate()
classifier_prototype$get_lr_statistics()
classifier_prototype$reliability$test_metric_mean
save_to_disk(classifier_prototype,
             dir_path = "For Later Use",
             folder_name="test_cls")
classifier_prototype$plot_training_history(final_training = TRUE,measure = "s_avg_iota")
#------------------------------------
devtools::load_all()
classifier_prototype=load_from_disk("For Later Use/test_cls")

classifier_prototype$plot_training_history(final_training = FALSE,measure = "s_avg_iota")
classifier_prototype$plot_learning_rate()

which(classifier_prototype$get_lr_statistics()$delta==max(classifier_prototype$get_lr_statistics()$delta))

#--------------------------------------------------------------------------------
devtools::load_all()
load_all_py_scripts()
classifier <- TEClassifierSequentialReferencePoint$new()
classifier$configure(
  label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = NULL,
  target_levels = c("neg", "pos"),
  skip_connection_type = "ResidualGate",
  cls_times_pooling_type = "WeightedAverage",
  metric_type = "CosineDistance",
  feat_act_fct = "Tanh",
  feat_size = 384,
  feat_bias = TRUE,
  feat_dropout = 0.0,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 1,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 3,
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
  tf_residual_type = "ResidualGate"
)

classifier$train(
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 10,
  data_val_size = 0.25,
  loss_cls_fct_name =  "FocalLoss",
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
  batch_size = 32,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 0.0,
  lr_min = 0.0,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  optimizer = "AdamW",
  amp = TRUE
)

classifier$reliability$test_metric_mean

#--------------------------------------------------------------------------------
devtools::load_all()
load_all_py_scripts()
feat_size=384
classifier <- TEClassifierParallelReferencePoint$new()
classifier$configure(
  label = "Classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = NULL,
  target_levels = c("neg", "pos"),
  skip_connection_type = "ResidualGate",
  metric_type = "CosineDistance",
  shared_feat_layer = FALSE,
  feat_act_fct = "Tanh",
  feat_size = feat_size,
  feat_bias = TRUE,
  feat_dropout = 0.02,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 2,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 3,
  ng_conv_bias = FALSE,
  ng_conv_dropout = .05,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "GELU",
  dense_n_layers = 0,
  dense_dropout = .30,
  dense_bias = FALSE,
  dense_parametrizations = "None",
  dense_normalization_type = "RMSNorm",
  dense_residual_type = "ResidualGate",
  rec_act_fct = "Tanh",
  rec_n_layers = 2,
  rec_type = "GRU",
  rec_bidirectional = TRUE,
  rec_dropout = .20,
  rec_bias = FALSE,
  rec_parametrizations = "None",
  rec_normalization_type = "PowerNorm",
  rec_residual_type = "ResidualGate",
  tf_act_fct = "SwiGLU",
  tf_dense_dim = ceiling(2.67 * feat_size),
  tf_n_layers = 2,
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
  data_embeddings = review_embeddings,
  data_targets = review_labels,
  data_folds = 10,
  data_val_size = 0.25,
  #loss_cls_fct_name =  "FocalLoss",
  loss_cls_fct_name =  "AEMLoss",
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
  epochs = 3500,
  batch_size = 64,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 1e-4,
  lr_min = 1e-4,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  lr_epochs = 20L,
  optimizer = "AdamW",
  amp = TRUE
)

classifier$reliability$test_metric_mean

#----------------------------
devtools::load_all()
load_all_py_scripts()
#prepare_session()
classifier <- TEClassifierSequentialReferencePoint$new()
#classifier <- TEClassifierSequential$new()
classifier$configure(
  label = "ReferencePoint classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = review_embeddings,
  feature_extractor = NULL,
  target_levels = c("neg", "pos"),
  skip_connection_type = "ResidualGate",
  cls_times_pooling_type = "WeightedAverage",
  #cls_pooling_type = "WeightedAverageTimes",
  #cls_pooling_features = 50,
  metric_type = "CosineDistance",
  feat_act_fct = "Tanh",
  feat_size = 384,
  feat_bias = TRUE,
  feat_dropout = 0.00,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "GELU",
  ng_conv_n_layers = 0,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 4,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.1,
  ng_conv_parametrizations = "None",
  ng_conv_normalization_type = "PowerNorm",
  ng_conv_residual_type = "ResidualGate",
  dense_act_fct = "ELU",
  dense_n_layers = 0,
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
  tf_n_layers = 2,
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
  loss_cls_fct_name =  "FocalLoss",
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
  batch_size = 32,
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
  amp = TRUE
)

classifier$reliability$test_metric_mean

ref_point_lcs=classifier
