devtools::load_all()
prepare_session()
root_path="C:/Users/User/Desktop/debug_run"
path_te=file.path(root_path,"te_models","edu_net_mpnet_1")
path_raw_texts=file.path(root_path,"input_data","dataset_texts.rda")
path_codings=file.path(root_path,"input_data","dataset_codings.csv")

te_model=load_from_disk(path_te)
raw_texts=load(path_raw_texts)
codings=read.csv(path_codings)
large_dataset_texts=LargeDataSetForText$new(text_data_frame)

classes=c(0,1,2,3)
review_labels=factor(codings$basic_structure)
names(review_labels)=codings$ID

embeddings=te_model$embed_large(
  text_dataset = large_dataset_texts,
  trace = TRUE,
  batch_size = 64L
)
load_all_py_scripts()
#cls=TEClassifierSequential$new()
cls=TEClassifierParallelPrototype$new()
cls$configure(
  shared_feat_layer = FALSE,
  merge_attention_type = "MultiHead",
  merge_num_heads = 4L,
  merge_normalization_type = "LayerNorm",
  merge_pooling_features = 0.25*192,
  merge_pooling_type = "MinMaxTimes",

  label = "ProtoNet classifier for Estimating a Postive or Negative Rating of Movie Reviews",
  text_embeddings = embeddings,
  feature_extractor = NULL,
  target_levels = classes,

  #skip_connection_type = "ResidualGate",
  #cls_pooling_features = 0.5*192,
  #cls_pooling_type = "MinMaxTimes",
  #cls_head_type = "Regular",
  #cls_head_type = "PairwiseOrthogonalDense",

  projection_type = "PairwiseOrthogonal",
  metric_type = "CosineDistance",

  feat_act_fct = "ELU",
  feat_size = 0.5*192,
  feat_bias = FALSE,
  feat_dropout = 0.33,
  feat_parametrizations = "None",
  feat_normalization_type = "PowerNorm",
  ng_conv_act_fct = "ELU",
  ng_conv_n_layers = 0,
  ng_conv_ks_min = 2,
  ng_conv_ks_max = 6,
  ng_conv_bias = FALSE,
  ng_conv_dropout = 0.3,
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
  tf_dense_dim = 1*192,
  tf_n_layers = 1,
  tf_dropout_rate_1 = 0.1,
  tf_dropout_rate_2 = 0.5,
  tf_attention_type = "MultiHead",
  tf_positional_type = "absolute",
  tf_num_heads = 8,
  tf_bias = FALSE,
  tf_parametrizations = "None",
  tf_normalization_type = "PowerNorm",
  tf_normalization_position = "Post",
  tf_residual_type = "ResidualGate",
  embedding_dim = 2
)
print(table(review_labels))
cls$count_parameter()
cls$train(
  data_embeddings = embeddings,
  data_targets = review_labels,
  data_folds = 5,
  data_val_size = 0.25,
  loss_pt_fct_name = "MultiWayContrastiveLossFC",
  #loss_balance_class_weights = TRUE,
  #loss_balance_sequence_length = TRUE,
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
  batch_size = 16,
  #Ns = 10,
  #Nq = 3,
  #loss_alpha = 0.5,
  #loss_margin = 0.10,
  #sampling_separate = FALSE,
  #sampling_shuffle = TRUE,
  trace = TRUE,
  ml_trace = 1,
  log_dir = NULL,
  log_write_interval = 10,
  n_cores = auto_n_cores(),
  lr_rate = 1e-4,
  lr_min = 1e-6,
  lr_scheduler = "None",
  lr_warm_up_ratio = 0.05,
  optimizer = "AdamW",
  amp = FALSE
)

