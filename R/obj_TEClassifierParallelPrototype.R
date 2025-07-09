# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

#' @title Text embedding classifier with a ProtoNet
#' @description
#' `r build_documentation_for_model(model_name="TEClassifierParallelPrototype",cls_type="prototype",core_type="parallel",input_type="text_embeddings")`
#'
#' @return Returns a new object of this class ready for configuration or for loading
#' a saved classifier.
#'
#'
#' @references Oreshkin, B. N., Rodriguez, P. & Lacoste, A. (2018). TADAM: Task dependent adaptive metric for improved
#'   few-shot learning. https://doi.org/10.48550/arXiv.1805.10123
#' @references Snell, J., Swersky, K. & Zemel, R. S. (2017). Prototypical Networks for Few-shot Learning.
#'   https://doi.org/10.48550/arXiv.1703.05175
#' @references Zhang, X., Nie, J., Zong, L., Yu, H. & Liang, W. (2019). One Shot Learning with Margin. In Q. Yang, Z.-H.
#'   Zhou, Z. Gong, M.-L. Zhang & S.-J. Huang (Eds.), Lecture Notes in Computer Science. Advances in Knowledge Discovery
#'   and Data Mining (Vol. 11440, pp. 305–317). Springer International Publishing.
#'   https://doi.org/10.1007/978-3-030-16145-3_24
#' @family Classification
#' @export
TEClassifierParallelPrototype <- R6::R6Class(
  classname = "TEClassifierParallelPrototype",
  inherit = TEClassifiersBasedOnProtoNet,
  public = list(
    # New-----------------------------------------------------------------------
    #' @description Creating a new instance of this class.
    #' @param name `r get_param_doc_desc("name")`
    #' @param label `r get_param_doc_desc("label")`
    #' @param text_embeddings `r get_param_doc_desc("text_embeddings")`
    #' @param feature_extractor `r get_param_doc_desc("feature_extractor")`
    #' @param target_levels `r get_param_doc_desc("target_levels")`
    #' @param cls_pooling_features `r get_param_doc_desc("cls_pooling_features")`
    #' @param cls_pooling_type `r get_param_doc_desc("cls_pooling_type")`
    #' @param feat_act_fct `r get_param_doc_desc("feat_act_fct")`
    #' @param feat_size `r get_param_doc_desc("feat_size")`
    #' @param feat_bias `r get_param_doc_desc("feat_bias")`
    #' @param feat_dropout `r get_param_doc_desc("feat_dropout")`
    #' @param feat_parametrizations `r get_param_doc_desc("feat_parametrizations")`
    #' @param feat_normalization_type `r get_param_doc_desc("feat_normalization_type")`
    #' @param ng_conv_act_fct `r get_param_doc_desc("ng_conv_act_fct")`
    #' @param ng_conv_n_layers `r get_param_doc_desc("ng_conv_n_layers")`
    #' @param ng_conv_ks_min `r get_param_doc_desc("ng_conv_ks_min")`
    #' @param ng_conv_ks_max `r get_param_doc_desc("ng_conv_ks_max")`
    #' @param ng_conv_bias `r get_param_doc_desc("ng_conv_bias")`
    #' @param ng_conv_dropout `r get_param_doc_desc("ng_conv_dropout")`
    #' @param ng_conv_parametrizations `r get_param_doc_desc("ng_conv_parametrizations")`
    #' @param ng_conv_normalization_type `r get_param_doc_desc("ng_conv_normalization_type")`
    #' @param ng_conv_residual_type `r get_param_doc_desc("ng_conv_residual_type")`
    #' @param dense_act_fct `r get_param_doc_desc("dense_act_fct")`
    #' @param dense_n_layers `r get_param_doc_desc("dense_n_layers")`
    #' @param dense_dropout `r get_param_doc_desc("dense_dropout")`
    #' @param dense_bias `r get_param_doc_desc("dense_bias")`
    #' @param dense_parametrizations `r get_param_doc_desc("dense_parametrizations")`
    #' @param dense_normalization_type `r get_param_doc_desc("dense_normalization_type")`
    #' @param dense_residual_type `r get_param_doc_desc("dense_residual_type")`
    #' @param rec_act_fct `r get_param_doc_desc("rec_act_fct")`
    #' @param rec_n_layers `r get_param_doc_desc("rec_n_layers")`
    #' @param rec_type `r get_param_doc_desc("rec_type")`
    #' @param rec_bidirectional `r get_param_doc_desc("rec_bidirectional")`
    #' @param rec_dropout `r get_param_doc_desc("rec_dropout")`
    #' @param rec_bias `r get_param_doc_desc("rec_bias")`
    #' @param rec_parametrizations `r get_param_doc_desc("rec_parametrizations")`
    #' @param rec_normalization_type `r get_param_doc_desc("rec_normalization_type")`
    #' @param rec_residual_type `r get_param_doc_desc("rec_residual_type")`
    #' @param tf_act_fct `r get_param_doc_desc("tf_act_fct")`
    #' @param tf_dense_dim `r get_param_doc_desc("tf_dense_dim")`
    #' @param tf_n_layers `r get_param_doc_desc("tf_n_layers")`
    #' @param tf_dropout_rate_1 `r get_param_doc_desc("tf_dropout_rate_1")`
    #' @param tf_dropout_rate_2 `r get_param_doc_desc("tf_dropout_rate_2")`
    #' @param tf_attention_type `r get_param_doc_desc("tf_attention_type")`
    #' @param tf_embedding_type `r get_param_doc_desc("tf_embedding_type")`
    #' @param tf_num_heads `r get_param_doc_desc("tf_num_heads")`
    #' @param tf_bias `r get_param_doc_desc("tf_bias")`
    #' @param tf_parametrizations `r get_param_doc_desc("tf_parametrizations")`
    #' @param tf_normalization_type `r get_param_doc_desc("tf_normalization_type")`
    #' @param tf_residual_type `r get_param_doc_desc("tf_residual_type")`
    #' @param merge_attention_type `r get_param_doc_desc("merge_attention_type")`
    #' @param merge_num_heads `r get_param_doc_desc("merge_num_heads")`
    #' @param merge_normalization_type `r get_param_doc_desc("merge_normalization_type")`
    #' @param metric_type `r get_param_doc_desc("metric_type")`
    #' @param embedding_dim `r get_param_doc_desc("embedding_dim")`
    #' @return Function does nothing return. It modifies the current object.
    configure = function(name = NULL,
                         label = NULL,
                         text_embeddings = NULL,
                         feature_extractor = NULL,
                         target_levels = NULL,
                         cls_pooling_features = 50,
                         cls_pooling_type = "min_max",
                         metric_type = "euclidean",
                         feat_act_fct="elu",
                         feat_size=50,
                         feat_bias=TRUE,
                         feat_dropout=0.0,
                         feat_parametrizations="None",
                         feat_normalization_type="layer_norm",
                         ng_conv_act_fct="elu",
                         ng_conv_n_layers=1,
                         ng_conv_ks_min=2,
                         ng_conv_ks_max=4,
                         ng_conv_bias=FALSE,
                         ng_conv_dropout=0.1,
                         ng_conv_parametrizations="None",
                         ng_conv_normalization_type="layer_norm",
                         ng_conv_residual_type="residual_gate",
                         dense_act_fct="elu",
                         dense_n_layers=1,
                         dense_dropout=0.5,
                         dense_bias=FALSE,
                         dense_parametrizations="None",
                         dense_normalization_type="layer_norm",
                         dense_residual_type="residual_gate",
                         rec_act_fct="tanh",
                         rec_n_layers=1,
                         rec_type="gru",
                         rec_bidirectional=FALSE,
                         rec_dropout=0.2,
                         rec_bias=FALSE,
                         rec_parametrizations="None",
                         rec_normalization_type="layer_norm",
                         rec_residual_type="residual_gate",
                         tf_act_fct="elu",
                         tf_dense_dim=50,
                         tf_n_layers=1,
                         tf_dropout_rate_1=0.1,
                         tf_dropout_rate_2=0.5,
                         tf_attention_type="multihead",
                         tf_embedding_type="absolute",
                         tf_num_heads=1,
                         tf_bias=FALSE,
                         tf_parametrizations="None",
                         tf_normalization_type="layer_norm",
                         tf_residual_type="residual_gate",
                         merge_attention_type = "multi_head",
                         merge_num_heads = 1,
                         merge_normalization_type="layer_norm",
                         embedding_dim = 2) {
      arguments <- get_called_args(n = 1)
      arguments$core_net_type <- "parallel"
      private$do_configuration(args = arguments, one_hot_encoding = FALSE)
    }
  ),
  private = list(
    # Private--------------------------------------------------------------------------
    create_reset_model = function() {
      private$check_config_for_TRUE()

      private$load_reload_python_scripts()

      self$model <- py$TEClassifierPrototype(
        features = as.integer(self$model_config$features),
        times = as.integer(self$model_config$times),
        target_levels = reticulate::np_array(seq(from = 0, to = (length(self$model_config$target_levels) - 1))),
        pad_value = as.integer(private$text_embedding_model$pad_value),
        skip_connection_type="None",
        cls_pooling_features = as.integer(self$model_config$cls_pooling_features),
        cls_pooling_type = self$model_config$cls_pooling_type,
        metric_type = self$model_config$metric_type,
        feat_act_fct=self$model_config$feat_act_fct,
        feat_size=as.integer(self$model_config$feat_size),
        feat_bias=self$model_config$feat_bias,
        feat_dropout=self$model_config$feat_dropout,
        feat_parametrizations=self$model_config$feat_parametrizations,
        feat_normalization_type=self$model_config$feat_normalization_type,
        ng_conv_act_fct=self$model_config$ng_conv_act_fct,
        ng_conv_n_layers=as.integer(self$model_config$ng_conv_n_layers),
        ng_conv_ks_min=as.integer(self$model_config$ng_conv_ks_min),
        ng_conv_ks_max=as.integer(self$model_config$ng_conv_ks_max),
        ng_conv_bias=self$model_config$ng_conv_bias,
        ng_conv_dropout=self$model_config$ng_conv_dropout,
        ng_conv_parametrizations=self$model_config$ng_conv_parametrizations,
        ng_conv_normalization_type=self$model_config$ng_conv_normalization_type,
        ng_conv_residual_type=self$model_config$ng_conv_residual_type,
        dense_act_fct=self$model_config$dense_act_fct,
        dense_n_layers=as.integer(self$model_config$dense_n_layers),
        dense_dropout=self$model_config$dense_dropout,
        dense_bias=self$model_config$dense_bias,
        dense_parametrizations=self$model_config$dense_parametrizations,
        dense_normalization_type=self$model_config$dense_normalization_type,
        dense_residual_type=self$model_config$dense_residual_type,
        rec_act_fct=self$model_config$rec_act_fct,
        rec_n_layers=as.integer(self$model_config$rec_n_layers),
        rec_type=self$model_config$rec_type,
        rec_bidirectional=self$model_config$rec_bidirectional,
        rec_dropout=self$model_config$rec_dropout,
        rec_bias=self$model_config$rec_bias,
        rec_parametrizations=self$model_config$rec_parametrizations,
        rec_normalization_type=self$model_config$rec_normalization_type,
        rec_residual_type=self$model_config$rec_residual_type,
        tf_act_fct=self$model_config$tf_act_fct,
        tf_dense_dim=as.integer(self$model_config$tf_dense_dim),
        tf_n_layers=as.integer(self$model_config$tf_n_layers),
        tf_dropout_rate_1=self$model_config$tf_dropout_rate_1,
        tf_dropout_rate_2=self$model_config$tf_dropout_rate_2,
        tf_attention_type=self$model_config$tf_attention_type,
        tf_embedding_type=self$model_config$tf_embedding_type,
        tf_num_heads=as.integer(self$model_config$tf_num_heads),
        tf_bias=self$model_config$tf_bias,
        tf_parametrizations=self$model_config$tf_parametrizations,
        tf_normalization_type=self$model_config$tf_normalization_type,
        tf_residual_type=self$model_config$tf_residual_type,
        merge_attention_type=self$model_config$merge_attention_type,
        merge_normalization_type=self$model_config$merge_normalization_type,
        embedding_dim = as.integer(self$model_config$embedding_dim),
        core_net_type = self$model_config$core_net_type
      )

      private$set_random_prototypes()
    },
    #--------------------------------------------------------------------------
    check_param_combinations_configuration = function() {},
    #--------------------------------------------------------------------------
    adjust_configuration = function() {
    }
  )
)

# Add Classifier to central index
TEClassifiers_class_names <- append(x = TEClassifiers_class_names, values = "TEClassifierParallelPrototype")
