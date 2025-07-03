#' @family Utils Documentation
get_layer_description <- function(layer) {
  documentation <- list()

  documentation$tf_layers <- list(
    title = "Transformer Encoder Layers",
    desc = "The transformer encoder layers follow the structure of the encoder layers
    used in transformer models. A single layer is designed as described by Chollet, Kalinowski, and Allaire (2022, p.373) with
    the exception that single components of the layers (such as the activation function,
    the kind of residual connection, the kind of normalization or the kind of attention) can be freely chosen.",
    img = NULL,
    references = NULL,
    param_prefix = "tf_"
  )

  documentation$feature_layers <- list(
    title = "Feature Layer",
    desc = "The feature layer is a dense layer that can be used to
    increase or decrease the number of features of the input data before passing the
    data into your model. The aim of this layer is to increase or reduce the complexity of the data for your model.
    The output size of this layer determines the number of features for all following layers.",
    img = NULL,
    references = NULL,
    param_prefix = "feat_"
  )

  documentation$dense_layers <- list(
    title = "Dense Layers",
    desc = "A fully connected layer. The layer is applied to every step of a sequence.",
    img = NULL,
    references = NULL,
    param_prefix = "dense_"
  )

  documentation$n_gram_layers <- list(
    title = "Multiple N-Gram Layers",
    desc = "This type of layer focuses on sub-sequence and performs an 1d convolutional operation. On a word and token level
    these sub-sequences can be interpreted as n-grams (Jacovi, Shalom & Goldberg 2018). The convolution is done across all features.
    The number of filters equals the number of features of the input tensor. Thus, the shape of the tensor is retained (Pham, Kruszewski & Boleda 2016).
    \n The layer is able to consider multiple n-grams at the same time. In this case the convolution of the n-grams is done
    seprately and the resulting tensors are concatenated along the feature dimension. The number of filters for every n-gram
    is set to num_features/num_n-grams. Thus, the resulting tensor has the same shape as the input tensor.
    \n Sub-sequences that are masked in the input are
    also masked in the output.
    \n The output of this layer can be understand as the results of the n-gram filters. Stacking this layer
    allows the model to perform n-gram detection of n-grams (meta perspective).",
    img = NULL,
    references = c(
      "Jacovi, A., Shalom, O. S. & Goldberg, Y. (2018). Understanding Convolutional Neural Networks for Text Classification. https://doi.org/10.48550/arXiv.1809.08037",
      "Pham, N.‑Q., Kruszewski, G. & Boleda, G. (2016). Convolutional Neural Network Language Models. In J. Su, K. Duh & X. Carreras (Hrsg.), Proceedings of the 2016 Conference on Empirical Methods in Natural Language Processing (S. 1153–1162). Association for Computational Linguistics. https://doi.org/10.18653/v1/D16-1123"
    ),
    param_prefix = "ng_conv_"
  )

  documentation$rec_layers <- list(
    title = "Recurrent Layers",
    desc = "A regular recurrent layer either as Gated Recurrent Unit (GRU) or Long Short-Term Memory (LSTM) layer. Uses
    PyTorchs implementation.",
    img = NULL,
    references = NULL,
    param_prefix = "rec_"
  )

  documentation$cls_pooling_layer <- list(
    title = "Classifiction Pooling Layer",
    desc = "Layer transforms sequences into a lower dimensional space that can be passed to dense layers. It
    performs two types of pooling. First, it extractes features across the time dimension selecting the maximal
    and/or minimal features. Second, it performs pooling over the remaining features selecting a speficifc number of
    the heighest and/or lowest features.
    \n In the case of selecting the minmal *and* maximal features at the same time in the first step the minmal
    features are concatenated to the tensor resulting the in the shape (Batch, Times, 2*Features). In the second step the
    number of requested features is halved. The first half is used for the maximal features and the second for the minimal
    features.",
    img = NULL,
    references = NULL,
    param_prefix = "cls_pooling_"
  )

  documentation$merge_layer <- list(
    title = "Merge Layer",
    desc = "Layer for combining the output of different layers. All inputs must be sequential data of shape (Batch, Times, Features).
    First, pooling over time is applied extracting the minimal and/or maximal features.
    Second, the pooled tensors are combined by calculating their weighted sum. Different attention mechanism can be used
    to dynamically calculate the corresponding weights. This allows the model to decide which part of the data is most usefull.
    Finally, pooling over features is applied extracting a specific number of maximal and/or minimal features.",
    img = NULL,
    references = NULL,
    param_prefix = "merge_"
  )

  if(layer!="all"){
    return(documentation[[layer]])
  } else {
    return(documentation)
  }

}

get_desc_cls_type=function(cls_type){
  if(cls_type=="prob"){
    desc="This is a probability classifier that predicts a probability distribution for
    different classes/categories. This is the standard case most common in literature."
  } else if (cls_type=="prototype"){
    desc="This object is a metric based classifer and represents in implementation of a prototypical network for
    few-shot learning as described by Snell,
   Swersky, and Zemel (2017). The network uses a multi way contrastive loss described by Zhang et al. (2019). The
   network learns to scale the metric as described by Oreshkin, Rodriguez, and Lacoste (2018)."
  }
  return(desc)
}

get_input_desc=function(input_type){
  if(input_type=="text_embeddings"){
    desc="For the creation and training of a
   classifier an object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings] on the one hand and a [factor] on
   the other hand are necessary.
   \n The object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings]  contains the numerical text representations
   (text embeddings) of the raw texts generated by an object of class [TextEmbeddingModel]. For supporting large data
   sets it is recommended to use [LargeDataSetForTextEmbeddings] instead of [EmbeddedText].
   \n The `factor` contains the classes/categories for every text. Missing values (unlabeled cases) are supported and can
   be used for pseudo labeling.
   \n For predictions an object of class [EmbeddedText] or [LargeDataSetForTextEmbeddings] has to be used which was
   created with the same [TextEmbeddingModel] as for training."
  }
}

#-------------------------------------------------------------------------------
#' @family Utils Documentation
get_layer_documentation <- function(layer_name, inc_img = FALSE, inc_params = FALSE, inc_references = FALSE) {
  current_doc <- get_layer_description(layer_name)
  param_dict <- get_param_dict()

  relevant_params_index <- stringi::stri_detect(
    str = names(param_dict),
    regex = paste0("^", current_doc$param_prefix)
  )
  relevant_params <- subset(x = names(param_dict), subset = relevant_params_index)

  # General description--------------------------------------------------------
  # Title
  title <- paste0("**", current_doc$title, "**\n\n")
  # Description general
  desc <- paste0(
    current_doc$desc," ",
    "All parameters with the prefix *", current_doc$param_prefix, "* can be used to configure this layer.",
    "\n"
  )
  # Description of all parameters
  param_desc <- NULL
  # Parameter Documentation---------------------------------------------------
  if (inc_params == TRUE) {
    for (i in seq_along(relevant_params)) {
      selected_param <- relevant_params[[i]]
      tmp_layer_desc <- NULL
      # Add description
      tmp_layer_desc <- paste0(
        tmp_layer_desc,
        "- *", selected_param, "*: ",
        param_dict[[selected_param]]$desc, "\n"
      )


      if (!is.null(param_dict[[selected_param]]$values_desc)) {
        tmp_layer_desc <- paste0(
          tmp_layer_desc,
          "Allowed values:\n\n"
        )
        for (j in seq_along(param_dict[[selected_param]]$values_desc)) {
          tmp_layer_desc <- paste0(
            tmp_layer_desc,
            "\t - ",
            "`", names(param_dict[[selected_param]]$values_desc)[j], "`", ": ",
            param_dict[[selected_param]]$values_desc[[j]], "\n"
          )
        }
      }
      param_desc <- paste0(
        param_desc, "\n",
        tmp_layer_desc
      )
    }
  }


  # Gather documentation elements---------------------------------------------
  markdown_doc <- paste0(
    "\n",
    title,
    desc,
    param_desc
  )
  return(markdown_doc)
}

#' @family Utils Documentation
get_desc_for_core_model_architecture <- function(name) {
  documentation <- list()

  documentation$sequential <- list(
    title = "Sequential Core Architecture",
    desc = "This model is based on a sequential architecture.
  The input is passed to a specific number of layers step by step.
  All layers are grouped by the kind of layers into stacks."
  )

  documentation$parallel <- list(
    title = "Parallel Core Architecture",
    desc = "This model is based on a parallel architecture.
  An input is passed to different types of layers separately. At the end the outputs
  are combined to create the final output of the whole model."
  )

  markdown_doc <- paste0(
    "\n",
    "**", documentation[[name]]$title, "**\n\n",
    documentation[[name]]$desc, "\n"
  )

  return(markdown_doc)
}

#' @family Utils Documentation
build_documentation_for_model <- function(model_name,cls_type=NULL,core_type=NULL,input_type="text_embeddings") {
  layer_dict=get_layer_description("all")
  prefixes=NULL
  for(i in seq_along(layer_dict)){
    prefixes=append(x=prefixes,values = layer_dict[[i]]$param_prefix)
  }

  layer_included=vector(length = length(prefixes))
  names(layer_included)=names(layer_dict)

  model=create_object(model_name)
  params=rlang::fn_fmls_names(model$configure)

  for(i in seq_along(layer_included)){
    check_inlucded=stringi::stri_detect(str = params,regex = paste0("^", prefixes[i]))
    if(sum(check_inlucded)>0){
      layer_included[i]=TRUE
    } else {
      layer_included[i]=FALSE
    }
  }

  model_documentation=NULL

  #CLS Type
  desc_cls_type=NULL
  if(!is.null(cls_type)){
    model_documentation=paste0("**Classification Type**\n\n",
    desc_cls_type=get_desc_cls_type(cls_type)
    )
  }

  #Core Architecture
  if(!is.null(core_type)){
    model_documentation=paste0(
      model_documentation,"\n\n",
      get_desc_for_core_model_architecture(core_type)
    )
  }

  #Layer Description
  for(i in seq_along(layer_included)){
    if(layer_included[i]==TRUE){
      model_documentation=paste0(
        model_documentation,"\n",
        get_layer_documentation(names(layer_included)[i])
      )
    }
  }

  #Input and Prediction
  desc_input_and_predict=NULL
  if(!is.null(input_type)){
    desc_input_and_predict=paste0(
      "**Training and Prediction**\n\n",
      get_input_desc(input_type)
    )
  }
  model_documentation=paste0(
    model_documentation,"\n",
    desc_input_and_predict
  )
  return(model_documentation)

}
