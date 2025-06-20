testthat::skip_on_cran()
testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

#Load python scripts
load_all_py_scripts()

#Prototyp Metric---------------------------------------------------------------
test_that("Prototype Metric", {
  layer <- py$layer_protonet_metric()

  samples <- matrix(
    data = c(
      0.59, 0.71, 0.51,
      0.45, -1, -0.77,
      0.27, 0.11, -0.99,
      -0.15, -0.61, -0.89,
      0.39, 0.57, -0.31,
      0.57, 0.21, 0.46,
      -0.59, 0.44, -0.06,
      -0.85, 0.45, 0.94,
      0.1, -0.36, 0.49
    ),
    nrow = 9,
    ncol = 3,
    byrow = TRUE
  )
  samples <- reticulate::np_array(samples)
  samples <- torch$from_numpy(
    samples
  )

  prototypes <- matrix(
    data = c(
      0.436667,   -0.060000,      -0.416667,
      0.270000,   0.056667,       -0.246667,
      -0.446667,  0.176667,       0.456667
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  prototypes <- reticulate::np_array(prototypes)
  prototypes <- torch$from_numpy(
    prototypes
  )
  result_matrix <- matrix(data = c(
    1.214546097,        1.049661321,    1.167033276,
    1.004301858,        1.192821119,    1.921787363,
    0.620796979,        0.745244181,    1.615827961,
    0.933124977,        1.017212968,    1.587566272,
    0.640668054,        0.530963485,    1.201041215,
    0.926942405,        0.782872205,    1.01721843,
    1.196350933,        0.95989004,     0.597355282,
    1.938080379,        1.678478147,    0.686294397,
    1.012614877,        0.863243239,    0.766789845
  ),
  nrow = 9,
  ncol = 3,
  byrow = TRUE)

  scaling_factor <- layer$get_scaling_factor()

  distances <- layer(x = samples, prototypes = prototypes)
  expect_equal(
    object = distances$detach()$numpy(),
    expected = result_matrix,
    tolerance = 1e-6
  )
})

# Masking Layer-----------------------------------------------------------------
test_that("Masking Layer", {
  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )

  layer <- py$masking_layer(pad_value)
  y <- layer(example_tensor)

  # Check if input is the same as the output
  expect_equal(y[[1]]$numpy(), example_tensor$numpy())

  # Check sequence length
  expect_equal(as.numeric(y[[2]]$numpy()), sequence_length)

  # Check Masking times
  expect_equal(rowSums(y[[3]]$numpy()), times - sequence_length)
})

# Identity Layer----------------------------------------------------------------
test_that("identity layer", {
  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  layer <- py$identity_layer()
  y <- layer(
    x = values[[1]],
    seq_len = values[[2]],
    mask_times = values[[3]],
    mask_features = values[[4]]
  )

  # Test that  values are the same
  expect_equal(y[[1]]$numpy(), values[[1]]$numpy())
  expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), values[[3]]$numpy())
  expect_equal(y[[4]]$numpy(), values[[4]]$numpy())

  # Test that padding is not destroyed
  y_2 <- masking_layer(y[[1]])
  expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
  expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())
})
# Residual connection with Mask-----------------------------------------------------------
test_that("residual connection with Mask", {
  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  types <- c("None", "addition", "residual_gate")
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  for (type in types) {
    layer <- py$layer_residual_connection(
      type = type,
      pad_value = as.integer(pad_value)
    )
    y <- layer(
      x = values[[1]],
      y = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )
    layer$eval()

    # Test that masking values are the same
    expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
    expect_equal(y[[3]]$numpy(), values[[3]]$numpy())
    expect_equal(y[[4]]$numpy(), values[[4]]$numpy())

    # Test that padding is not destroyed
    y_2 <- masking_layer(y[[1]])
    expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
    expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
    expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())

    # Test that values do not change at random for same input
    y_1 <- layer(
      x = values[[1]],
      y = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )
    y_2 <- layer(
      x = values[[1]],
      y = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )
    expect_equal(y_1[[1]]$detach()$numpy(), y_2[[1]]$detach()$numpy())
  }
})

# LayerNorm with Mask-----------------------------------------------------------
test_that("LayerNorm with Mask", {
  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  layer <- py$LayerNorm_with_Mask(
    times = as.integer(times),
    features = as.integer(features),
    pad_value = as.integer(pad_value)
  )
  y <- layer(
    x = values[[1]],
    seq_len = values[[2]],
    mask_times = values[[3]],
    mask_features = values[[4]]
  )

  # Test that masking values are the same
  expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), values[[3]]$numpy())
  expect_equal(y[[4]]$numpy(), values[[4]]$numpy())

  # Test that padding is not destroyed
  y_2 <- masking_layer(y[[1]])
  expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
  expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())

  # Test that the computations are correct for sequences with full length
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = rep.int(times, times = 10),
    pad_value = pad_value
  )
  comparison_layer <- torch$nn$LayerNorm(
    normalized_shape = example_tensor$size(2L),
    eps = 1e-05,
    elementwise_affine = TRUE,
    bias = TRUE,
    device = NULL,
    dtype = example_tensor$dtype
  )
  res_expected <- comparison_layer(example_tensor)$detach()
  values <- masking_layer(example_tensor)
  results <- layer(
    x = values[[1]],
    seq_len = values[[2]],
    mask_times = values[[3]],
    mask_features = values[[4]]
  )[[1]]$detach()
  expect_equal(results$numpy(), res_expected$numpy(),
    tolerance = 1e-5
  )
})

# Dense Layer with Mask-----------------------------------------------------------
test_that("DenseLayer with Mask", {
  normalization_types <- c("None", "layer_norm")
  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  # Test for equal, more, and fewer features as input size
  features_output <- c(
    features,
    sample(x = seq(from = 1, to = (features - 1)), size = 1),
    sample(x = seq(from = (features + 1), to = 2 * features), size = 1)
  )
  for (norm_types in normalization_types) {
    for (target_features in features_output) {
      # Create layer
      layer <- py$dense_layer_with_mask(
        input_size = as.integer(features),
        output_size = as.integer(target_features),
        times = as.integer(times),
        pad_value = as.integer(pad_value),
        act_fct = "elu",
        dropout = 0.3,
        bias = TRUE,
        parametrizations = "None",
        dtype = values[[1]]$dtype,
        residual_type = "None",
        normalization_type = norm_types
      )
      layer$eval()

      y <- layer(
        x = values[[1]],
        seq_len = values[[2]],
        mask_times = values[[3]],
        mask_features = values[[4]]
      )

      # Test that masking values are the same
      expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
      expect_equal(y[[3]]$numpy(), values[[3]]$numpy())

      # Test the correct size of the new masking on feature level
      expect_equal(dim(y[[4]]$numpy())[3], target_features)

      # Test that padding is not destroyed
      y_2 <- masking_layer(y[[1]])
      expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
      expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
      expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())

      # Test that values do not change at random for same input
      y_1 <- layer(
        x = values[[1]],
        seq_len = values[[2]],
        mask_times = values[[3]],
        mask_features = values[[4]]
      )
      y_2 <- layer(
        x = values[[1]],
        seq_len = values[[2]],
        mask_times = values[[3]],
        mask_features = values[[4]]
      )
      expect_equal(y_1[[1]]$detach()$numpy(), y_2[[1]]$detach()$numpy())
    }
  }
})

# layer_tf_encoder-------------------------------------------------------
test_that("layer_tf_encoder", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
      package = "aifeducation"
    )
  )

  attention_types <- c("multi_head", "fourier")

  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 4, to = 1024, by = 2), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  # Test for equal, more, and fewer features as input size
  features_output <- c(
    features,
    sample(x = seq(from = 1, to = (features - 1)), size = 1),
    sample(x = seq(from = (features + 1), to = 2 * features), size = 1)
  )

  for (attention_type in attention_types) {
    # Create layer
    layer <- py$layer_tf_encoder(
      dense_dim = 38L,
      times = as.integer(times),
      pad_value = as.integer(pad_value),
      attention_type = attention_type,
      features = as.integer(features),
      normalization_type = "layer_norm",
      residual_type = "residual_gate",
      num_heads = as.integer(2),
      act_fct = "elu",
      dropout_rate_1 = 0.3,
      dropout_rate_2 = 0.3,
      bias = TRUE,
      parametrizations = "None",
      dtype = values[[1]]$dtype,
    )
    layer$eval()

    y <- layer(
      x = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )

    # Test that masking values are the same
    expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
    expect_equal(y[[3]]$numpy(), values[[3]]$numpy())

    # Test that padding is not destroyed
    y_2 <- masking_layer(y[[1]])
    expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
    expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
    expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())

    # Test that values do not change at random for same input
    y_1 <- layer(
      x = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )
    y_2 <- layer(
      x = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )
    expect_equal(y_1[[1]]$detach()$numpy(), y_2[[1]]$detach()$numpy())
  }
})

# exreme_pooling_over_time------------------------------------------------------------
test_that("exreme_pooling_over_time", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
      package = "aifeducation"
    )
  )

  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  for (pooling_type in c("max", "min", "min_max")) {
    layer <- py$exreme_pooling_over_time(
      times = as.integer(times),
      features = as.integer(features),
      pad_value = as.integer(pad_value),
      pooling_type = pooling_type
    )
    y_1 <- layer(values[[1]], values[[4]])
    if (pooling_type != "min_max") {
      expect_equal(dim(y_1$detach()$numpy()), c(length(sequence_length), features))
    } else {
      expect_equal(dim(y_1$detach()$numpy()), c(length(sequence_length), 2 * features))
    }
  }
})

# layer_adaptive_extreme_pooling_1d---------------------------------------------
test_that("layer_adaptive_extreme_pooling_1d", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
      package = "aifeducation"
    )
  )

  tensor <- torch$rand(30L, 768L)
  output_size <- 10

  for (pooling_type in c("max", "min", "min_max")) {
    layer <- py$layer_adaptive_extreme_pooling_1d(
      output_size = as.integer(output_size),
      pooling_type = pooling_type
    )
    result <- layer(tensor)

    # Check the corres output shape
    expect_equal(ncol(result$numpy()), output_size)


    # Check if the correct values are selected
    result_matrix <- result$numpy()
    tensor_matrix <- tensor$numpy()
    for (i in 1:nrow(result_matrix)) {
      if (pooling_type == "max") {
        ordered_values <- tensor_matrix[i, order(tensor_matrix[i, ], decreasing = TRUE)]
        relevant_values <- ordered_values[seq(from = 1, to = output_size)]
        expect_equal(sum(round(result_matrix[i,],digits=5) %in% round(relevant_values,digits=5)), output_size)
      } else if (pooling_type == "min") {
        ordered_values <- tensor_matrix[i, order(tensor_matrix[i, ], decreasing = FALSE)]
        relevant_values <- ordered_values[seq(from = 1, to = output_size)]
        expect_equal(sum(round(result_matrix[i,],digits=5) %in% round(relevant_values,digits=5)), output_size)
      } else {
        n_max=ceiling(output_size/2)
        n_min=output_size-n_max

        #Max
        ordered_values_max <- tensor_matrix[i, order(tensor_matrix[i, ], decreasing = TRUE)]
        relevant_values_max <- ordered_values_max[seq(from = 1, to = n_max)]
        expect_equal(sum(round(result_matrix[i,],digits=5) %in% round(relevant_values_max,digits=5)), n_max)

        #Min
        ordered_values_min <- tensor_matrix[i, order(tensor_matrix[i, ], decreasing = FALSE)]
        relevant_values_min <- ordered_values_min[seq(from = 1, to = n_min)]
        expect_equal(sum(round(result_matrix[i,],digits=5) %in% round(relevant_values_min,digits=5)), n_min)

      }
    }
  }
})

# Layer layer_n_gram_convolution--------------------------------------------------
test_that("layer_n_gram_convolution", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
      package = "aifeducation"
    )
  )

  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  n_filter <- sample(x = seq(from = 2, to = features, by = 1), size = 1)
  layer <- py$layer_n_gram_convolution(
    kernel_size_times = as.integer(2),
    times = as.integer(times),
    features = as.integer(features),
    pad_value = as.integer(pad_value),
    n_filter = as.integer(n_filter),
    bias = TRUE,
    parametrizations = "None",
    dtype = values[[1]]$dtype,
    device = "cpu"
  )
  layer$eval()

  y <- layer(
    x = values[[1]],
    seq_len = values[[2]],
    mask_times = values[[3]],
    mask_features = values[[4]]
  )

  # Test that masking values are the same
  expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), values[[3]]$numpy())

  # Test the correct size of the new masking on feature level
  expect_equal(dim(y[[4]]$numpy())[3], n_filter)

  # Test that padding is not destroyed
  y_2 <- masking_layer(y[[1]])
  expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
  expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())

  # Test that values do not change at random for same input
  y_1 <- layer(
    x = values[[1]],
    seq_len = values[[2]],
    mask_times = values[[3]],
    mask_features = values[[4]]
  )
  y_2 <- layer(
    x = values[[1]],
    seq_len = values[[2]],
    mask_times = values[[3]],
    mask_features = values[[4]]
  )
  expect_equal(y_1[[1]]$detach()$numpy(), y_2[[1]]$detach()$numpy())

  # Test method return shape
  expect_equal(dim(y_1[[1]]$detach()$numpy()), c(length(sequence_length), times, n_filter))
})

# Layer layer_mutiple_n_gram_convolution--------------------------------------------------
test_that("layer_mutiple_n_gram_convolution", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
      package = "aifeducation"
    )
  )

  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)

  for (max_n_gram in 3:times) {
    layer <- py$layer_mutiple_n_gram_convolution(
      ks_min = 2L,
      ks_max = as.integer(max_n_gram),
      times = as.integer(times),
      features = as.integer(features),
      pad_value = as.integer(pad_value),
      bias = TRUE,
      parametrizations = "None",
      dtype = values[[1]]$dtype,
      device = "cpu",
      act_fct = "elu"
    )
    layer$eval()

    y <- layer(
      x = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )

    # Test that masking values are the same
    expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
    expect_equal(y[[3]]$numpy(), values[[3]]$numpy())

    # Test the correct size of the new masking on feature level
    expect_equal(dim(y[[4]]$numpy())[3], features)

    # Test that padding is not destroyed
    y_2 <- masking_layer(y[[1]])
    expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
    expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
    expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())

    # Test that values do not change at random for same input
    y_1 <- layer(
      x = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )
    y_2 <- layer(
      x = values[[1]],
      seq_len = values[[2]],
      mask_times = values[[3]],
      mask_features = values[[4]]
    )
    expect_equal(y_1[[1]]$detach()$numpy(), y_2[[1]]$detach()$numpy())

    # Test method return shape
    expect_equal(dim(y_1[[1]]$detach()$numpy()), c(length(sequence_length), times, features))
  }
})

# Layer merge leyer---------------------------------------
test_that("merge_layer", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
      package = "aifeducation"
    )
  )

  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 52, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  n_input_streams <- sample(seq(from = 2, to = 10, by = 1), size = 1)
  n_extracted_features <- sample(seq(from = 2, to = features, by = 1), size = 1)
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)
  for (pooling_type in c("max", "min", "min_max")) {
    layer <- py$merge_layer(
      times = as.integer(times),
      features = as.integer(features),
      n_extracted_features = as.integer(n_extracted_features),
      n_input_streams = as.integer(n_input_streams),
      pad_value = as.integer(pad_value),
      pooling_type = pooling_type,
      attention_type = "multi_head",
      num_heads = 1L,
      dtype = values[[1]]$dtype,
      device = "cpu"
    )
    layer$eval()

    y <- layer(
      tensor_list = rep(values[1], times = n_input_streams),
      mask_features = values[[4]]
    )

    # Test the correct shape
    expect_equal(dim(y$detach()$numpy()), c(length(sequence_length), n_extracted_features))
  }
})

# Layer rnn preparation Layer---------------------------------------
test_that("rnn_preparation", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
      package = "aifeducation"
    )
  )

  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )
  masking_layer <- py$masking_layer(pad_value)
  values <- masking_layer(example_tensor)


  layer_do <- py$layer_pack_and_masking()
  layer_do$eval()

  layer_undo <- py$layer_unpack_and_masking(
    sequence_length = as.integer(times),
    pad_value = pad_value
  )
  layer_undo$eval()

  y <- layer_do(
    x = values[[1]],
    seq_len = values[[2]],
    mask_times = values[[3]],
    mask_features = values[[4]]
  )
  y <- layer_undo(
    x = y[[1]],
    seq_len = y[[2]],
    mask_times = y[[3]],
    mask_features = y[[4]]
  )

  # Test that masking values are the same
  expect_equal(y[[2]]$numpy(), values[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), values[[3]]$numpy())

  # Test the correct size of the new masking on feature level
  expect_equal(dim(y[[4]]$numpy())[3], features)

  # Test that padding is not destroyed
  y_2 <- masking_layer(y[[1]])
  expect_equal(y[[2]]$numpy(), y_2[[2]]$numpy())
  expect_equal(y[[3]]$numpy(), y_2[[3]]$numpy())
  expect_equal(y[[4]]$numpy(), y_2[[4]]$numpy())
})

test_that("layer_class_mean", {
  reticulate::py_run_file(
    system.file("python/pytorch_te_parallel.py",
                package = "aifeducation"
    )
  )

  layer=py$layer_class_mean()


  test_tensor=matrix(
    data=c(0.59,	0.71,	0.51,
    0.45,	-1,	-0.77,
    0.27,	0.11,	-0.99,
    -0.15,	-0.61,	-0.89,
    0.39,	0.57,	-0.31,
    0.57,	0.21,	0.46,
    -0.59,	0.44,	-0.06,
    -0.85,	0.45,	0.94,
    0.1,	-0.36,	0.49
  ),nrow=9,ncol=3,byrow=TRUE)
  test_tensor=reticulate::np_array(test_tensor)
  test_tensor=torch$from_numpy(
    test_tensor
  )
  test_classes=torch$from_numpy(reticulate::np_array(c(0,0,0,1,1,1,2,2,2)))
  num_classes=3

  result_matrix=matrix(data=c(
    0.436667,	-0.060000,	-0.416667,
    0.270000,	0.056667,	-0.246667,
    -0.446667,	0.176667,	0.456667
  ),
  nrow=3,ncol=3,byrow=TRUE)

  cls_means=layer(x=test_tensor,
                  classes=test_classes,
                  total_classes=3L)
  expect_equal(
    object = cls_means$numpy(),
    expected = result_matrix,
    tolerance = 1e-5
    )
})

