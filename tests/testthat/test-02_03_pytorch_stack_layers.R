testthat::skip_on_cran()
testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

#Load python scripts
load_all_py_scripts()

# stack_dense_layer----------------------------------------------------
test_that("stack_dense_layer", {
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

  layer <- py$stack_dense_layer(
    times = as.integer(times),
    hidden_size = as.integer(features),
    n_layers = 3L,
    dropout = 0.3,
    act_fct = "elu",
    bias = TRUE,
    normalization_type = "layer_norm",
    pad_value=as.integer(pad_value),
    parametrizations = "None",
    dtype=values[[1]]$dtype,
    device="cpu",
    residual_type="residual_gate"
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

})

#Stack Reccurent layers----------------------------------------------------
test_that("stack_rec_layers", {
  rec_types=c("gru","lstm")
  bidirectional_types=c(TRUE,FALSE)
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

  for (rec_type in rec_types){
    for(bidirectional in bidirectional_types){
      layer <- py$stack_recurrent_layers(
        times = as.integer(times),
        hidden_size = as.integer(features),
        n_layers = 3L,
        dropout = 0.3,
        rec_type=rec_type,
        rec_bidirectional=bidirectional,
        pad_value=as.integer(pad_value),
        bias = TRUE,
        parametrizations = "None",
        dtype=values[[1]]$dtype,
        device="cpu",
        residual_type="residual_gate",
        return_sequence=TRUE
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
      expect_equal(y[[4]]$numpy(), values[[4]]$numpy())

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


#stack_tf_encoder_layer----------------------------------------------------
test_that("stack_tf_encoder_layer", {
  attention_types=c("multihead","fourier")
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

  for (attention_type in attention_types){
    layer <- py$stack_tf_encoder_layer(
      dense_dim=as.integer(4*features),
      attention_type=attention_type,
      num_heads=2L,
      times = as.integer(times),
      features = as.integer(features),
      n_layers = 3L,
      dropout_rate_1 = 0.1,
      dropout_rate_2=0.2,
      pad_value=as.integer(pad_value),
      positional_embedding="absolute",
      bias = TRUE,
      parametrizations = "None",
      dtype=values[[1]]$dtype,
      device="cpu",
      residual_type="residual_gate"
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

#stack_n_gram_convolution----------------------------------------------------
test_that("stack_n_gram_convolution", {
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

  for(max_n_gram in 3:times){
    layer=py$stack_n_gram_convolution(
      ks_min=2L,
      ks_max=as.integer(max_n_gram),
      times=as.integer(times),
      features=as.integer(features),
      n_layers=3L,
      pad_value=as.integer(pad_value),
      bias=TRUE,
      parametrizations="None",
      dtype=values[[1]]$dtype,
      device="cpu",
      act_fct="elu",
      residual_type="residual_gate"
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

    #Test method return shape
    expect_equal(dim(y_1[[1]]$detach()$numpy()),c(length(sequence_length),times,features))
  }
})
