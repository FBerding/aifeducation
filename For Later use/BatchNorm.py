#BatchNorm_with_Mask------------------------------------------------------------
#Layer generating the Batch Norm for sequential data.
# Returns a list with the following tensors
# * Input and output tensor (Batch, Times, Features) or (Batch, Features)
# * Sequence length of the tensors shape (Batch)
# * mask_times Mask on the level of complete sequences shape (Batch, Times)
# * mask_features Mask on the level of single features shape (Bath, Times, Features)
# True indicates that the sequence or feature is padded. If True these values should not be part 
# of further computations
class BatchNorm_with_Mask(torch.nn.Module):
    def __init__(self, features,pad_value,eps=1e-5,alpha=0.1):
      super().__init__()
      self.eps=eps
      self.alpha=alpha
      self.features = features
      if isinstance(pad_value, torch.Tensor):
        self.pad_value=pad_value.detach()
      else:
        self.pad_value=torch.tensor(pad_value)
      self.gamma = torch.nn.Parameter(torch.ones(1, 1, self.features))
      self.beta = torch.nn.Parameter(torch.zeros(1, 1, self.features))
      self.register_buffer("running_mean",torch.zeros((1, 1, self.features)))
      self.register_buffer("running_variance",torch.ones((1, 1, self.features)))

    def forward(self, x,mask_times=None):
      if x.dim()==2:
        xs=torch.unsqueeze(x,dim=1)
      else:
        xs=x
      if mask_times is None:
        mask_times=torch.zeros((xs.size(0),xs.size(1)),dtype=torch.bool,device=xs.device)
      #Set padding value to zero for correct sum
      mask_features=get_FeatureMask_from_mask(mask_times,self.features)
      x_zeros=xs*(~mask_features)
      gamma_expanded=self.gamma.expand(x_zeros.size(0),x_zeros.size(1),x_zeros.size(2))
      beta_expanded=self.beta.expand(x_zeros.size(0),x_zeros.size(1),x_zeros.size(2))
      if self.training==True: 
        if x_zeros.size(0)>=2:
          #Number of not padded elements
          n_elements=torch.sum(~mask_times,dim=(0,1))
          #Calc Batch Mean for every feature. Size is (Feature)
          batch_mean=torch.sum(x_zeros,dim=(0,1))/n_elements
          #Calc Batch Variance
          batch_variance=x_zeros-torch.unsqueeze(torch.unsqueeze(batch_mean,dim=0),dim=0).expand(x_zeros.size(0),x_zeros.size(1),x_zeros.size(2))
          batch_variance=torch.pow(batch_variance,2)
          batch_variance=(~mask_features)*batch_variance
          batch_variance=torch.sum(batch_variance,dim=(0,1))
          batch_variance=batch_variance/n_elements
          #Update running mean and variance
          self.running_mean=(1-self.alpha)*self.running_mean+self.alpha*torch.unsqueeze(torch.unsqueeze(batch_mean,dim=0),dim=0)
          self.running_variance=(1-self.alpha)*self.running_variance+self.alpha*(n_elements/(n_elements-1))*torch.unsqueeze(torch.unsqueeze(batch_variance,dim=0),dim=0)*(x_zeros.size(0)/(x_zeros.size(0)-1))
          #self.running_variance=torch.clamp(self.running_variance,min=0.0,max=None)
          #Normalize Scale and shift
          y=gamma_expanded*(x_zeros-batch_mean)/(torch.sqrt(batch_variance)+self.eps)+beta_expanded
        else:
          #Normalize Scale and shift
          y=gamma_expanded*(x_zeros-self.running_mean)/(torch.sqrt(self.running_variance)+self.eps)+beta_expanded
          #y=x_zeros
      else:
        #Normalize Scale and shift
        y=gamma_expanded*(x_zeros- self.running_mean)/(torch.sqrt(self.running_variance)+self.eps)+beta_expanded
      #Insert padding values
      normalized=y.masked_fill(mask=mask_features, value=self.pad_value)
      if x.dim()==2:
        normalized=torch.squeeze(normalized,dim=1)
      #Return results
      return normalized, mask_times


# BatchNorm with Mask-----------------------------------------------------------
test_that("BatchNorm with Mask", {
  device <- ifelse(torch$cuda$is_available(), "cuda", "cpu")
  pad_value <- sample(x = seq(from = -200, to = -10, by = 10), size = 1)
  times <- sample(x = seq(from = 3, to = 10, by = 1), size = 1)
  features <- sample(x = seq(from = 3, to = 1024, by = 1), size = 1)
  sequence_length <- sample(x = seq(from = 1, to = times, by = 1), size = 30, replace = TRUE)
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = sequence_length,
    pad_value = pad_value
  )$to(device)
  masking_layer <- py$masking_layer(pad_value)$to(device)
  values <- masking_layer(example_tensor)

  layer <- py$BatchNorm_with_Mask(
    features = as.integer(features),
    pad_value = as.integer(pad_value),
    alpha=0.1,
    eps = 1e-05
  )$to(device)
  layer$eval()
  y <- layer(
    x = values[[1]],
    mask_times = values[[2]]
  )

  # Test that masking values are the same
  expect_equal(tensor_to_numpy(y[[2]]), tensor_to_numpy(values[[2]]))

  # Test that padding is not destroyed
  y_2 <- masking_layer(y[[1]])
  expect_equal(tensor_to_numpy(y[[2]]), tensor_to_numpy(y_2[[2]]))

  # Test that the computations are correct for sequences with full length
  example_tensor <- generate_tensors(
    times = times,
    features = features,
    seq_len = rep.int(times, times = 10),
    pad_value = pad_value
  )$to(device)
  comparison_layer <- torch$nn$BatchNorm1d(
    num_features = example_tensor$size(2L),
    eps = 1e-05,
    affine = TRUE,
    momentum=0.1,
    track_running_stats=TRUE,
    device = NULL,
    dtype = example_tensor$dtype
  )$to(device)
  comparison_layer$eval()
  res_expected <- tensor_to_numpy(
    torch$permute(
      comparison_layer(
        torch$permute(
          example_tensor,reticulate::tuple(list(0L,2L,1L))
        )
      ),
      reticulate::tuple(list(0L,2L,1L))
    )
  )
  values <- masking_layer(example_tensor)

  results <- tensor_to_numpy(layer(
    x = values[[1]],
    mask_times = values[[2]]
  )[[1]])
  expect_equal(results, res_expected, tolerance = 1e-4)
})
