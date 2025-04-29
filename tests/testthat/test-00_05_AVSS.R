# === Random data =================================================================
set.seed(1231) # regenerate the same random result
my_array1 <- array(runif(2 * 5 * 3), dim = c(2, 5, 3))
set.seed(1232)
my_array2 <- array(runif(2 * 5 * 3), dim = c(2, 5, 3))
hidden_states_random <- list()
hidden_states_random[[1]] <- my_array1
hidden_states_random[[2]] <- my_array2

e <- 0.5
layer_index <- 1

all_layers_random <- list()
for (i in seq_along(hidden_states_random)) {
  all_layers_random[[i]] <- evaluate_layer_importance(hidden_states_random, i, e)
}

all_layers_random_df <- do.call(rbind, lapply(all_layers_random, as.data.frame))
all_layers_random_df

# === Real data ===================================================================
model_dir <- "Trial/mpnet_v1"
load_safe <- TRUE
from_tf <- FALSE

reticulate::py_run_file(system.file("python/MPNetForMPLM_PT.py",
                                    package = "aifeducation"
))

tokenizer <- transformers$AutoTokenizer$from_pretrained(model_dir)

model <- transformers$MPNetModel$from_pretrained(
  model_dir,
  from_tf = from_tf,
  use_safetensors = load_safe
)

inputs <- tokenizer(
  "This is the first sentance",
  return_tensors = "pt"
)$data

with(torch$no_grad(), {
  outputs <- model$`__call__`(!!!inputs, output_hidden_states = TRUE)
  hidden_states <- outputs$hidden_states
})

hidden_states_list <- list()
for (i in seq_along(hidden_states)) {
  py_tensor <- hidden_states[[i]]
  r_array <- reticulate::py_to_r(py_tensor$detach()$cpu()$numpy())
  hidden_states_list[[i]] <- r_array
}

e <- 0.5
layer_index <- 1
all_layers <- list()
for (i in seq_along(hidden_states_list)) {
  all_layers[[i]] <- evaluate_layer_importance(hidden_states_list, i, e)
}

all_layers_df <- do.call(rbind, lapply(all_layers, as.data.frame))
all_layers_df
