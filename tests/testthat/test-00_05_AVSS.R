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

texts <- c(
  "This is the first sentance",
  "This is the second sentance",
  "This is the third sentance"
)

# === for one layer
batch_size <- 2
layer_index <- 1
e <- 0.5

layer_importance <- evaluate_layer_importance_all_batches(
  model = model, tokenizer = tokenizer, dataset = texts, batch_size = batch_size,
  layer_index = layer_index, e = e
)

layer_importance_df <- as.data.frame(layer_importance)
layer_importance_df

# === for some layers
layer_num <- 5
all_layers <- NULL

for (i in seq_len(layer_num)) {
  layer_importance <- evaluate_layer_importance_all_batches(
    model = model, tokenizer = tokenizer, dataset = texts, batch_size = batch_size,
    layer_index = i, e = e
  )
  all_layers[[i]] <- layer_importance
}

all_layers_df <- do.call(rbind, lapply(all_layers, as.data.frame))
all_layers_df
