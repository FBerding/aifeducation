from safetensors import safe_open
from safetensors.torch import save_file

def remove_unused_weights(input_path, output_path, skip_prefixes):
    weights = {}
    metadata = {}

    with safe_open(input_path, framework="pt") as f:
        metadata = f.metadata() or {}
        for key in f.keys():
            if not any(key.startswith(prefix) for prefix in skip_prefixes):
                weights[key] = f.get_tensor(key)

    save_file(weights, output_path, metadata=metadata)
