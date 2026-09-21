import torch

def save_config(args):
  arguments=args
  arguments.pop("self")
  arguments.pop("__class__", None)
  arguments.pop("device")
  arguments.pop("dtype")
  return arguments

def write_config_to_json(self,filepath):
  tmp_config=self.config
  tmp_config["class_name"]=self.__class__.__name__
  try:
    with open(filepath, "w", encoding="utf-8") as file:
        # indent=4 formatiert das JSON lesbar mit Einrückungen
        json.dump(self.config, file, ensure_ascii=False, indent=4)
  except IOError as e:
    print(f"Error during saving config: {e}")

def get_SeqLen_from_mask(mask):
  seq_len = torch.sum(~mask,dim=1,keepdim=False)
  return seq_len.detach()

def get_FeatureMask_from_mask(mask,num_features):
  mask = torch.unsqueeze(mask,dim=2).expand((mask.size(0),mask.size(1),num_features))
  return mask.detach()
