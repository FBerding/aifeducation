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
