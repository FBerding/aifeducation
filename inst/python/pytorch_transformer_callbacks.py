import transformers
import csv

def create_AIFETransformerCSVLogger(log_file,value_top,total_top,message_top,min_step):
  class AIFETransformerCSVLogger(transformers.TrainerCallback):
    def on_train_begin(self, args, state, control, **kwargs):
      try:
        f = open(log_file,"w",newline="")
        fieldnames = ["value", "total","message"]
        writer = csv.DictWriter(f, fieldnames=fieldnames,dialect='unix')
        writer.writeheader()
        writer.writerow({'value': value_top, 'total': total_top, 'message': message_top})
        writer.writerow({'value': 0, 'total': args.num_train_epochs, 'message': "Epochs"})
        writer.writerow({'value': 0, 'total': str(int(state.max_steps/args.num_train_epochs)), 'message': "Steps"})
        f.close()
      except:
        a=None
        
    def on_epoch_end(self, args, state, control, **kwargs):
      #Hier bitte den log der loss function ergänzen bzw. im Event on log
      try:
        f = open(log_file,"w",newline="")
        fieldnames = ["value", "total","message"]
        writer = csv.DictWriter(f, fieldnames=fieldnames,dialect='unix')
        writer.writeheader()
        writer.writerow({'value': value_top, 'total': total_top, 'message': message_top})
        writer.writerow({'value': state.epoch, 'total': args.num_train_epochs, 'message': "Epochs"})
        writer.writerow({'value': (state.global_step % (state.max_steps/args.num_train_epochs)), 'total': str(int(state.max_steps/args.num_train_epochs)), 'message': "Steps"})
        f.close()
      except:
        a=None
        
    def on_step_end(self, args, state, control, **kwargs):
      if (state.global_step % min_step)==0:
        try:
          f = open(log_file,"w",newline="")
          fieldnames = ["value", "total","message"]
          writer = csv.DictWriter(f, fieldnames=fieldnames,dialect='unix')
          writer.writeheader()
          writer.writerow({'value': value_top, 'total': total_top, 'message': message_top})
          writer.writerow({'value': state.epoch, 'total': args.num_train_epochs, 'message': "Epochs"})
          writer.writerow({'value': (state.global_step % (state.max_steps/args.num_train_epochs)), 'total': str(int(state.max_steps/args.num_train_epochs)), 'message': "Steps"})
          f.close()
        except:
          a=None
  
  return AIFETransformerCSVLogger()
    

class ReportAiforeducationShiny_PT(transformers.TrainerCallback):
    def on_train_begin(self, args, state, control, **kwargs):
        r.py_update_aifeducation_progress_bar_steps(
            value = 0,
            total = state.max_steps,
            title = ("Batch/Step: " + str(0) + "/" + str(int(state.max_steps/args.num_train_epochs))))
        r.py_update_aifeducation_progress_bar_epochs(
            value = 0,
            total = args.num_train_epochs,
            title = ("Epoch: " + str(0) + "/" + str(args.num_train_epochs)))
  
    def on_epoch_end(self, args, state, control, **kwargs):
        r.py_update_aifeducation_progress_bar_epochs(
            value = state.epoch,
            total = args.num_train_epochs,
            title = ("Epoch: " + str(int(state.epoch)) + "/" + str(args.num_train_epochs)))
    
    def on_step_end(self, args, state, control, **kwargs):
        r.py_update_aifeducation_progress_bar_steps(
            value = (state.global_step % (state.max_steps/args.num_train_epochs)),
            total = state.max_steps/args.num_train_epochs,
            title = ("Batch/Step: " + str((state.global_step % (state.max_steps/args.num_train_epochs))) + "/" + str(int(state.max_steps/args.num_train_epochs))))
