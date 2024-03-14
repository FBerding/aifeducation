import transformers

class ReportAiforeducationShiny_PT(transformers.TrainerCallback):
  def on_train_begin(self, args, state, control, **kwargs):
    r.py_update_aifeducation_progress_bar_steps(value=0,total=state.max_steps,title=("Batch/Step: "+str(0)+"/"+str(int(state.max_steps/args.num_train_epochs))))
    r.py_update_aifeducation_progress_bar_epochs(value=0,total=args.num_train_epochs,title=("Epoch: "+str(0)+"/"+str(args.num_train_epochs)))

  def on_epoch_end(self, args, state, control, **kwargs):
    r.py_update_aifeducation_progress_bar_epochs(value=state.epoch,total=args.num_train_epochs,title=("Epoch: "+str(int(state.epoch))+"/"+str(args.num_train_epochs)))
  
  def on_step_end(self, args, state, control, **kwargs):
    r.py_update_aifeducation_progress_bar_steps(value=(state.global_step % (state.max_steps/args.num_train_epochs)),total=state.max_steps/args.num_train_epochs,title=("Batch/Step: "+str((state.global_step % (state.max_steps/args.num_train_epochs)))+"/"+str(int(state.max_steps/args.num_train_epochs))))
    
    

