import torch
import numpy as np

class MetaLernerBatchSampler(torch.utils.data.sampler.Sampler):
    #Ns Number of examples per class in sample set (k-shot)
    #Nq number of examples per class in the query set (k-shot)
    def __init__(self, targets, Ns,Nq):
        # build data for sampling here
        self.targets = targets
        self.Ns=Ns
        self.Nq=Nq
        
        self.classes=torch.unique(self.targets).numpy()
        self.n_classes=len(self.classes)
        self.batch_size=self.n_classes*(self.Ns+self.Nq)
        
        self.indices_per_class={}
        self.cases_per_class={}
        for c in self.classes:
          self.indices_per_class[c]=torch.where(self.targets==c)[0]
          self.cases_per_class[c]=len(self.indices_per_class[c])
        self.number_batches=self.cases_per_class[max(self.cases_per_class,key=self.cases_per_class.get)]//(self.Ns+self.Nq)
        
    def __iter__(self):
        # implement logic of sampling here
        permutations={}
        index_shift=0
        for c in self.classes:
            permutations[c]=torch.randperm(self.cases_per_class[c])
        for batch_number in range(self.number_batches):
          index_shift=batch_number*(self.Ns+self.Nq)
          batch_sample=[]
          batch_query=[]
          for c in self.classes:
            class_permutations=permutations[c]
            ids_sample=np.array(range((0+index_shift),(self.Ns+index_shift)))%self.cases_per_class[c]
            ids_query=np.array(range((self.Ns+index_shift),((self.Ns+self.Nq)+index_shift)))%self.cases_per_class[c]
            
            perm_sample_idx=class_permutations[ids_sample]
            perm_query_idx=class_permutations[ids_query]
            
            perm_sample=self.indices_per_class[c][perm_sample_idx].numpy()
            perm_query=self.indices_per_class[c][perm_query_idx].numpy()
            
            batch_sample.extend(perm_sample)
            batch_query.extend(perm_query)
          batch=[]
          batch=batch_sample+batch_query
          yield batch

    def __len__(self):
        return self.number_batches

class ClassMean_PT(torch.nn.Module):
  def __init__(self,n_classes):
    super().__init__()
    self.n_classes=n_classes
  
  def forward(self,x,classes):
    index_matrix=torch.nn.functional.one_hot(torch.Tensor.to(classes,dtype=torch.int64),num_classes=self.n_classes)
    index_matrix=torch.transpose(index_matrix,dim0=0,dim1=1)
    index_matrix=torch.Tensor.to(index_matrix,dtype=x.dtype)
    cases_per_class=torch.sum(index_matrix,dim=1)
    class_mean=torch.matmul(torch.diag(1/cases_per_class),torch.matmul(index_matrix,x))
    return class_mean
  
class ProtoNetMetric_PT(torch.nn.Module):
  def __init__(self):
    super().__init__()
    self.alpha=torch.nn.Parameter(torch.randn(1))
  
  def forward(self,x,prototypes):
    distance_matrix=torch.zeros(x.size()[0],prototypes.size(0))
    for i in range(prototypes.size(0)):
      distance=torch.square(self.alpha+1e-16)*torch.square(torch.nn.functional.pairwise_distance(x,prototypes[i],p=2.0,keepdim=False,eps=0))
      distance_matrix[:,i]=distance
    return distance_matrix

class ProtoNetLossWithMargin_PT(torch.nn.Module):
  def __init__(self,alpha=0.2,margin=0.5):
    super().__init__()
    self.alpha=alpha
    self.margin=margin
  
  def forward(self,classes_q,distance_matrix):
    K=distance_matrix.size()[1]

    index_matrix=torch.nn.functional.one_hot(torch.Tensor.to(classes_q,dtype=torch.int64),num_classes=K)
    index_matrix=torch.Tensor.to(index_matrix,dtype=distance_matrix.dtype)
    
    distance_to_min=self.alpha*(torch.sum(torch.diag(torch.matmul(index_matrix.float(),torch.square(torch.transpose(distance_matrix,0,1))))))
    
    distance_margin=(self.margin-torch.transpose(distance_matrix,0,1))
    distance_margin=torch.where(distance_margin<0,torch.zeros(size=distance_margin.size()),distance_margin)
    
    distance_to_max=(1-self.alpha)*(torch.sum(torch.diag(torch.matmul(1-index_matrix.float(),torch.square(distance_margin)))))
    loss=(1/K)*(distance_to_min+distance_to_max)
    #print(distance_to_min)
    #print(distance_to_max)
    return loss

class TextEmbeddingClassifierProtoNet_PT(torch.nn.Module):
  def __init__(self,features, times, hidden, rec,rec_type,rec_bidirectional, intermediate_size,
  attention_type, repeat_encoder, dense_dropout,rec_dropout, encoder_dropout,
  add_pos_embedding, self_attention_heads, target_levels,embedding_dim):
    
    super().__init__()
    
    self.embedding_dim=embedding_dim
    self.classes=torch.from_numpy(target_levels)
    self.n_classes=len(target_levels)
    
    self.trained_prototypes=torch.nn.Parameter(torch.randn(self.n_classes,self.embedding_dim))

    if isinstance(rec, int):
      rec=[rec]
      
    if isinstance(hidden, int):
      hidden=[hidden]
    
    if rec is None:
      n_rec=0
    else:
      n_rec=len(rec)
    
    if hidden is None:
      n_hidden=0
    else:
      n_hidden=len(hidden)
    
    if n_hidden==0 and n_rec==0 and repeat_encoder==0:
      last_in_features=features*times
    elif  n_hidden>0:
      last_in_features=hidden[n_hidden-1]
    elif n_rec>0:
      if rec_bidirectional==True:
        last_in_features=2*rec[len(rec)-1]
      else:
        last_in_features=rec[len(rec)-1]
    else:
      last_in_features=features

    self.embedding_head=torch.nn.Linear(
      in_features=last_in_features,
      out_features=self.embedding_dim)
    
    self.class_mean=ClassMean_PT(n_classes=self.n_classes)
    self.metric=ProtoNetMetric_PT()
    
    self.core_net=TextEmbeddingClassifier_PT(
      features=features, 
      times=times,
      hidden=hidden, 
      rec=rec, 
      rec_type=rec_type,
      rec_bidirectional=rec_bidirectional,
      intermediate_size=intermediate_size,
      attention_type=attention_type, 
      repeat_encoder=repeat_encoder, 
      dense_dropout=dense_dropout,
      rec_dropout=rec_dropout,
      encoder_dropout=encoder_dropout, 
      add_pos_embedding=add_pos_embedding,
      self_attention_heads=self_attention_heads, 
      target_levels=target_levels,
      classification_head=False)
  
  def forward(self, input_q,classes_q=None,input_s=None,classes_s=None,predication_mode=True):
    if input_s is None or classes_s is None:
      prototypes=self.trained_prototypes
    else:
      #Sample set
      sample_embeddings=self.core_net(input_s)
      #print(sample_embeddings)
      sample_embeddings=self.embedding_head(sample_embeddings)
      #print(sample_embeddings)
      prototypes=self.class_mean(x=sample_embeddings,classes=classes_s)
      #print(prototypes)

    #Query set
    query_embeddings=self.core_net(input_q)
    query_embeddings=self.embedding_head(query_embeddings)

    #Calc distance from query embeddings to global global prototypes
    distances=self.metric(x=query_embeddings,prototypes=prototypes)
    probabilities=torch.nn.Softmax(dim=1)(torch.exp(-distances))
      
    if predication_mode==False:
      return probabilities, distances
    else:
      return probabilities
  def embed(self,inputs):
    #Sample set
    sample_embeddings=self.core_net(inputs)
    #print(sample_embeddings)
    sample_embeddings=self.embedding_head(sample_embeddings)
    return sample_embeddings
  def set_trained_prototypes(self,prototypes):
    self.trained_prototypes=torch.nn.Parameter(prototypes)
  def get_trained_prototypes(self):
    return self.trained_prototypes
  

#-------------------------------------------------------------------------------    
def TeClassifierProtoNetTrain_PT_with_Datasets(model,loss_fct_name, optimizer_method, epochs, trace,Ns,Nq,
loss_alpha, loss_margin, train_data,val_data,filepath,use_callback,n_classes,test_data=None,
shiny_app_active=False):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    model.to(device,dtype=float)
  else:
    model.to(device,dtype=torch.double)
  
  if optimizer_method=="adam":
    optimizer=torch.optim.Adam(model.parameters())
  elif optimizer_method=="rmsprop":
    optimizer=torch.optim.RMSprop(model.parameters())
    
  #if loss_fct_name=="ProtoNetworkMargin":
  loss_fct=ProtoNetLossWithMargin_PT(
    alpha=loss_alpha,
    margin=loss_margin)
    
  #Set furhter necessary functions
  get_class_mean=ClassMean_PT(n_classes=n_classes)
    
  #Tensor for Saving Training History
  if not (test_data is None):
    history_loss=torch.zeros(size=(3,epochs),requires_grad=False)
    history_acc=torch.zeros(size=(3,epochs),requires_grad=False)
    history_bacc=torch.zeros(size=(3,epochs),requires_grad=False)
  else:
    history_loss=torch.zeros(size=(2,epochs),requires_grad=False)
    history_acc=torch.zeros(size=(2,epochs),requires_grad=False)
    history_bacc=torch.zeros(size=(2,epochs),requires_grad=False)
  
  best_bacc=float('-inf')
  best_val_loss=float('inf')
  
  #GUI ProgressBar
  if shiny_app_active is True:
    current_step=0
    total_epochs=epochs
    total_steps=len(trainloader)
    r.py_update_aifeducation_progress_bar_steps(value=0,total=total_steps,title=("Batch/Step: "+str(0)+"/"+str(total_steps)))
    r.py_update_aifeducation_progress_bar_epochs(value=0,total=epochs,title=("Epoch: "+str(0)+"/"+str(epochs)))

  #Set Up Loaders
  valloader=torch.utils.data.DataLoader(
    val_data,
    batch_size=Ns+Nq,
    shuffle=False)
    
  if not (test_data is None):
    testloader=torch.utils.data.DataLoader(
      test_data,
      batch_size=Ns+Nq,
      shuffle=False)

  for epoch in range(epochs):
    #Set Up Sampler for a new random selection---------------------------------
    ProtoNetSampler_Train=MetaLernerBatchSampler(
      targets=train_data["labels"],
      Ns=Ns,
      Nq=Nq)  
    
    trainloader=torch.utils.data.DataLoader(
      train_data,
      batch_sampler=ProtoNetSampler_Train)
    
    #Training------------------------------------------------------------------
    train_loss=0.0
    n_matches_train=0
    n_total_train=0
    confusion_matrix_train=torch.zeros(size=(n_classes,n_classes))
    confusion_matrix_train=confusion_matrix_train.to(device,dtype=torch.double)
    
    #Gui ProgressBar
    if shiny_app_active is True:
      current_step=0
    
    model.train(True)
    for batch in trainloader:
      #assign colums of the batch
      inputs=batch["input"]
      labels=batch["labels"]

      sample_inputs=inputs[0:(n_classes*Ns)].clone()
      query_inputs=inputs[(n_classes*Ns):(n_classes*(Ns+Nq))].clone()
      
      sample_classes=labels[0:(n_classes*Ns)].clone()
      query_classes=labels[(n_classes*Ns):(n_classes*(Ns+Nq))].clone()

      sample_inputs = sample_inputs.to(device)
      query_inputs = query_inputs.to(device)
  
      sample_classes = sample_classes.to(device)
      query_classes = query_classes.to(device)
      
      optimizer.zero_grad()
      outputs=model(input_q=query_inputs,
      classes_q=query_classes,
      input_s=sample_inputs,
      classes_s=sample_classes,
      predication_mode=False)
      
      loss=loss_fct(classes_q=query_classes,distance_matrix=outputs[1])
      loss.backward()
      optimizer.step()
      
      train_loss +=loss.item()
      model.eval()
      
      #Calc Accuracy
      pred_idx=outputs[0].max(dim=1).indices.to(dtype=torch.long)
      label_idx=query_classes.to(dtype=torch.long)
      
      match=(pred_idx==label_idx)
      n_matches_train+=match.sum().item()
      n_total_train+=outputs[0].size(0)

      #Calc Balanced Accuracy
      confusion_matrix_train+=multiclass_confusion_matrix(input=pred_idx,target=label_idx,num_classes=n_classes)
      
      #Update Progress Logger
      if shiny_app_active is True:
        current_step+=1
        r.py_update_aifeducation_progress_bar_steps(value=current_step,total=total_steps,title=("Batch/Step: "+str(current_step)+"/"+str(total_steps)))
    
    acc_train=n_matches_train/n_total_train
    bacc_train=torch.sum(torch.diagonal(confusion_matrix_train)/torch.sum(confusion_matrix_train,dim=1))/n_classes
    
    #Calculate trained prototypes----------------------------------------------
    model.eval()
    
    running_class_mean=None
    running_class_freq=None
    
    for batch in trainloader:
      #assign colums of the batch
      inputs=batch["input"]
      labels=batch["labels"]
      
      inputs = inputs.to(device)
      labels=labels.to(device)
      
      embeddings=model.embed(inputs)
      new_class_means=get_class_mean(x=embeddings,classes=labels)
      new_class_freq=torch.bincount(input=labels.int(),minlength=n_classes)

      if running_class_mean is None:
        running_class_mean=new_class_means
        running_class_freq=new_class_freq
      else:
        w_old=(running_class_freq/(running_class_freq+new_class_freq))
        w_new=(new_class_freq/(running_class_freq+new_class_freq))
        
        weighted_mean_old=torch.matmul(torch.diag(w_old).to(device,dtype=float),running_class_mean.to(device,dtype=float))
        weighted_mean_new=torch.matmul(torch.diag(w_new).to(device,dtype=float),new_class_means.to(device,dtype=float))
        
        running_class_mean=weighted_mean_old+weighted_mean_new
        running_class_freq=running_class_freq+new_class_freq

    
    model.set_trained_prototypes(running_class_mean)
    
    #Validation----------------------------------------------------------------
    val_loss=0.0
    n_matches_val=0
    n_total_val=0
    
    confusion_matrix_val=torch.zeros(size=(n_classes,n_classes))
    confusion_matrix_val=confusion_matrix_val.to(device,dtype=torch.double)

    model.eval()
    with torch.no_grad():
      for batch in valloader:
        inputs=batch["input"]
        labels=batch["labels"]
        
        inputs = inputs.to(device)
        labels=labels.to(device)
        outputs=model(inputs,predication_mode=False)

        loss=loss_fct(classes_q=labels,distance_matrix=outputs[1])
        val_loss +=loss.item()
        
        #Calc Accuracy
        pred_idx=outputs[0].max(dim=1).indices.to(dtype=torch.long)
        label_idx=labels.to(dtype=torch.long)
        
        match=(pred_idx==label_idx)
        n_matches_val+=match.sum().item()
        n_total_val+=outputs[0].size(0)
        
        #Calc Balanced Accuracy
        confusion_matrix_val+=multiclass_confusion_matrix(input=pred_idx,target=label_idx,num_classes=n_classes)

    acc_val=n_matches_val/n_total_val
    bacc_val=torch.sum(torch.diagonal(confusion_matrix_val)/torch.sum(confusion_matrix_val,dim=1))/n_classes
   
    #Test----------------------------------------------------------------------
    if not (test_data is None):
      test_loss=0.0
      n_matches_test=0
      n_total_test=0
      
      confusion_matrix_test=torch.zeros(size=(n_classes,n_classes))
      confusion_matrix_test=confusion_matrix_test.to(device,dtype=torch.double)
  
      model.eval()
      with torch.no_grad():
        for batch in testloader:
          inputs=batch["input"]
          labels=batch["labels"]
          
          inputs = inputs.to(device)
          labels=labels.to(device)
        
          outputs=model(inputs,predication_mode=False)
 
          loss=loss_fct(classes_q=labels,distance_matrix=outputs[1])
          test_loss +=loss.item()
        
          #Calc Accuracy
          pred_idx=outputs[0].max(dim=1).indices.to(dtype=torch.long)
          label_idx=labels.to(dtype=torch.long)
            
          match=(pred_idx==label_idx)
          n_matches_test+=match.sum().item()
          n_total_test+=outputs[0].size(0)
          
          #Calc Balanced Accuracy
          confusion_matrix_test+=multiclass_confusion_matrix(input=pred_idx,target=label_idx,num_classes=n_classes)
  
      acc_test=n_matches_test/n_total_test
      bacc_test=torch.sum(torch.diagonal(confusion_matrix_test)/torch.sum(confusion_matrix_test,dim=1))/n_classes
    
    
    #Record History
    if not (test_data is None):
      history_loss[0,epoch]=train_loss/len(trainloader)
      history_loss[1,epoch]=val_loss/len(valloader)
      history_loss[2,epoch]=test_loss/len(testloader)
      
      history_acc[0,epoch]=acc_train
      history_acc[1,epoch]=acc_val
      history_acc[2,epoch]=acc_test
      
      history_bacc[0,epoch]=bacc_train
      history_bacc[1,epoch]=bacc_val
      history_bacc[2,epoch]=bacc_test
    else:
      history_loss[0,epoch]=train_loss/len(trainloader)
      history_loss[1,epoch]=val_loss/len(valloader)
      
      history_acc[0,epoch]=acc_train
      history_acc[1,epoch]=acc_val
      
      history_bacc[0,epoch]=bacc_train
      history_bacc[1,epoch]=bacc_val

    #Trace---------------------------------------------------------------------
    if trace>=1:
      if test_data is None:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss,
          acc_train,
          bacc_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val))
      else:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f} | Test Loss: {:.4f} ACC: {:.4f} BACC: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss/len(trainloader),
          acc_train,
          bacc_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val,
          test_loss/len(testloader),
          acc_test,
          bacc_test))
          
    #Update ProgressBar in Shiny
    if shiny_app_active is True:
        r.py_update_aifeducation_progress_bar_epochs(value=epoch+1,total=epochs,title=("Epoch: "+str(epoch+1)+"/"+str(epochs)))
        
    
    #Callback-------------------------------------------------------------------
    if use_callback==True:
      if bacc_val>best_bacc:
        if trace>=1:
          print("Val Balanced Accuracy increased from {:.4f} to {:.4f}".format(best_bacc,bacc_val))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_val_loss=val_loss/len(valloader)
      if bacc_val==best_bacc and val_loss/len(valloader)<best_val_loss:
        if trace>=1:
          print("Val Loss decreased from {:.4f} to {:.4f}".format(best_val_loss,val_loss/len(valloader)))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_val_loss=val_loss/len(valloader)
  
  #Finalize--------------------------------------------------------------------
  if use_callback==True:
    if trace>=1:
      print("Load Best Weights from {}".format(filepath))
    model.load_state_dict(torch.load(filepath))
    #safetensors.torch.load_model(model=model,filename=filepath)


  history={
    "loss":history_loss.numpy(),
    "accuracy":history_acc.numpy(),
    "balanced_accuracy":history_bacc.numpy()} 
 
    
  return history

def TeProtoNetBatchEmbed(model,dataset_q,batch_size):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    model.to(device,dtype=float)
  else:
    model.to(device,dtype=torch.double)
    
  model.eval()
  predictionloader=torch.utils.data.DataLoader(
    dataset_q,
    batch_size=batch_size,
    shuffle=False)

  with torch.no_grad():
    iteration=0
    for batch in predictionloader:
      inputs=batch["input"]
      inputs = inputs.to(device)
      
      predictions=model.embed(inputs)
      
      if iteration==0:
        predictions_list=predictions.to("cpu")
      else:
        predictions_list=torch.concatenate((predictions_list,predictions.to("cpu")), axis=0, out=None)
      iteration+=1
  
  return predictions_list      
