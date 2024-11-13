# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

import torch
import numpy as np

class MetaLernerBatchSampler(torch.utils.data.sampler.Sampler):
    #Ns Number of examples per class in sample set (k-shot)
    #Nq number of examples per class in the query set (k-shot)
    #targets pytorch tensor containing the classes/categories
    def __init__(self, targets, Ns,Nq,separate,shuffle):
        # build data for sampling here
        self.Ns=Ns
        self.Nq=Nq
        self.separate=separate
        self.shuffle=shuffle

        #Get the available classes in targets
        self.classes=torch.unique(targets).numpy()
        #Get the number of classes
        self.n_classes=len(self.classes)
        #Calculate the batch size depending on Ns and Nq
        self.batch_size=self.n_classes*(self.Ns+self.Nq)
        
        #Create dictonary that contains the indexes sorted for every class
        self.indices_per_class={}
        #Create dictornary thats sotres the number of cases per class
        self.cases_per_class={}
        #Gather indicies per class and cases per class
        for c in self.classes:
          self.indices_per_class[c]=torch.where(targets==c)[0]
          self.cases_per_class[c]=len(self.indices_per_class[c])
        
         #Create dictonary that contains the indexes sorted for every class and query/sample
        self.query_indices_per_class={}
        self.sample_indices_per_class={}
        #Create dictornary thats sotres the number of cases per class and query/sample
        self.query_cases_per_class={}
        self.sample_cases_per_class={}

        #Split indices per class into a sample and query sample if separate is True
        if self.separate is True:
          for c in self.classes:
            #Calculate number of cases for sample
            n_sample=int(round(self.Ns/(self.Ns+self.Nq)*self.cases_per_class[c]))
            n_sample=max(1,n_sample)
            n_sample=min(n_sample,(self.cases_per_class[c]-1))
            #Calculate number of cases for query
            n_query=self.cases_per_class[c]-n_sample
            #Create permutation in order to create a random sample
            permutation=self.indices_per_class[c][torch.randperm(self.cases_per_class[c])]
            #Assign indices
            self.sample_indices_per_class[c]=permutation[np.array(range(0,n_sample))]
            self.query_indices_per_class[c]=permutation[np.array(range(n_sample,self.cases_per_class[c]))]
            #Calculate number of cases
            self.sample_cases_per_class[c]=len(self.sample_indices_per_class[c])
            self.query_cases_per_class[c]=len(self.query_indices_per_class[c])
        else:
          #Create a random permutation if separate is False and shuffle is False
          #If shuffle is True random sampling is applied during iter
          if self.shuffle is False:
            for c in self.classes:
              self.indices_per_class[c]=self.indices_per_class[c][torch.randperm(self.cases_per_class[c])]
          
        #Calculate number of batches
        self.number_batches=self.cases_per_class[max(self.cases_per_class,key=self.cases_per_class.get)]//(self.Ns+self.Nq)
        
    def __iter__(self):
      for current_iter in range(self.number_batches):
        #Create list for saving the results per class temporarily 
        batch_sample=[]
        batch_query=[]
      
        if self.separate is False:
          if self.shuffle is True:
            for c in self.classes:
              #Calculate permutations for the random sample for each class
              permutations=self.indices_per_class[c][torch.randperm(self.cases_per_class[c])]
              
              #Calculat the indexes for selecting the first Ns+Nq indices
              ids_sample=np.array(range(0,self.Ns))
              ids_query=np.array(range(self.Ns,(self.Ns+self.Nq)))

              #Extract the final indices
              perm_sample=permutations[ids_sample].numpy()
              perm_query=permutations[ids_query].numpy()
              
              #Add them to batch
              batch_sample.extend(perm_sample)
              batch_query.extend(perm_query)
          else:
            for c in self.classes:
              #Calculate indices. If the end of all cases is reached for this class
              #start at beginng and fill the list
              index_shift= (1+current_iter)*(self.Ns+self.Nq)
              ids_sample=np.array(range((0+index_shift),(self.Ns+index_shift)))%self.cases_per_class[c]
              ids_query=np.array(range((self.Ns+index_shift),((self.Ns+self.Nq)+index_shift)))%self.cases_per_class[c]
              
              #Extract the final indices
              perm_sample=self.indices_per_class[c][ids_sample].numpy()
              perm_query=self.indices_per_class[c][ids_query].numpy()
            
              #Add them to batch
              batch_sample.extend(perm_sample)
              batch_query.extend(perm_query)
        if self.separate is True:
          if self.shuffle is True:
            for c in self.classes:
              #Calculate permutations for the random sample for each class
              permutations_sample=self.sample_indices_per_class[c][torch.randperm(self.sample_cases_per_class[c])]
              permutations_query=self.query_indices_per_class[c][torch.randperm(self.query_cases_per_class[c])]
              
              #Calculat the indexes for selecting the first Ns+Nq indices
              ids_sample=np.array(range(0,self.Ns))
              ids_query=np.array(range(0,self.Nq))
              
              #Extract the final indices
              perm_sample=permutations_sample[ids_sample].numpy()
              perm_query=permutations_query[ids_query].numpy()
                
              #Add them to batch
              batch_sample.extend(perm_sample)
              batch_query.extend(perm_query)
          else:
            #Calculate indices. If the end of all cases is reached for this class
            #start at beginng and fill the list
            index_shift_sample= (1+current_iter)*self.Ns
            index_shift_query= (1+current_iter)*self.Nq
            for c in self.classes:
              ids_sample=np.array(range((0+index_shift_sample),(self.Ns+index_shift_sample)))%self.sample_cases_per_class[c]
              ids_query=np.array(range((0+index_shift_query),(self.Nq+index_shift_query)))%self.query_cases_per_class[c]
              
              #Extract the final indices
              perm_sample=self.sample_indices_per_class[c][ids_sample].numpy()
              perm_query=self.query_indices_per_class[c][ids_query].numpy()
              
              #Add them to batch
              batch_sample.extend(perm_sample)
              batch_query.extend(perm_query)
        #Create the final batch
        #Add first the sample of all classes and then the query of all classes
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
    self.alpha=torch.nn.Parameter(torch.ones(1))
  
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
  def __init__(self,features, times, dense_size,dense_layers,rec_size,rec_layers,rec_type,rec_bidirectional, intermediate_size,
  attention_type, repeat_encoder, dense_dropout,rec_dropout, encoder_dropout,
  add_pos_embedding, self_attention_heads, target_levels,embedding_dim):
    
    super().__init__()
    
    self.embedding_dim=embedding_dim
    self.classes=torch.from_numpy(np.copy(target_levels))
    self.n_classes=len(target_levels)
    
    self.trained_prototypes=torch.nn.Parameter(torch.rand(self.n_classes,self.embedding_dim))
    #self.near_factor=torch.nn.Parameter(torch.ones(1))

    if  dense_layers>0:
      last_in_features=dense_size
    elif rec_layers>0:
      if rec_bidirectional==True:
        last_in_features=2*rec_size
      else:
        last_in_features=rec_size
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
      dense_layers=dense_layers, 
      dense_size=dense_size,
      rec_layers=rec_layers, 
      rec_size=rec_size,
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
      sample_embeddings=self.embed(input_s)
      prototypes=self.class_mean(x=sample_embeddings,classes=classes_s)

    #Query set
    query_embeddings=self.embed(input_q)

    #Calc distance from query embeddings to global global prototypes
    distances=self.metric(x=query_embeddings,prototypes=prototypes)
    probabilities=torch.nn.Softmax(dim=1)(torch.exp(-distances))
      
    if predication_mode==False:
      return probabilities, distances
    else:
      return probabilities
  
  def get_distances(self,inputs):
    distances=self.metric(x=self.embed(inputs),prototypes=self.trained_prototypes)
    return distances
  
  def embed(self,inputs):
    embeddings=self.core_net(inputs)
    embeddings=torch.tanh(self.embedding_head(embeddings))
    return embeddings
  
  def set_trained_prototypes(self,prototypes):
    self.trained_prototypes=torch.nn.Parameter(prototypes)
  def get_trained_prototypes(self):
    return self.trained_prototypes
  

#-------------------------------------------------------------------------------    
def TeClassifierProtoNetTrain_PT_with_Datasets(model,loss_fct_name, optimizer_method, epochs, trace,Ns,Nq,
loss_alpha, loss_margin, train_data,val_data,filepath,use_callback,n_classes,sampling_separate,sampling_shuffle,test_data=None,
log_dir=None, log_write_interval=10, log_top_value=0, log_top_total=1, log_top_message="NA"):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    dtype=float
    model.to(device,dtype=dtype)
  else:
    dtype=torch.double
    model.to(device,dtype=dtype)
  
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
    history_loss=torch.ones(size=(3,epochs),requires_grad=False)*-100
    history_acc=torch.ones(size=(3,epochs),requires_grad=False)*-100
    history_bacc=torch.ones(size=(3,epochs),requires_grad=False)*-100
    history_avg_iota=torch.ones(size=(3,epochs),requires_grad=False)*-100
  else:
    history_loss=torch.ones(size=(2,epochs),requires_grad=False)*-100
    history_acc=torch.ones(size=(2,epochs),requires_grad=False)*-100
    history_bacc=torch.ones(size=(2,epochs),requires_grad=False)*-100
    history_avg_iota=torch.ones(size=(2,epochs),requires_grad=False)*-100
  
  best_bacc=float('-inf')
  best_acc=float('-inf')
  best_val_loss=float('inf')
  best_val_avg_iota=float('-inf')
  
  #Set Up Loaders
  ProtoNetSampler_Train=MetaLernerBatchSampler(
  targets=train_data["labels"],
  Ns=Ns,
  Nq=Nq,
  separate=sampling_separate,
  shuffle=sampling_shuffle)
  
  trainloader=torch.utils.data.DataLoader(
    train_data,
    batch_sampler=ProtoNetSampler_Train)
  
  valloader=torch.utils.data.DataLoader(
    val_data,
    batch_size=Ns+Nq,
    shuffle=False)
    
  if not (test_data is None):
    testloader=torch.utils.data.DataLoader(
      test_data,
      batch_size=Ns+Nq,
      shuffle=False)
  
  #Log file
  if not (log_dir is None):
    log_file=log_dir+"/aifeducation_state.log"
    log_file_loss=log_dir+"/aifeducation_loss.log"
    last_log=None
    last_log_loss=None
    current_step=0
    total_epochs=epochs
    
    total_steps=len(trainloader)+len(valloader)
    if not (test_data is None):
      total_steps=total_steps+len(testloader)


  for epoch in range(epochs):
    #logging
    current_step=0

    #Training------------------------------------------------------------------
    train_loss=0.0
    n_matches_train=0
    n_total_train=0
    confusion_matrix_train=torch.zeros(size=(n_classes,n_classes))
    confusion_matrix_train=confusion_matrix_train.to(device,dtype=torch.double)
    
    n_batches=0
    
    model.train(True)
    for batch in trainloader:
      n_batches=n_batches+1
      #assign colums of the batch
      inputs=batch["input"]
      labels=batch["labels"]

      sample_inputs=inputs[0:(n_classes*Ns)].clone()
      query_inputs=inputs[(n_classes*Ns):(n_classes*(Ns+Nq))].clone()
      
      sample_classes=labels[0:(n_classes*Ns)].clone()
      query_classes=labels[(n_classes*Ns):(n_classes*(Ns+Nq))].clone()

      sample_inputs = sample_inputs.to(device,dtype=dtype)
      query_inputs = query_inputs.to(device,dtype=dtype)
  
      sample_classes = sample_classes.to(device,dtype=dtype)
      query_classes = query_classes.to(device,dtype=dtype)
      
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
      
      #Update log file
      if not (log_dir is None):
        current_step+=1
        last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                  total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                  message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
        last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)
    
    acc_train=n_matches_train/n_total_train
    bacc_train=torch.sum(torch.diagonal(confusion_matrix_train)/torch.sum(confusion_matrix_train,dim=1))/n_classes
    avg_iota_train=torch.diagonal(confusion_matrix_train)/(torch.sum(confusion_matrix_train,dim=0)+torch.sum(confusion_matrix_train,dim=1)-torch.diagonal(confusion_matrix_train))
    avg_iota_train=torch.sum(avg_iota_train)/n_classes
    
    #Calculate trained prototypes----------------------------------------------
    model.eval()
    
    running_class_mean=None
    running_class_freq=None
    
    for batch in trainloader:
      #assign colums of the batch
      inputs=batch["input"]
      labels=batch["labels"]
      
      #inputs=inputs[0:(n_classes*Ns)].clone()
      #labels=labels[0:(n_classes*Ns)].clone()
      
      inputs = inputs.to(device,dtype=dtype)
      labels=labels.to(device,dtype=dtype)
      
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
        
        inputs = inputs.to(device,dtype=dtype)
        labels=labels.to(device,dtype=dtype)
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
        
        #Update log file
        if not (log_dir is None):
          current_step+=1
          last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                    total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                    message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
          last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)

    acc_val=n_matches_val/n_total_val
    bacc_val=torch.sum(torch.diagonal(confusion_matrix_val)/torch.sum(confusion_matrix_val,dim=1))/n_classes
    avg_iota_val=torch.diagonal(confusion_matrix_val)/(torch.sum(confusion_matrix_val,dim=0)+torch.sum(confusion_matrix_val,dim=1)-torch.diagonal(confusion_matrix_val))
    avg_iota_val=torch.sum(avg_iota_val)/n_classes
    
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
          
          inputs = inputs.to(device,dtype=dtype)
          labels=labels.to(device,dtype=dtype)
        
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
          
          #Update log file
          if not (log_dir is None):
            current_step+=1
            last_log=write_log_py(log_file=log_file, value_top = log_top_value, value_middle = epoch+1, value_bottom = current_step,
                      total_top = log_top_total, total_middle = epochs, total_bottom = total_steps, message_top = log_top_message, message_middle = "Epochs",
                      message_bottom = "Steps", last_log = last_log, write_interval = log_write_interval)
            last_log_loss=write_log_performance_py(log_file=log_file_loss, history=history_loss.numpy().tolist(), last_log = last_log_loss, write_interval = log_write_interval)
    
      
      acc_test=n_matches_test/n_total_test
      bacc_test=torch.sum(torch.diagonal(confusion_matrix_test)/torch.sum(confusion_matrix_test,dim=1))/n_classes
      avg_iota_test=torch.diagonal(confusion_matrix_test)/(torch.sum(confusion_matrix_test,dim=0)+torch.sum(confusion_matrix_test,dim=1)-torch.diagonal(confusion_matrix_test))
      avg_iota_test=torch.sum(avg_iota_test)/n_classes    
    
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
      
      history_avg_iota[0,epoch]=avg_iota_train
      history_avg_iota[1,epoch]=avg_iota_val
      history_avg_iota[2,epoch]=avg_iota_test
    else:
      history_loss[0,epoch]=train_loss/len(trainloader)
      history_loss[1,epoch]=val_loss/len(valloader)
      
      history_acc[0,epoch]=acc_train
      history_acc[1,epoch]=acc_val
      
      history_bacc[0,epoch]=bacc_train
      history_bacc[1,epoch]=bacc_val
      
      history_avg_iota[0,epoch]=avg_iota_train
      history_avg_iota[1,epoch]=avg_iota_val

    #Trace---------------------------------------------------------------------
    if trace>=1:
      if test_data is None:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} AI {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f}  AI: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss/len(trainloader),
          acc_train,
          bacc_train,
          avg_iota_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val,
          avg_iota_val))
      else:
        print("Epoch: {}/{} Train Loss: {:.4f} ACC {:.4f} BACC {:.4f} AI {:.4f} | Val Loss: {:.4f} ACC: {:.4f} BACC: {:.4f} AI {:.4f} | Test Loss: {:.4f} ACC: {:.4f} BACC: {:.4f} AI: {:.4f}".format(
          epoch+1,
          epochs,
          train_loss/len(trainloader),
          acc_train,
          bacc_train,
          avg_iota_train,
          val_loss/len(valloader),
          acc_val,
          bacc_val,
          avg_iota_val,
          test_loss/len(testloader),
          acc_test,
          bacc_test,
          avg_iota_test))
          
    #Callback-------------------------------------------------------------------
    if use_callback==True:
      if avg_iota_val>best_val_avg_iota:
        if trace>=1:
          print("Val Avg. Iota increased from {:.4f} to {:.4f}".format(best_val_avg_iota,avg_iota_val))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_val_avg_iota=avg_iota_val
        best_acc=acc_val
        best_val_loss=val_loss/len(valloader)
      
      if avg_iota_val==best_val_avg_iota and acc_val>best_acc:
        if trace>=1:
          print("Val Accuracy increased from {:.4f} to {:.4f}".format(best_acc,acc_val))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_acc=acc_val
        best_val_avg_iota=avg_iota_val
        best_val_loss=val_loss/len(valloader)
        
      if avg_iota_val==best_val_avg_iota and acc_val==best_acc and val_loss/len(valloader)<best_val_loss:
        if trace>=1:
          print("Val Loss decreased from {:.4f} to {:.4f}".format(best_val_loss,val_loss/len(valloader)))
          print("Save checkpoint to {}".format(filepath))
        torch.save(model.state_dict(),filepath)
        best_bacc=bacc_val
        best_acc=acc_val
        best_val_avg_iota=avg_iota_val
        best_val_loss=val_loss/len(valloader)
          
    #Check if there are furhter information for training-----------------------
    # If there are no addtiononal information. Stop training and continue
    if train_loss/len(trainloader)<0.0001 and acc_train==1 and bacc_train==1:
      break
  
  #Finalize--------------------------------------------------------------------
  if use_callback==True:
    if trace>=1:
      print("Load Best Weights from {}".format(filepath))
    model.load_state_dict(torch.load(filepath))
    #safetensors.torch.load_model(model=model,filename=filepath)


  history={
    "loss":history_loss.numpy(),
    "accuracy":history_acc.numpy(),
    "balanced_accuracy":history_bacc.numpy(),
    "avg_iota":history_avg_iota.numpy()} 

  return history

def TeProtoNetBatchEmbedDistance(model,dataset_q,batch_size):
  
  device=('cuda' if torch.cuda.is_available() else 'cpu')
  
  if device=="cpu":
    dtype=float
    model.to(device,dtype=dtype)
  else:
    dtype=torch.double
    model.to(device,dtype=dtype)
    
  model.eval()
  predictionloader=torch.utils.data.DataLoader(
    dataset_q,
    batch_size=batch_size,
    shuffle=False)

  with torch.no_grad():
    iteration=0
    for batch in predictionloader:
      inputs=batch["input"]
      inputs = inputs.to(device,dtype=dtype)
      
      predictions=model.embed(inputs)
      distances=model.get_distances(inputs)
      
      if iteration==0:
        predictions_list=predictions.to("cpu")
        distance_list=distances.to("cpu")
      else:
        predictions_list=torch.concatenate((predictions_list,predictions.to("cpu")), axis=0, out=None)
        distance_list=torch.concatenate((distance_list,distances.to("cpu")), axis=0, out=None)
      iteration+=1
  
  return predictions_list, distance_list      
