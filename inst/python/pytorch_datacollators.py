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
        self.classes=torch.unique(targets).numpy().astype(int)
        #Get the number of classes
        self.n_classes=len(self.classes)
        #Calculate the batch size depending on Ns and Nq
        self.batch_size=self.n_classes*(self.Ns+self.Nq)
        
        #Create dictonary that contains the indexes sorted for every class
        self.indices_per_class={}
        #Create dictornary thats sorts the number of cases per class
        self.cases_per_class={}
        #create ictornary thats stores the number of batches per class
        self.batches_per_class={}
        #Gather indicies per class and cases per class
        for c in self.classes:
          self.indices_per_class[c]=torch.where(targets==c)[0]
          self.cases_per_class[c]=len(self.indices_per_class[c])
        
         #Create dictonary that contains the indexes sorted for every class and query/sample
        self.query_indices_per_class={}
        self.sample_indices_per_class={}
        #Create dictornary that sorts the number of cases per class and query/sample
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
            #Calculate batches per class
            self.batches_per_class[c]=min(self.sample_cases_per_class[c]//self.Ns, self.query_cases_per_class[c]//self.Nq)
        else:
          #Create a random permutation if separate is False and shuffle is False
          #If shuffle is True random sampling is applied during iter
          if self.shuffle is True:
            for c in self.classes:
              self.indices_per_class[c]=self.indices_per_class[c][torch.randperm(self.cases_per_class[c])]
          for c in self.classes:    
            self.batches_per_class[c]=self.cases_per_class[c]//(self.Ns+self.Nq)
          
        #Calculate number of batches
        self.number_batches=min(self.batches_per_class.values())

    def __iter__(self):
      for current_iter in range(self.number_batches):
        #Create list for saving the results per class temporarily 
        batch_sample=[]
        batch_query=[]
        batches_class_sample=[[] for _ in range(self.n_classes)]
        batches_class_query=[[] for _ in range(self.n_classes)]
        batches_sample=[]
        batches_query=[]
        
        if self.separate is False:
          for c in self.classes:
            batch_query=[]
            batch_sample=[]
            if self.shuffle is True:
              permutations=self.indices_per_class[c][torch.randperm(self.cases_per_class[c])]
            else:
              permutations=self.indices_per_class[c]
            counter=1
            for idx in permutations:
              if counter<=self.Ns:
                batch_sample.append(idx)
                counter+=1
              elif counter>self.Ns and counter<=(self.Ns+self.Nq):
                batch_query.append(idx)
                counter+=1
              if(counter==(self.Ns+self.Nq)+1):
                batches_class_sample[c].append(batch_sample)
                batches_class_query[c].append(batch_query)
                batch_query=[]
                batch_sample=[]
                counter=1
            #Result is a list of batches for every class      
        else:
          for c in self.classes:
            batch_sample=[]
            batch_query=[]
            if self.shuffle is True:
              permutations_query=self.query_indices_per_class[c][torch.randperm(self.query_cases_per_class[c])]
              permutations_sample=self.sample_indices_per_class[c][torch.randperm(self.sample_cases_per_class[c])]
            else:
              permutations_query=self.query_indices_per_class[c]
              permutations_sample=self.sample_indices_per_class[c]
            counter=1
            
            for idx in permutations_query:
              if counter<=self.Nq:
                batch_query.append(idx)
                counter+=1
              if counter==self.Nq+1:
                batches_class_query[c].append(batch_query)
                batch_query=[]
                counter=1
            
            counter=1
            for idx in permutations_sample:    
              if counter<=self.Ns:
                batch_sample.append(idx)
                counter+=1
              if counter==self.Ns+1:
                batches_class_sample[c].append(batch_sample)
                batch_sample=[]
                counter=1

        for i in range(self.number_batches):
          final_batch=[]
          for c in self.classes:
            final_batch.extend(batches_class_sample[c][i])
          for c in self.classes:
            final_batch.extend(batches_class_query[c][i])
          final_batch_int = [t.item() for t in final_batch]
          yield final_batch_int
      
    def __len__(self):
      return self.number_batches
