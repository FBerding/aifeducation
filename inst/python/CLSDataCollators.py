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
    def __init__(self, targets, Ns, Nq, separate, shuffle):
        self.Ns = Ns
        self.Nq = Nq
        self.separate = separate
        self.shuffle = shuffle

        targets = targets.cpu()
        self.classes = torch.unique(targets)
        self.n_classes = len(self.classes)
        self.batch_size = self.n_classes * (self.Ns + self.Nq)
        
        self.indices_per_class = {}
        self.cases_per_class = {}
        self.batches_per_class = {}
        
        for c in self.classes:
            c_int = int(c.item())
            self.indices_per_class[c_int] = torch.where(targets == c)[0]
            self.cases_per_class[c_int] = len(self.indices_per_class[c_int])
        
        self.query_indices_per_class = {}
        self.sample_indices_per_class = {}

        if self.separate:
            for c in self.classes:
                c_int = int(c.item())
                n_cases = self.cases_per_class[c_int]
                # Calculate split
                n_sample = int(round(self.Ns / (self.Ns + self.Nq) * n_cases))
                n_sample = max(1, min(n_sample, n_cases - 1))
                n_query = n_cases - n_sample
                # Shuffle
                permutation = self.indices_per_class[c_int][torch.randperm(n_cases)]
                # Assign to sample and query
                self.sample_indices_per_class[c_int] = permutation[:n_sample]
                self.query_indices_per_class[c_int] = permutation[n_sample:]
                self.batches_per_class[c_int] = min(
                    len(self.sample_indices_per_class[c_int]) // self.Ns, 
                    len(self.query_indices_per_class[c_int]) // self.Nq
                )
        else:
            for c in self.classes:
                c_int = int(c.item())
                if self.shuffle:
                    self.indices_per_class[c_int] = self.indices_per_class[c_int][torch.randperm(self.cases_per_class[c_int])]
                self.batches_per_class[c_int] = self.cases_per_class[c_int] // (self.Ns + self.Nq)
        self.number_batches = min(self.batches_per_class.values())

    def __iter__(self):
        samples_dict = {}
        queries_dict = {}
        for c in self.classes:
            c_int = int(c.item())
            if self.separate:
                s_idx = self.sample_indices_per_class[c_int]
                q_idx = self.query_indices_per_class[c_int]
                if self.shuffle:
                    s_idx = s_idx[torch.randperm(len(s_idx))]
                    q_idx = q_idx[torch.randperm(len(q_idx))]
                samples_dict[c_int] = s_idx
                queries_dict[c_int] = q_idx
            else:
                idx = self.indices_per_class[c_int]
                if self.shuffle:
                    idx = idx[torch.randperm(len(idx))]
                samples_dict[c_int] = idx
        # Create Batches
        for i in range(self.number_batches):
            final_batch = []
            if self.separate:
                # Ns
                for c in self.classes:
                    c_int = int(c.item())
                    start, end = i * self.Ns, (i + 1) * self.Ns
                    final_batch.extend(samples_dict[c_int][start:end].tolist())
                # Nq
                for c in self.classes:
                    c_int = int(c.item())
                    start, end = i * self.Nq, (i + 1) * self.Nq
                    final_batch.extend(queries_dict[c_int][start:end].tolist())
            else:
                step = self.Ns + self.Nq
                # Ns
                for c in self.classes:
                    c_int = int(c.item())
                    start = i * step
                    end = start + self.Ns
                    final_batch.extend(samples_dict[c_int][start:end].tolist())
                # Nq
                for c in self.classes:
                    c_int = int(c.item())
                    start = i * step + self.Ns
                    end = start + self.Nq
                    final_batch.extend(samples_dict[c_int][start:end].tolist())
            yield final_batch
      
    def __len__(self):
        return self.number_batches
