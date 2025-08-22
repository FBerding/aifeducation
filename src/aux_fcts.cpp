// This file is part of the R package "aifeducation".
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 3 as published by
// the Free Software Foundation.
//
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>

// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]


//'Reshape matrix to array
//'
//'Function written in C++ for reshaping a matrix containing sequential data into
//'an array for use with keras.
//'
//'@param matrix \code{matrix} containing the sequential data.
//'@param times \code{size_t} Number of sequences.
//'@param features \code{size_t} Number of features within each sequence.
//'@return Returns an array. The first dimension corresponds to the cases,
//'the second to the times, and the third to the features.
//'
//'@import Rcpp
//'@useDynLib aifeducation, .registration = TRUE
//'@family Utils Developers
//'@export
// [[Rcpp::export]]
arma::cube matrix_to_array_c(arma::mat matrix,
                             size_t times,
                             size_t features){
  size_t i=0;
  size_t j=0;
  size_t f=0;
  size_t index=0;

  //cube(n_rows, n_cols, n_slices)
  arma::cube output_array(matrix.n_rows,
                         times,
                         features);

  for(i=0;i<matrix.n_rows;i++){
    for(j=0;j<times;j++){
      index=j*features;
      for(f=0;f<features;f++){
        output_array(i,j,f)=matrix(i,f+index);
      }
      //output_array.tube(i,j,i,j)=matrix.submat(i,
      //                0+j*(features-1),
      //                i,
      //                (features-1)+j*(features-1));
    }
  }
  return output_array;
}




//' Transform tensor to matrix
//'
//' Function written in C++ for transformation the tensor (with size batch x times x features) to the matrix (with
//'   size batch x times*features)
//'
//' @param tensor `3-D array (cube)` data as tensor (with size batch x times x features)
//' @param times `unsigned integer` times number
//' @param features `unsigned integer` features number
//'
//' @return Returns matrix (with size batch x times*features)
//' @family Utils Developers
//' @export
// [[Rcpp::export]]
arma::mat tensor_to_matrix_c(arma::cube tensor,
                             size_t times,
                             size_t features)
{
  arma::mat output_matrix(tensor.n_rows,
                          times * features);

  for(size_t batch_i = 0; batch_i < tensor.n_rows; ++batch_i)
  {
    for(size_t time_i = 0; time_i < times; ++time_i)
    {
      size_t index = time_i * features;

      for(size_t feature_i = 0; feature_i < features; ++feature_i)
        output_matrix(batch_i, index + feature_i) = tensor(batch_i, time_i, feature_i);

    }
  }
  return output_matrix;
}
