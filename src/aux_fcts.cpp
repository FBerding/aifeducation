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
//'@param times \code{uword} Number of sequences.
//'@param features \code{uword} Number of features within each sequence.
//'@return Returns an array. The first dimension corresponds to the cases,
//'the second to the times, and the third to the features.
//'
//'@import Rcpp
//'@useDynLib aifeducation, .registration = TRUE
//'@family Auxiliary Functions
//'@export
// [[Rcpp::export]]
arma::cube matrix_to_array_c(arma::mat matrix,
                             arma::uword times,
                             arma::uword features){
  arma::uword i=0;
  arma::uword j=0;
  arma::uword f=0;
  arma::uword index=0;

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


//'Transforming classes to one-hot encoding
//'
//'Function written in C++ transforming a vector of classes (int) into
//'a binary class matrix.
//'
//'@param class_vector \code{vector} containing integers for every class. The
//'integers must range from 0 to n_classes-1.
//'@param n_classes \code{int} Total number of classes.
//'@return Returns a \code{matrix} containing the binary representation for
//'every class.
//'
//'@import Rcpp
//'@useDynLib aifeducation, .registration = TRUE
//'@family Auxiliary Functions
//'@export
// [[Rcpp::export]]
 arma::mat to_categorical_c(arma::vec class_vector,
                             arma::uword n_classes){
   arma::uword i=0;
   arma::mat binary_class_rep(class_vector.n_elem, n_classes);

   for(i=0;i<binary_class_rep.n_rows;i++){
     binary_class_rep(i,class_vector(i))=1;
     }

   return binary_class_rep;
 }

