// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

//'Reshape matrix to array
//'
//'@importFrom Rcpp
//'@useDynLib aifeducation, .registration = TRUE
//'@export
// [[Rcpp::export]]
arma::cube matrix_to_array_c(arma::mat matrix,
                             arma::uword times,
                             arma::uword features){
  arma::uword i=0;
  arma::uword j=0;

  //cube(n_rows, n_cols, n_slices)
  arma::cube output_array(matrix.n_rows,
                         times,
                         features);

  for(i=0;i<matrix.n_rows;i++){
    for(j=0;j<times;j++){
      output_array.tube(i,j,i,j)=matrix.submat(i,
                      0+j*(features-1),
                      i,
                      (features-1)+j*(features-1));
    }
  }
  return output_array;
}
