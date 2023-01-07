#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
Rcpp::NumericVector mmv(NumericMatrix m_1, NumericMatrix m_2){
  int i=0;
  int j=0;
  int c=0;
  double tmp_sum=0.0;

  Rcpp::NumericMatrix result(m_1.nrow(),m_2.ncol());
  for(i=0;i<m_1.nrow();i++){
    for(j=0;j<m_2.ncol();j++){
      tmp_sum=0.0;
      for(c=0;c<m_1.ncol();c++){
        tmp_sum=tmp_sum+m_1(i,c)*m_2(c,j);
      }
      result(i,j)=tmp_sum;
    }
  }

  return result(0,_);
}
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
Rcpp::NumericVector mvv(NumericMatrix m_1, NumericVector m_2){
  int i=0;
  int j=0;
  double tmp_sum=0.0;
  Rcpp::NumericVector result(m_1.nrow());

  for(i=0;i<m_1.nrow();i++){
    tmp_sum=0.0;
    for(j=0;j<m_2.length();j++){
      tmp_sum=tmp_sum+m_1(i,j)*m_2(j);
    }
    result(i)=tmp_sum;
  }
  return result;
}
//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
Rcpp::List calc_iota2_object(Rcpp::NumericVector true_values,
                             Rcpp::NumericVector assigned_values){
  int n_levels=Rcpp::table(true_values).length();
  Rcpp::NumericMatrix freq_table(n_levels,n_levels);
  Rcpp::NumericMatrix tmp_freq_table(n_levels,n_levels);
  Rcpp::NumericMatrix aem(n_levels,n_levels);
  Rcpp::NumericMatrix aem_t(n_levels,n_levels);
  Rcpp::NumericVector true_sizes(n_levels);
  Rcpp::NumericVector iota2(n_levels);
  Rcpp::NumericVector iota2e1(n_levels);
  Rcpp::NumericVector iota2e2(n_levels);
  Rcpp::NumericVector denominator(n_levels);
  Rcpp::NumericVector tmp_correct(n_levels);
  Rcpp::NumericVector tmp_diag(n_levels);
  Rcpp::NumericVector tmp_forgotten(n_levels);
  Rcpp::NumericVector tmp_wrong(n_levels);

  int i=0;
  int  j=0;
  int n=true_values.length();
  int tmp_freq=0;
  int row_index=0;
  int col_index=0;
  int max_index=0;


  for(i=0;i<n;i++){
    row_index=true_values(i);
    col_index=assigned_values(i);
    tmp_freq=freq_table(row_index,col_index)+1;
    freq_table(row_index,col_index)=tmp_freq;
    tmp_freq=true_sizes[row_index]+1;
    true_sizes[row_index]=tmp_freq;
  }

  true_sizes=true_sizes/Rcpp::sum(true_sizes);

  for(i=0;i<n_levels;i++){
    aem(i,_)=freq_table(i,_)/Rcpp::sum(freq_table(i,_));
    max_index=Rcpp::which_max(aem(i,_));
    if(max_index!=i){
      aem(i,i)=aem(i,max_index);
      aem(i,_)=aem(i,_)/Rcpp::sum(aem(i,_));
    }
    tmp_diag(i)=aem(i,i);
  }
  //Rcout<<aem<<"\n";
  for(i=0;i<aem.ncol();i++){
    for(j=0;j<aem.ncol();j++){
      aem_t(i,j)=aem(j,i);
    }
  }

  for(i=0;i<tmp_correct.length();i++){
    tmp_correct(i)=true_sizes(i)*tmp_diag(i);
    tmp_forgotten(i)=true_sizes(i)*(1-tmp_diag(i));
  }

  tmp_wrong=mvv(aem_t,true_sizes)-tmp_correct;


  denominator=tmp_correct+tmp_forgotten+tmp_wrong;

  iota2=tmp_correct/denominator;
  iota2e1=tmp_forgotten/denominator;
  iota2e2=tmp_wrong/denominator;

  Rcpp::List results=Rcpp::List::create(aem, true_sizes,iota2,iota2e1,iota2e2);
  return results;
}
//'-----------------------------------------------------------------------------

//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
arma::vec expected_category(arma::mat x){

  int i=0;
  int k=0;

  k=x.n_rows;

  arma::vec tmp_expected_category(k,arma::fill::zeros);
  for(i=0;i<k;i++){
    tmp_expected_category(i)=x.row(i).index_max();
    //tmp_expected_category(i)=Rcpp::which_max(x(i,_));
  }
  return tmp_expected_category;
}
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
//' [[Rcpp::export]]
arma::vec err_mse(arma::vec x){
  int k=x.n_elem;
  int i=0;
  arma::vec tmp_result=arma::pow(x,2);

  //for(i=0;i<k;i++){
  //  tmp_result(i)=x(i)*x(i);
  //}
  return tmp_result;
}
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
// [[Rcpp::export]]
double grad_mse(double x){
  return -2*x;
}
//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
double err_cross_entropy(arma::vec x,
                         arma::vec y){
  double tmp_sum=0.0;

  //int k=x.n_elem;
  //int i=0;
  //Rcpp::NumericVector log_y=Rcpp::log(y);

  //for(i=0;i<k;i++){
  //  tmp_sum=tmp_sum-x(i)*log_y(i);
  //}

  tmp_sum=arma::accu((-1)*(x%arma::log(y)));

  return tmp_sum;
}
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
double grad_cross_entropy(double x,double y){
  return -x/y;
}
//----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
double err_iota2(arma::vec output,
                 int true_c,
                 Rcpp::List iota2object){
  Rcpp::NumericVector iota2=iota2object(2);

  double iota_error=1-Rcpp::sum(iota2)/iota2.length();
  iota_error=output(true_c)*iota_error;
  return iota_error;
}
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
double grad_iota2(double x,
                  int true_c,
                  int assigned_c,
                  Rcpp::List iota2object){
  double grad=0.0;
  double tmp_error=0.0;
  Rcpp::NumericMatrix aem=iota2object[0];

  if(assigned_c==true_c){
    tmp_error=1-aem(true_c,assigned_c);
  } else {
    tmp_error=aem(true_c,assigned_c);
  }

  grad=-x*tmp_error*tmp_error;
  return grad;

}


//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
arma::vec act_sigmoid(arma::vec x){
  int k=x.n_elem;
  int i=0;
  arma::vec tmp_results(k);
  arma::vec tmp_exp=arma::exp(-x);

  for(i=0;i<k;i++){
    tmp_results(i)=1/(1+tmp_exp(i));
  }

  return tmp_results;
}
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
double grad_sigmoid(double o_k,
                    double o_j){
  double grad=o_k*(1-o_k)*o_j;
  return grad;
}
//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
arma::vec act_relu(arma::vec x){
  int k=x.n_elem;
  int i=0;
  arma::vec tmp_results(k);
  arma::vec tmp_value_pair(2);
  tmp_value_pair(0)=0;

  for(i=0;i<k;i++){
    tmp_value_pair(1)=x(i);
    tmp_results(i)=max(tmp_value_pair);
  }

  return tmp_results;
}
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
double grad_relu(double o_k,
                 double o_j){
  double grad=0.0;
  if(o_k>0){
    grad=1*o_j;
  } else {
    grad=0;
  }
  return grad;
}
//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
arma::vec act_tanh(arma::vec x){
  return arma::tanh(x);
}
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
// [[Rcpp::export]]
double grad_tanh(double o_k,
                 double o_j){
  double grad=0.0;
  Rcpp::NumericVector tmp(1);
  tmp(0)=o_k;
  Rcpp::NumericVector tmp_tanh=Rcpp::tanh(tmp);

  grad=(1-tmp_tanh(0)*tmp_tanh(0))*o_j;
  return grad;
}
//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
arma::vec act_softmax(arma::vec x){
  arma::vec nominator=arma::exp(x);
  double denominator=arma::accu(nominator);
  arma::vec result=nominator/denominator;
  return result;
  //return Rcpp::tanh(x);
}

//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
double grad_softmax(Rcpp::NumericVector x,int k){
  Rcpp::NumericVector tmp=Rcpp::exp(x);
  double sum_x=Rcpp::sum(tmp);
  double result_1=tmp(k)/sum_x;
  double result=result_1-result_1*result_1;
  return result;
}

//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
// [[Rcpp::export]]
Rcpp::List multi_net_train(arma::mat input,
                           Rcpp::CharacterVector output,
                           arma::mat test_input,
                           Rcpp::CharacterVector test_output,
                           Rcpp::CharacterVector output_levels,
                           Rcpp::NumericVector hidden,
                           double learningrate,
                           Rcpp::String act_fct,
                           Rcpp::String act_fct_last,
                           Rcpp::String err_msr_last,
                           int freq_recalc_iota2,
                           int max_iter,
                           double cr_rel_change,
                           double cr_abs_error,
                           Rcpp::String monitor,
                           int patience,
                           bool return_best,
                           bool trace){

  //Definition of objects--------------------------------------------------------
  int n_layer=hidden.length()+2;
  int n_input=input.n_cols;
  int n_output=output_levels.length();

  Rcpp::NumericVector vec_true_c(input.n_rows);
  Rcpp::NumericVector vec_assigned_c(input.n_rows);

  Rcpp::NumericVector vec_true_c_test(test_input.n_rows);
  Rcpp::NumericVector vec_assigned_c_test(test_input.n_rows);

  Rcpp::NumericMatrix aem(output.length(),output.length());
  Rcpp::NumericVector iota2(output.length());

  Rcpp::List iota2object_init_train=Rcpp::List::create(vec_true_c,iota2);
  Rcpp::List iota2object_init_test=Rcpp::List::create(vec_true_c,iota2);
  Rcpp::List iota2object_train=Rcpp::List::create(vec_true_c,iota2);
  Rcpp::List iota2object_test=Rcpp::List::create(vec_true_c,iota2);

  Rcpp::NumericMatrix history(max_iter,3);
  Rcpp::NumericVector history_enty(3);

  double best_loss=10000.0;

  double error_sum=0.0;
  double tmp_error=0.0;

  double test_error_sum=0.0;
  double test_tmp_error=0.0;

  double monitor_loss=0.0;
  double monitor_rel_change=0.0;

  Rcpp::NumericVector patience_vec(patience);
  int index_patience_max=0;
  int index_history=0;

  int recalc_counter=0;

  arma::field<arma::vec> res_list(n_layer-1);
  arma::field<arma::vec> error_list(n_layer-1);
  arma::field<arma::vec> res_list_iota2 (n_layer-1);
  arma::rowvec tmp_row_vec;
  arma::colvec tmp_col_vec;

  Rcpp::String current_function;
  arma::rowvec upper_vec;
  arma::vec lower_vec;

  //Rcpp::List res_list_iota2;

  //Creating output_matrix--------------------------------------------------------
  int n_levels=output_levels.length();
  //Rcpp::NumericVector error_output(n_levels);
  arma::vec error_output(n_levels);
  double n_cases=output.length();
  int i=0;
  int i_iota2=0;
  int j=0;
  int r=0;
  int r_iota2=0;
  int k=0;

  Rcpp::NumericVector tmp_vec_1;
  Rcpp::NumericVector tmp_vec_2;
  Rcpp::NumericMatrix tmp_matrix_1;
  Rcpp::NumericMatrix tmp_matrix_2;
  arma::vec atmp_vec_1;
  arma::vec atmp_vec_2;
  arma::mat atmp_matrix_1;
  arma::mat atmp_matrix_2;

  arma::vec atmp_act_grad_vec;
  arma::mat atmp_act_grad_mat;
  arma::vec atmp_error_grad_vec;
  arma::mat atmp_error_grad_mat;

  double tmp_act_grad=0.0;
  double tmp_error_grad=0.0;

  Rcpp::String tmp_level;
  //Rcpp::NumericMatrix output_matrix(n_cases,
  //                                  n_levels);
  arma::mat output_matrix(n_cases,
                          n_levels);
  for(i=0;i<n_cases;i++){
    tmp_level=output(i);
    for(j=0;j<n_levels;j++){
      if(tmp_level==output_levels(j)){
        output_matrix(i,j)=0.99;
      } else {
        output_matrix(i,j)=0.01/n_levels;
      }
    }
  }
  //Creating output_matrix for test data----------------------------------------
  double n_cases_test=test_output.length();

  //Rcpp::NumericMatrix test_output_matrix(n_cases_test,
  //                                       n_levels);
  arma::mat test_output_matrix(n_cases_test,
                                         n_levels);

  for(i=0;i<n_cases_test;i++){
    tmp_level=test_output(i);
    for(j=0;j<n_levels;j++){
      if(tmp_level==output_levels(j)){
        test_output_matrix(i,j)=0.99;
      } else {
        test_output_matrix(i,j)=0.01/n_levels;
      }
    }
  }

  //Initializing Layers----------------------------------------------------------
  Rcpp::NumericVector sample_range;

  sample_range=Rcpp::seq(-30.0, 30.0);
  //sample_range=Rcpp::seq_len(1);
  sample_range=sample_range/100;

  //Initializing objects---------------------------------------------------------
  //for(i=0;i<(n_layer-1);i++){
  //  res_list.push_back(Rcpp::NumericVector(1));
  //  res_list_iota2.push_back(Rcpp::NumericVector(1));
  //  error_list.push_back(Rcpp::NumericVector(1));
  //}
  for(r=0;r<input.n_rows;r++){
    atmp_vec_1=output_matrix.row(r).as_col();
    vec_true_c(r)=atmp_vec_1.index_max();
  }
  for(r=0;r<test_input.n_rows;r++){
    atmp_vec_1=test_output_matrix.row(r).as_col();
    vec_true_c_test(r)=atmp_vec_1.index_max();
  }

  //Initializing weights----------------------------------------------------------
  arma::field<arma::mat> wts_list(n_layer-1);
  arma::field<arma::mat> best_wts;
  arma::field<arma::mat> delta_wts_list(n_layer-1);
  arma::mat delta_wts;

  for(i=0;i<(n_layer-1);i++){
    if(i==0){
      //Rcpp::NumericMatrix tmp_wts(hidden(i),n_input);
      arma::mat tmp_wts(hidden(i),n_input);
      tmp_wts.zeros();
      for(j=0;j<hidden(i);j++){
        tmp_vec_1=Rcpp::sample(
          sample_range,
          n_input,
          true,
          R_NilValue);
        tmp_wts.row(j)=Rcpp::as<arma::rowvec>(tmp_vec_1);
      }
      wts_list(i)=1*tmp_wts;
      delta_wts_list(i)=1*tmp_wts;
    } else if(i==(n_layer-2)){
      //Rcpp::NumericMatrix tmp_wts(n_output,hidden(i-1));
      arma::mat tmp_wts(n_output,hidden(i-1));
      tmp_wts.zeros();
      for(j=0;j<n_output;j++){
        tmp_vec_1=Rcpp::sample(
          sample_range,
          hidden(i-1),
          true,
          R_NilValue);
        tmp_wts.row(j)=Rcpp::as<arma::rowvec>(tmp_vec_1);
      }
      wts_list(i)=1*tmp_wts;
      delta_wts_list(i)=1*tmp_wts;
    } else {
      //Rcpp::NumericMatrix tmp_wts(hidden(i),hidden(i-1));
      arma::mat tmp_wts(hidden(i),hidden(i-1));
      tmp_wts.zeros();
      for(j=0;j<hidden(i);j++){
        tmp_vec_1=Rcpp::sample(
          sample_range,
          hidden(i-1),
          true,
          R_NilValue);
        tmp_wts.row(j)=Rcpp::as<arma::rowvec>(tmp_vec_1);
      }
      wts_list(i)=1*tmp_wts;
      delta_wts_list(i)=1*tmp_wts;
    }
  }

  //Setting activation functions--------------------------------------------------
  arma::vec (*activation)(arma::vec );
  double (*act_gradient)(double, double);
  activation=&act_relu;
  act_gradient=&grad_relu;

  if(act_fct=="sigmoid"){
    activation=&act_sigmoid;
    act_gradient=&grad_sigmoid;
  } else if (act_fct=="relu"){
    activation=&act_relu;
    act_gradient=&grad_relu;
  } else if (act_fct=="tanh"){
    activation=&act_tanh;
    act_gradient=&grad_tanh;
  }

  //Setting last activation functions--------------------------------------------------
  arma::vec (*activation_last)(arma::vec );
  double (*act_gradient_last)(double, double);
  activation_last=&act_relu;
  act_gradient_last=&grad_relu;

  //Rcpp::NumericVector (*activation_last_vec)(Rcpp::NumericVector);
  double (*act_gradient_last_vec)(Rcpp::NumericVector, int);
  act_gradient_last_vec=&grad_softmax;

  if(act_fct_last=="sigmoid"){
    activation_last=&act_sigmoid;
    act_gradient_last=&grad_sigmoid;
  } else if (act_fct_last=="relu"){
    activation_last=&act_relu;
    act_gradient_last=&grad_relu;
  } else if (act_fct_last=="tanh"){
    activation_last=&act_tanh;
    act_gradient_last=&grad_tanh;
  } else if(act_fct_last=="softmax"){
    activation_last=&act_softmax;
    act_gradient_last_vec=&grad_softmax;
    //act_gradient_last=&grad_tanh;
  }

  //setting last error functions--------------------------------------------------
  double (*err_gradient)(double);
  err_gradient=&grad_mse;

  arma::vec (*err_fct_last)(arma::vec );
  double (*err_gradient_last)(double);
  err_fct_last=&err_mse;
  err_gradient_last=&grad_mse;

  double (*err_fct_last_vec)(arma::vec ,arma::vec );
  double (*err_gradient_last_vec)(double,double);
  err_gradient_last_vec=&grad_cross_entropy;
  err_fct_last_vec=&err_cross_entropy;

  double (*err_fct_iota2)(arma::vec ,int,Rcpp::List);
  double (*err_grad_iota2)(double,int,int,Rcpp::List);
  err_fct_iota2=&err_iota2;
  err_grad_iota2=&grad_iota2;

  if(err_msr_last=="mse"){
    err_gradient_last=&grad_mse;
    err_fct_last=&err_mse;
  } else if(err_msr_last=="cross_entropy"){
    err_gradient_last_vec=&grad_cross_entropy;
    err_fct_last_vec=&err_cross_entropy;
  } else if(err_msr_last=="iota2"){
    err_fct_iota2=&err_iota2;
    err_grad_iota2=&grad_iota2;
  }

  //Calculating initial error----------------------------------------------------
   if(err_msr_last=="iota2"){
    for(r=0;r<input.n_rows;r++){
      //Feedforward for all rows in input
      for(i=0;i<(n_layer-1);i++){
        if(i==0){
          res_list(i)=activation(wts_list(0)*input.row(r).as_col());
        } else if(i==((n_layer-1)-1)){
          res_list(i)=activation_last(wts_list(i)*res_list(i-1));
        } else {
          res_list(i)=activation(wts_list(i)*res_list(i-1));
        }
      }
      atmp_vec_2=res_list(n_layer-2);
      vec_assigned_c(r)=atmp_vec_2.index_max();
    }
    iota2object_init_train=calc_iota2_object(vec_true_c,vec_assigned_c);
  }

   for(r=0;r<input.n_rows;r++){
     //Feedforward for all rows in input
     for(i=0;i<(n_layer-1);i++){
       if(i==0){
         res_list(i)=activation(wts_list(0)*input.row(r).as_col());
       } else if(i==((n_layer-1)-1)){
         res_list(i)=activation_last(wts_list(i)*res_list(i-1));
       } else {
         res_list(i)=activation(wts_list(i)*res_list(i-1));
       }
     }

    atmp_vec_1=output_matrix.row(r).as_col();
    atmp_vec_2=res_list(n_layer-2);
    if(err_msr_last=="cross_entropy"){
      tmp_error=err_fct_last_vec(output_matrix.row(r).as_col(),res_list(n_layer-2));
      error_sum=error_sum+tmp_error/n_cases;
    } else if(err_msr_last=="mse"){
      arma::vec tmp_sum=err_fct_last(atmp_vec_1-atmp_vec_2);
      tmp_error=arma::accu(tmp_sum);
      error_sum=error_sum+tmp_error/n_cases;
    } else if(err_msr_last=="iota2"){
      error_output=atmp_vec_1-atmp_vec_2;
      error_sum=error_sum+err_fct_iota2(error_output, vec_true_c(r),iota2object_init_train)/n_cases;
    }
  }

  //Calculating initial error for test data---------------------------------------
  if(err_msr_last=="iota2"){
    //Feedforward for all rows in input
    for(r=0;r<test_input.n_rows;r++){
      for(i=0;i<(n_layer-1);i++){
        if(i==0){
          res_list(i)=activation(wts_list(0)*test_input.row(r).as_col());
        } else if(i==((n_layer-1)-1)){
          res_list(i)=activation_last(wts_list(i)*res_list(i-1));
        } else {
          res_list(i)=activation(wts_list(i)*res_list(i-1));
        }
      }
      atmp_vec_2=res_list(n_layer-2);
      vec_assigned_c_test(r)=atmp_vec_2.index_max();
    }
    iota2object_init_test=calc_iota2_object(vec_true_c_test,vec_assigned_c_test);
  }

  for(r=0;r<test_input.n_rows;r++){
    //Feedforward for all rows in input
    for(i=0;i<(n_layer-1);i++){
      if(i==0){
        res_list(i)=activation(wts_list(0)*test_input.row(r).as_col());
      } else if(i==((n_layer-1)-1)){
        res_list(i)=activation_last(wts_list(i)*res_list(i-1));
      } else {
        res_list(i)=activation(wts_list(i)*res_list(i-1));
      }
    }

    atmp_vec_1=test_output_matrix.row(r).as_col();
    atmp_vec_2=res_list(n_layer-2);
    if(err_msr_last=="cross_entropy"){
      test_tmp_error=err_fct_last_vec(test_output_matrix.row(r).as_col(),res_list(n_layer-2));
      test_error_sum=error_sum+tmp_error/n_cases_test;
    } else if(err_msr_last=="mse"){
      arma::vec test_tmp_sum=err_fct_last(atmp_vec_1-atmp_vec_2);
      test_tmp_error=arma::accu(test_tmp_sum);
      test_error_sum=test_error_sum+test_tmp_error/n_cases_test;
    } else if(err_msr_last=="iota2"){
      error_output=atmp_vec_1-atmp_vec_2;
      test_error_sum=test_error_sum+err_fct_iota2(error_output, vec_true_c_test(r),iota2object_init_test)/n_cases_test;
    }
  }

  //Printing initial error--------------------------------------------------------
  if(trace==true){
    Rcout <<"initial loss: " << error_sum
          <<" initial test loss: "<<test_error_sum<<"\n";
  }
  history(0,0)=0.0;
  history(0,1)=error_sum;
  history(0,2)=test_error_sum;

  //Setting initial monitor-------------------------------------------------------
  double rel_change=9999.99;
  double test_rel_change=9999.99;

  if(monitor=="loss"){
    monitor_loss=error_sum;
    monitor_rel_change=rel_change;
  } else if(monitor=="val_loss"){
    monitor_loss=test_error_sum;
    monitor_rel_change=test_rel_change;
  }

  //Starting training------------------------------------------------------------
  int iter=1;

  double error_sum_old=0.0;
  double test_error_sum_old=0.0;

  while(((iter<=max_iter) & (monitor_rel_change>cr_rel_change)) & (monitor_loss > cr_abs_error)){
    error_sum_old=error_sum;
    error_sum=0.0;
    test_error_sum_old=test_error_sum;
    test_error_sum=0.0;

    for(r=0;r<input.n_rows;r++){
      Rcpp::checkUserInterrupt();

      //Feed forward----------------------------------------------------------------
      //Delta Weights
      //Rcpp::List delta_wts_list;
      //Rcpp::List error_list;

      //Rcpp::List res_list;

      //Feedforward for all rows in input
      //Rcpp::List res_list;
      for(i=0;i<(n_layer-1);i++){
        if(i==0){
          res_list(i)=activation(wts_list(0)*input.row(r).as_col());
        } else if(i==((n_layer-1)-1)){
          res_list(i)=activation_last(wts_list(i)*res_list(i-1));
        } else {
          res_list(i)=activation(wts_list(i)*res_list(i-1));
        }
      }

      for(i=(n_layer-1-1);i>=0;i--){
        Rcpp::checkUserInterrupt();
          //arma::mat tmp_wts=wts_list(i);
          //arma::mat delta_wts(n_output,tmp_wts.n_cols);

          //Only relevant for the last layer
         if(i==(n_layer-1-1)){
           atmp_vec_1=output_matrix.row(r).as_col();
           atmp_vec_2=res_list(n_layer-2);
           error_output=atmp_vec_1-atmp_vec_2;
           error_list(i)=1*error_output;
            //Appyling Error Function-----------------------------------------------
            if(err_msr_last=="mse"){
              error_sum=error_sum+arma::accu(err_fct_last(error_output))/n_cases;
            } else if (err_msr_last=="cross_entropy"){
              error_sum=error_sum+err_fct_last_vec(output_matrix.row(r).as_col(),res_list((n_layer-2)))/n_cases;
            } else if (err_msr_last=="iota2"){
              //Calculating the iota2object----------------------------------------
              //for every freq_recalc_iota2 object
              if(((recalc_counter==freq_recalc_iota2) & (r<(input.n_rows-1))) |
                  (r==0)){
                //if(trace==true){
                //  Rcout<<"Recalculating iota2"<<"\n";
                //}
                for(r_iota2=0;r_iota2<input.n_rows;r_iota2++){
                  //Feedforward for all rows in input
                  //Rcpp::List res_list_iota2;
                  for(i_iota2=0;i_iota2<(n_layer-1);i_iota2++){
                    if(i_iota2==0){
                      res_list_iota2(i_iota2)=activation(wts_list(0)*input.row(r_iota2).as_col());
                    } else if(i_iota2==((n_layer-1)-1)){
                      res_list_iota2(i_iota2)=activation_last(wts_list(i_iota2)*res_list_iota2(i_iota2-1));
                    } else {
                      res_list_iota2(i_iota2)=activation(wts_list(i_iota2)*res_list_iota2(i_iota2-1));
                    }
                  }

                  atmp_vec_2=res_list_iota2(n_layer-2);
                  vec_assigned_c(r_iota2)=atmp_vec_2.index_max();
                }
                iota2object_train=calc_iota2_object(vec_true_c,vec_assigned_c);
                iota2=iota2object_train(2);
                recalc_counter=0;
              }
              error_sum=error_sum+err_fct_iota2(error_output,vec_true_c(r),iota2object_train)/n_cases;
            }
            //Applying Gradient Error Function-------------------------------
            if(err_msr_last=="mse"){
              atmp_error_grad_vec=-2*error_output;
              atmp_error_grad_mat=atmp_error_grad_vec*arma::rowvec(wts_list(i).n_cols, arma::fill::ones);
            } else if(err_msr_last=="cross_entropy"){
              atmp_vec_1=res_list((n_layer-2));
              atmp_error_grad_vec=(-1)*output_matrix.row(r).as_col()/atmp_vec_1;
              atmp_error_grad_mat=atmp_error_grad_vec*arma::rowvec(wts_list(i).n_cols, arma::fill::ones);
            } else if(err_msr_last=="iota2"){
              atmp_vec_1=res_list(i);
              atmp_error_grad_vec.resize(wts_list(i).n_rows);
              for(k=0;k<wts_list(i).n_rows;k++){
                atmp_error_grad_vec.resize(wts_list(i).n_rows);
                atmp_error_grad_vec(k)=err_grad_iota2(atmp_vec_1(k),vec_true_c(r),k,iota2object_train);
              }
              //if(r<=3){
              //Rcout<<"true cat: "<<vec_true_c<<"\n";
              //Rcout<<"assigned cat: "<<vec_assigned_c<<"\n";
              //atmp_error_grad_vec.print();
              //}
              atmp_error_grad_mat=atmp_error_grad_vec*arma::rowvec(wts_list(i).n_cols, arma::fill::ones);
            }
         } else {
           error_output=error_list(i);
           atmp_error_grad_vec=-2*error_output;
           atmp_error_grad_mat=atmp_error_grad_vec*arma::rowvec(wts_list(i).n_cols, arma::fill::ones);
          }

          //Applying Gradient Activation Function-------------------------------

          if(i==(n_layer-1-1)){
            current_function=act_fct_last;
            upper_vec=res_list(i).as_row();
            lower_vec=res_list(i-1);
          } else if(i==0) {
            current_function=act_fct;
            upper_vec=res_list(i).as_row();
            lower_vec=input.row(r).as_col();
          } else {
            current_function=act_fct;
            upper_vec=res_list(i).as_row();
            lower_vec=res_list(i-1);
          }

          if(current_function=="softmax"){
            tmp_col_vec=arma::exp(upper_vec.as_col())/arma::accu(arma::exp(upper_vec.as_col()));
            atmp_act_grad_mat=(tmp_col_vec-(tmp_col_vec%tmp_col_vec))*arma::rowvec(lower_vec.n_elem, arma::fill::ones);
          } else if(current_function=="sigmoid"){
            tmp_row_vec=upper_vec;
            tmp_col_vec=lower_vec;
            atmp_act_grad_mat=(tmp_row_vec.as_col()%(1-tmp_row_vec.as_col()))*tmp_col_vec.as_row();
          } else if(current_function=="tanh"){
            tmp_row_vec=upper_vec;
            tmp_col_vec=lower_vec;
            atmp_act_grad_mat=(1-(arma::tanh(tmp_row_vec.as_col())%arma::tanh(tmp_row_vec.as_col())))*tmp_col_vec.as_row();
          } else if(current_function=="relu"){
            tmp_row_vec=upper_vec;
            tmp_col_vec=lower_vec;
            atmp_act_grad_mat.reshape(wts_list(i).n_rows,wts_list(i).n_cols);
            for(j=0;j<wts_list(i).n_cols;j++){
              for(k=0;k<wts_list(i).n_rows;k++){
                if(tmp_row_vec(k)>0){
                  atmp_act_grad_mat(k,j)=tmp_col_vec(j);
                } else {
                  atmp_act_grad_mat(k,j)=0;
                }
              }
            }
          }


          //Calculating Delta Weights-------------------------------
          //atmp_error_grad_mat.print();
          //atmp_act_grad_mat.print();
              //delta_wts=learningrate*(atmp_error_grad_mat%atmp_act_grad_mat);
              //delta_wts_list(i)=delta_wts;
              //if(r<=3 & i==(n_layer-2)){
              //Rcout<<"true outpur "<< r<<"\n";
              //output_matrix.row(r).as_col().print();
              //Rcout<<"final layer "<< r<<"\n";
              //res_list(i).print();
              //Rcout<<"error "<< r<<"\n";
              //error_list(i).print();
              //Rcout<<"grad error"<<r<<" \n";
              //atmp_error_grad_mat.print();
              //Rcout<<"grad act"<<r<<" \n";
              //atmp_act_grad_mat.print();
              //}
              delta_wts_list(i)=learningrate*(atmp_error_grad_mat%atmp_act_grad_mat);

          //if(r<=2 & i==(n_layer-2)){
          //  delta_wts_list(i).print();
          //}

          //Calculating Error for next layer-----------------------------------
          if(i>0){
            error_list(i-1)=wts_list(i).t()*error_output;
            //error_list(i-1).print();
          }
          //Rcout<<r<<" "<<i<<"test \n";
    }
    //Adjusting weights---------------------------------------------------------
    for(i=0;i<wts_list.n_elem;i++){
      //Rcpp::NumericMatrix tmp_matrix_1=wts_list(i);
      //Rcpp::NumericMatrix tmp_matrix_2=delta_wts_list(i);
      //Rcpp::NumericMatrix tmp_res(tmp_matrix_1.nrow(),tmp_matrix_1.ncol());
      //for(j=0;j<tmp_matrix_1.nrow();j++){
      //  tmp_res(j,_)=tmp_matrix_1(j,_)-tmp_matrix_2(j,_);
      //}
      //Rcout<<tmp_res<<"\n";
      wts_list(i)=wts_list(i)-delta_wts_list(i);

    }
    recalc_counter++;
    }

    rel_change=(error_sum_old-error_sum)/error_sum_old;

    //Calculating error of test data----------------------------------------------
    if(err_msr_last=="iota2"){
      for(r=0;r<test_input.n_rows;r++){
        //Feedforward for all rows in input
        //Rcpp::List res_list;
        //Rcpp::NumericVector tmp_matrix_1 =wts_list[0];


        for(i=0;i<(n_layer-1);i++){
          //Rcout<<i<<"\n";
          //Rcout<<(n_layer-1)<<"\n";
          if(i==0){
            res_list(i)=activation(wts_list(0)*test_input.row(r).as_col());
          } else if(i==((n_layer-1)-1)){
            res_list(i)=activation_last(wts_list(i)*res_list(i-1));
          } else {
            res_list(i)=activation(wts_list(i)*res_list(i-1));
          }
        }
        atmp_vec_2=res_list(n_layer-2);
        vec_assigned_c_test(r)=atmp_vec_2.index_max();
        iota2object_test=calc_iota2_object(vec_true_c_test,vec_assigned_c_test);
      }
    }

    for(r=0;r<test_input.n_rows;r++){
      //Feedforward for all rows in input
      //Rcpp::List res_list;
      for(i=0;i<(n_layer-1);i++){
        if(i==0){
          res_list(i)=activation(wts_list(0)*test_input.row(r).as_col());
        } else if(i==((n_layer-1)-1)){
          res_list(i)=activation_last(wts_list(i)*res_list(i-1));
        } else {
          res_list(i)=activation(wts_list(i)*res_list(i-1));
        }
      }
      //Rcout<<i<<"\n";
      //Rcout<<(n_layer-1)<<"\n";
      atmp_vec_1=test_output_matrix.row(r).as_col();
      atmp_vec_2=res_list(n_layer-2);
      if(err_msr_last=="cross_entropy"){
        test_tmp_error=err_fct_last_vec(test_output_matrix.row(r).as_col(),res_list(n_layer-2));
        test_error_sum=error_sum+test_tmp_error/n_cases_test;
      } else if(err_msr_last=="mse"){
        arma::vec test_tmp_sum=err_fct_last(atmp_vec_1-atmp_vec_2);
        test_tmp_error=arma::accu(test_tmp_sum);
        test_error_sum=test_error_sum+test_tmp_error/n_cases_test;
      } else if(err_msr_last=="iota2"){
        error_output=atmp_vec_1-atmp_vec_2;
        test_error_sum=test_error_sum+err_fct_iota2(error_output, vec_true_c_test(r),iota2object_test)/n_cases_test;
      }
    }
    test_rel_change=(test_error_sum_old-test_error_sum)/test_error_sum_old;

    //Printing results of epoch---------------------------------------------------
    if(trace==true){
      Rcout<<"epoch: "
           <<iter
           <<" loss: "
           <<error_sum
           <<" rel. change: "
           << rel_change
           <<" val. loss: "
           << test_error_sum
           <<" val. rel. change: "
           <<test_rel_change
           <<"\n";
    }

    //Setting monitor values-----------------------------------------------------
    if(monitor=="loss"){
      monitor_loss=error_sum;
      monitor_rel_change=rel_change;
    } else if(monitor=="val_loss"){
      monitor_loss=test_error_sum;
      monitor_rel_change=test_rel_change;
    }

    //Patience-Monitoring-------------------------------------------------------
    if(iter>patience){
      for(r=0;r<patience;r++){
        index_history=iter-r-1;
        patience_vec(r)=history(index_history,2);
      }
      index_patience_max=Rcpp::which_max(patience_vec);
      //if(test_error_sum<0){
        //if(patience_vec[index_patience_max]<0){
        //  monitor_rel_change=(patience_vec[index_patience_max]-0)/patience_vec[index_patience_max]*(-1);
        //} else {
          //monitor_rel_change=(patience_vec[index_patience_max]-0)/patience_vec[index_patience_max];
        //}
      //} else {
        monitor_rel_change=(patience_vec(index_patience_max)-test_error_sum)/patience_vec(index_patience_max);
      //}
    } else {
      monitor_rel_change=10;
    }
    //Rcout<<monitor_rel_change<<"\n";

    //Checking if the current solution is better as the old solution-------------
    if(return_best==true){
      if(test_error_sum<best_loss){
        best_loss=test_error_sum;
        best_wts=wts_list;
      }
    } else {
      best_wts=wts_list;
    }


    //Saving fitting history-----------------------------------------------------
    history(iter,0)=iter;
    history(iter,1)=error_sum;
    history(iter,2)=test_error_sum;
    //history.push_back(history_enty);

    //}
    iter++;
  }

  //Return model----------------------------------------------------------------
  Rcpp::List final_model=Rcpp::List::create(Named("wts") = best_wts, Named("history")=history);
  return final_model;
}

//-----------------------------------------------------------------------------
//' @importFrom Rcpp
//' @useDynLib aifeducation, .registration = TRUE
// [[Rcpp::export]]
arma::vec multi_net_predict_c(arma::field<arma::mat> wts_list,
                                        arma::mat newdata,
                                        Rcpp::String act_fct,
                                        Rcpp::String act_fct_last){

  //arma::mat tmp_wts=wts_list(wts_list.n_elem-1);
  arma::vec expected_cat;
  int n_layer=wts_list.n_elem+1;
  arma::mat predictions(newdata.n_rows,wts_list(n_layer-2).n_rows);
  int i=0;
  int r=0;

  //Setting activation functions--------------------------------------------------
  arma::vec (*activation)(arma::vec);
  double (*act_gradient)(double, double);

  if(act_fct=="sigmoid"){
    activation=&act_sigmoid;
    act_gradient=&grad_sigmoid;
  } else if (act_fct=="relu"){
    activation=&act_relu;
    act_gradient=&grad_relu;
  } else if (act_fct=="tanh"){
    activation=&act_tanh;
    act_gradient=&grad_tanh;
  }

  //Setting last activation functions--------------------------------------------
  arma::vec (*activation_last)(arma::vec);
  double (*act_gradient_last)(double, double);

  //Rcpp::NumericVector (*activation_last_vec)(Rcpp::NumericVector);
  double (*act_gradient_last_vec)(Rcpp::NumericVector, int);

  if(act_fct_last=="sigmoid"){
    activation_last=&act_sigmoid;
    act_gradient_last=&grad_sigmoid;
  } else if (act_fct_last=="relu"){
    activation_last=&act_relu;
    act_gradient_last=&grad_relu;
  } else if (act_fct_last=="tanh"){
    activation_last=&act_tanh;
    act_gradient_last=&grad_tanh;
  } else if(act_fct_last=="softmax"){
    activation_last=&act_softmax;
    act_gradient_last_vec=&grad_softmax;
    act_gradient_last=&grad_tanh;
  }

  //---------------------------------------------------------------------------
  arma::field<arma::vec> res_list(n_layer-1);
  //for(i=0;i<(n_layer-1);i++){
  //  res_list.push_back(Rcpp::NumericVector(1));
  //}

  //Feedforward for all rows in input
  //Rcpp::List res_list;
  for(r=0;r<newdata.n_rows;r++){
    for(i=0;i<(n_layer-1);i++){
      //wts_list(i).print();
      if(i==0){
        res_list(i)=activation(wts_list(0)*newdata.row(r).as_col());
      } else if(i==(n_layer-2)){
        res_list(i)=activation_last(wts_list(i)*res_list(i-1));
        predictions.row(r)=activation_last(wts_list(i)*res_list(i-1)).as_row();
      } else {
        res_list(i)=activation(wts_list(i)*res_list(i-1));
      }
      //Rcout<<i<<"\n";

    }
  }
  expected_cat=expected_category(predictions);
  return expected_cat;
}
