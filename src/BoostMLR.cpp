#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

double Sum_C(NumericVector x);
double Sum_C_NA(NumericVector x);

int length_C_NA(NumericVector x);
int length_integer_C_NA(IntegerVector x);
int length_character_C_NA(CharacterVector x);

double Mean_C(NumericVector x);
double Mean_C_NA(NumericVector x);

IntegerVector Which_C(double x, NumericVector x_set);
IntegerVector Which_C_NA(double x, NumericVector x_set);
IntegerVector Which_direction_C_NA(double x, NumericVector x_set,String direction);
IntegerVector Which_integer_C(int x, IntegerVector x_set);
IntegerVector Which_integer_C_NA(int x, IntegerVector x_set);

IntegerVector create_integer_vector(int start_point, int end_point);

int Which_Min_C(NumericVector x);
int Which_Min_C_NA(NumericVector x);
int Which_Max_C(NumericVector x);
int Which_Max_C_NA(NumericVector x);

List StdVar_C(NumericMatrix MyMat);
List StdVar_C_NA_Supply_MeanSE(NumericMatrix MyMat,NumericVector x_Mean,NumericVector x_Std_Error);
List StdVar_C_NA(NumericMatrix MyMat);

IntegerVector Match_C_NA(NumericVector x_subset, NumericVector x_set);
IntegerVector Approx_Match_C(NumericVector x, NumericVector y);
IntegerVector Approx_Match_C_NA(NumericVector x, NumericVector y);

NumericMatrix Diag_Matrix_C(NumericVector x);

NumericMatrix Matrix_Sum_C(NumericMatrix x, NumericMatrix y);
NumericMatrix Matrix_Sum_C_NA(NumericMatrix x, NumericMatrix y);

double l2Dist_Vector_C(NumericVector x1, NumericVector x2, List ID);
double l2Dist_Vector_C_NA(NumericVector x1, NumericVector x2, List ID);

void set_seed(unsigned long int seed);
NumericVector randomShuffle(NumericVector x, int size, bool setting_seed, unsigned long int seed_value, bool replace = false, sugar::probs_t p = R_NilValue);
IntegerVector int_randomShuffle(IntegerVector x, int size, bool setting_seed, unsigned long int seed_value, bool replace = false, sugar::probs_t p = R_NilValue);
int single_integer_randomShuffle(IntegerVector x, int size, bool setting_seed, unsigned long int seed_value, bool replace = false, sugar::probs_t p = R_NilValue);


IntegerVector Reverse_Ordering(IntegerVector a);

NumericVector RemoveNA(NumericVector x);
IntegerVector RemoveNA_Integer(IntegerVector x);
CharacterVector RemoveNA_Character(CharacterVector x);

double Max_C_NA(NumericVector x);
double Max_upper_limit_C_NA(NumericVector x, double x_upper_limit);
double Min_C_NA(NumericVector x);

NumericVector stl_sort(NumericVector x);
NumericVector stl_sort_NA(NumericVector x);
Rcpp::NumericVector stl_sort_reverse(Rcpp::NumericVector x);
Rcpp::NumericVector stl_sort_reverse_NA(Rcpp::NumericVector x);

NumericVector unique_C(NumericVector x);
NumericVector unique_C_NA(NumericVector x);
CharacterVector unique_character_C_NA(CharacterVector x);
IntegerVector unique_integer_C(IntegerVector x);
IntegerVector unique_integer_C_NA(IntegerVector x);

NumericVector sort_unique_C(NumericVector x);
NumericVector sort_unique_C_NA(NumericVector x);
IntegerVector sort_unique_integer_C(IntegerVector x);
IntegerVector sort_unique_integer_C_NA(IntegerVector x);

int cumulative_integer_sum(IntegerVector x);

int pmax_two_integers(int x1,int x2);

IntegerVector Which_Max_Matrix(NumericMatrix x);
IntegerVector Which_Max_Matrix_NA(NumericMatrix x);
IntegerVector Which_Min_Matrix_NA(NumericMatrix x);
NumericVector rowSums_C(NumericMatrix x);
NumericVector rowSums_C_NA(NumericMatrix x);

LogicalVector isNA(IntegerVector x);
LogicalVector isNA_Numeric(NumericVector x);
bool any_function(LogicalVector x);
bool all_function(LogicalVector x);
IntegerVector Which_missing_Numeric(NumericVector x);
IntegerVector Which_missing_NumericDF(DataFrame x);

double Rho_Inv_C(double Rho_Value,double N_Value);
arma::mat MatrixInversion_Equicorrelation_C(int N_Value, double phi, double rho);
NumericVector Matrix_Vector_Multiplication_C(arma::mat y, NumericVector x);

IntegerVector interaction_two_integer_sets(IntegerVector x, IntegerVector y);
arma::mat Matrix_Multiplication_Arma_C(arma::mat x,arma::mat y);
arma::mat Matrix_Sum_Arma_C_NA(arma::mat x,arma::mat y);
NumericVector Sum_numericVectors(NumericVector x, NumericVector y);

bool all_equal(IntegerVector x, IntegerVector y);
String convert_double_string(double x);
NumericVector round_NumVector(NumericVector x, int d);
double round_double(double x, int d); 
NumericMatrix Convert_DF_into_NumMat(DataFrame x);
arma::mat DiagonalMatrix_C(int Dimension, double x);
NumericMatrix MatrixInversion_R(NumericMatrix x);
arma::mat MatrixInversion_C(arma::mat x);
DataFrame Subsetting_Dataframe(DataFrame x, IntegerVector Index);
IntegerVector create_index_logical_vector(LogicalVector x);
NumericVector create_variable_by_substitution(NumericVector x, NumericVector x_sub);
NumericVector create_sequence(double min_value, double max_value, double by_amount);



// [[Rcpp::export]]
double Sum_C(NumericVector x){
  int n = x.size();
  double Sum = 0;
  for(int i = 0;i < n;++i){
    Sum = Sum + x[i];
  }
  return Sum;
}

// [[Rcpp::export]]
double Sum_C_NA(NumericVector x){
  double Sum;
  if(all(is_na(x) )){
    Sum = NA_REAL;
  }else
  {
    int n = x.size();
    double x_temp;
    Sum = 0;
    for(int i = 0;i < n;++i){
      x_temp = x[i];
      if( (NumericVector::is_na( x_temp ) )) {
        x_temp = 0;
      }
      Sum = Sum + x_temp;
    }
  }
  return Sum;
}

// [[Rcpp::export]]
NumericVector Sum_numericVectors(NumericVector x, NumericVector y)
{
  int n = x.size();
  NumericVector output(n);
  if(n == y.size() )
  {
    for(int i = 0; i < n; ++i)
    {
      output[i] = x[i] + y[i];
    }
  }else
  {
    output = NA_REAL;
  }
  return output;
}

// [[Rcpp::export]]
int length_C_NA(NumericVector x){
  int n = x.size();
  if(any(is_na(x))){
    n = n - sum(is_na(x));  
  }
  return n;
}

// [[Rcpp::export]]
int length_integer_C_NA(IntegerVector x){
  int n = x.size();
  if(any(is_na(x))){
    n = n - sum(is_na(x));  
  }
  return n;
}

// [[Rcpp::export]]
int length_character_C_NA(CharacterVector x){
  int n = x.size();
  if(any(is_na(x))){
    n = n - sum(is_na(x));  
  }
  return n;
}

// [[Rcpp::export]]
double Mean_C(NumericVector x){
  int n = x.size();
  return Sum_C(x)/n;
}

// [[Rcpp::export]]
double Mean_C_NA(NumericVector x){
  return Sum_C_NA(x)/length_C_NA(x);
}

// [[Rcpp::export]]
IntegerVector Which_C(double x, NumericVector x_set){
  int N = x_set.size();
  IntegerVector out_Temp(N);
  int count = 0;
  for(int j = 0; j < N; ++j){
    if(x == x_set[j]){
      count = count + 1;
      out_Temp[count - 1] = j;
    }
  }
  if(count == 0){
    return(0);
  }else
  {
    IntegerVector out(count);
    for(int i = 0; i < count; ++i){
      out[i] = out_Temp[i];
    }
    return out;
  }  
}


// [[Rcpp::export]]
IntegerVector Which_C_NA(double x, NumericVector x_set){
  if(NumericVector::is_na(x)){
    IntegerVector out(1);
    out = NA_INTEGER;
    return out;
  }else
  {
    if(all(is_na(x_set)) ){
      IntegerVector out(1);
      out = NA_INTEGER;
      return out;
    }else
    {
      int N = x_set.size();
      IntegerVector out_Temp(N);
      int count = 0;
      for(int j = 0; j < N; ++j){
        if(x == x_set[j]){
          count = count + 1;
          out_Temp[count - 1] = j;
        }
      }
      if(count == 0){
        return(0);
      }else
      {
        IntegerVector out(count);
        for(int i = 0; i < count; ++i){
          out[i] = out_Temp[i];
        }
        return out;
      } 
    }
  }
}

// [[Rcpp::export]]
IntegerVector Which_direction_C_NA(double x, NumericVector x_set,String direction){
  if(NumericVector::is_na(x)){
    IntegerVector out(1);
    out = NA_INTEGER;
    return out;
  }else
  {
    if(all(is_na(x_set)) ){
      IntegerVector out(1);
      out = NA_INTEGER;
      return out;
    }else
    {
      int N = x_set.size();
      IntegerVector out_Temp(N);
      int count = 0;
      for(int j = 0; j < N; ++j){
        if(direction == "less_than"){
          if(x_set[j] < x){
          count = count + 1;
          out_Temp[count - 1] = j;
          }
        }

        if(direction == "less_than_or_equal"){
          if(x_set[j] <= x){
          count = count + 1;
          out_Temp[count - 1] = j;
          }
        }

        if(direction == "greater_than"){
          if(x_set[j] > x){
          count = count + 1;
          out_Temp[count - 1] = j;
          }
        }

        if(direction == "greater_than_or_equal"){
          if(x_set[j] >= x){
          count = count + 1;
          out_Temp[count - 1] = j;
          }
        }

      }
      if(count == 0){
        return(0);
      }else
      {
        IntegerVector out(count);
        for(int i = 0; i < count; ++i){
          out[i] = out_Temp[i];
        }
        return out;
      } 
    }
  }
}

// [[Rcpp::export]]
IntegerVector Which_integer_C(int x, IntegerVector x_set){
  int N = x_set.size();
  IntegerVector out_Temp(N);
  int count = 0;
  for(int j = 0; j < N; ++j){
    if(x == x_set[j]){
      count = count + 1;
      out_Temp[count - 1] = j;
    }
  }
  if(count == 0){
    return(0);
  }else
  {
    IntegerVector out(count);
    for(int i = 0; i < count; ++i){
      out[i] = out_Temp[i];
    }
    return out;
  }  
}


// [[Rcpp::export]]
IntegerVector Which_integer_C_NA(int x, IntegerVector x_set){
  if(IntegerVector::is_na(x)){
    IntegerVector out(1);
    out = NA_INTEGER;
    return out;
  }else
  {
    if(all(is_na(x_set)) ){
      IntegerVector out(1);
      out = NA_INTEGER;
      return out;
    }else
    {
      int N = x_set.size();
      IntegerVector out_Temp(N);
      int count = 0;
      for(int j = 0; j < N; ++j){
        if(x == x_set[j]){
          count = count + 1;
          out_Temp[count - 1] = j;
        }
      }
      if(count == 0){
        return(0);
      }else
      {
        IntegerVector out(count);
        for(int i = 0; i < count; ++i){
          out[i] = out_Temp[i];
        }
        return out;
      } 
    }
  }
}

// [[Rcpp::export]]
IntegerVector create_integer_vector(int start_point, int end_point){
  int n = (end_point - start_point) + 1;
  IntegerVector x(n);
  int count = 0;
  for(int i = 0; i < n; ++i){
    x[i] = start_point + count;
    count = count + 1;
  }
  return x;
}

// [[Rcpp::export]]
int Which_Min_C(NumericVector x){
  int Which_Min;
  int n = x.size();
  if(n > 1){
    if(is_true(all(x == x[0]))){
      Rcout << "All values are identical; selecting index for maximum randomly" << "\n";
      IntegerVector Index = create_integer_vector(0,n-1);
      IntegerVector Index_random = int_randomShuffle(Index,1,false,786);
      Which_Min = Index_random[0];
    }else
    {
      int n = x.size();
      double Min_Value;
      int count = 0;
      for(int i = 0; i < n; ++i){
        if(NumericVector::is_na(x[i])){
          continue;
        }
        else
        {
          if(count == 0){
            Min_Value = x[i];
            count = count + 1;
          }
          if( x[i] <= Min_Value){
            Min_Value = x[i];
            Which_Min = i;
          }
        }
      }
    }
  }else
  {
    Which_Min = 0;
  }
  return Which_Min;
}


// [[Rcpp::export]]
int Which_Min_C_NA(NumericVector x){
  int Which_Min;
  if(all(is_na(x))){
    Which_Min = NA_INTEGER;
  }else
  {
    int n = x.size();
    if(n > 1){
      if(is_true(all(x == x[0]))){
      IntegerVector Index = create_integer_vector(0,n-1);
      IntegerVector Index_random = int_randomShuffle(Index,1,false,786);
      Which_Min = Index_random[0];
      }else
      {
        int n = x.size();
        double Min_Value;
        int count = 0;
        for(int i = 0; i < n; ++i){
          if(NumericVector::is_na(x[i])){
            continue;
          }
          else
          {
            if(count == 0){
              Min_Value = x[i];
              count = count + 1;
            }
            if( x[i] <= Min_Value){
              Min_Value = x[i];
              Which_Min = i;
            }
          }
        }
      }
    }else
    {
      Which_Min = 0;
    }
  }
  return Which_Min;
}

/*
In the function below, if all obervations are identical, say 0, then instead of using random index, we set this as NA.
*/

// [[Rcpp::export]]
int Which_Min_C_NA_NRA(NumericVector x){
  int Which_Min;
  if(all(is_na(x))){
    Which_Min = NA_INTEGER;
  }else
  {
    int n = x.size();
    if(n > 1){
      if(is_true(all(x == x[0]))){
      Which_Min = NA_INTEGER;
      }else
      {
        int n = x.size();
        double Min_Value;
        int count = 0;
        for(int i = 0; i < n; ++i){
          if(NumericVector::is_na(x[i])){
            continue;
          }
          else
          {
            if(count == 0){
              Min_Value = x[i];
              count = count + 1;
            }
            if( x[i] <= Min_Value){
              Min_Value = x[i];
              Which_Min = i;
            }
          }
        }
      }
    }else
    {
      Which_Min = 0;
    }
  }
  return Which_Min;
}

// [[Rcpp::export]]
int Which_Max_C(NumericVector x){
  int Which_Max;
  int n = x.size();
  if(n > 1){
    if(is_true(all(x == x[0]))){
      IntegerVector Index = create_integer_vector(0,n-1);
      IntegerVector Index_random = int_randomShuffle(Index,1,false,786);
      Which_Max = Index_random[0];
    }else
    {
      double Max_Value;
      int count = 0;
      for(int i = 0; i < n; ++i){
        if(NumericVector::is_na(x[i])){
          continue;
        }
        else
        {
          if(count == 0){
            Max_Value = x[i];
            count = count + 1;
          }
          if( x[i] >= Max_Value){
            Max_Value = x[i];
            Which_Max = i;
          }
        }
      }    
    }
  }else
  {
    Which_Max = 0;
  }
  return Which_Max;
}


// [[Rcpp::export]]
int Which_Max_C_NA(NumericVector x){
  int Which_Max;
  if(all(is_na(x))){
    Which_Max = NA_INTEGER;
  }else
  {
    int n = x.size();
    if(n > 1)
    {
      if(is_true(all(x == x[0])))
      {
        IntegerVector Index = create_integer_vector(0,n-1);
        IntegerVector Index_random = int_randomShuffle(Index,1,false,786);
        Which_Max = Index_random[0];
      }else
      {
        double Max_Value;
        int count = 0;
        for(int i = 0; i < n; ++i){
          if(NumericVector::is_na(x[i])){
            continue;
          }
          else
          {
            if(count == 0){
              Max_Value = x[i];
              count = count + 1;
            }
            if( x[i] >= Max_Value){
              Max_Value = x[i];
              Which_Max = i;
            }
          }
        }    
      }
    }else
    {
      Which_Max = 0;
    } 
  }
  return Which_Max;
}

/*
In the function below, if all obervations are identical, say 0, then instead of using random index, we set this as NA.
*/
// [[Rcpp::export]]
int Which_Max_C_NA_NRA(NumericVector x){
  int Which_Max;
  if(all(is_na(x))){
    Which_Max = NA_INTEGER;
  }else
  {
    int n = x.size();
    if(n > 1)
    {
      if(is_true(all(x == x[0])))
      {
        Which_Max = NA_INTEGER;
      }else
      {
        double Max_Value;
        int count = 0;
        for(int i = 0; i < n; ++i){
          if(NumericVector::is_na(x[i])){
            continue;
          }
          else
          {
            if(count == 0){
              Max_Value = x[i];
              count = count + 1;
            }
            if( x[i] >= Max_Value){
              Max_Value = x[i];
              Which_Max = i;
            }
          }
        }    
      }
    }else
    {
      Which_Max = 0;
    } 
  }
  return Which_Max;
}

// [[Rcpp::export]]
List StdVar_C(NumericMatrix MyMat){
  int n = MyMat.nrow();
  int K = MyMat.ncol();
  NumericMatrix NewMyMat(n,K);
  NumericVector x_Mean(K);
  double x_Std_Error_Temp;
  NumericVector x_Std_Error(K);
  for(int k = 0; k < K; ++k){
    NumericVector Column_k(n);
    for(int i = 0; i < n; ++i){
      Column_k[i] = MyMat(i,k);
    }
    x_Mean[k] = Mean_C(Column_k);
    x_Std_Error_Temp = sqrt(Sum_C( pow( Column_k - x_Mean[k],2.0  ) ));
    if(x_Std_Error_Temp == 0){
      x_Std_Error[k] = 1;
    }else
    {
      x_Std_Error[k] = x_Std_Error_Temp;
    }
    for(int i = 0; i < n; ++i){
      NewMyMat(i,k) = (Column_k[i] - x_Mean[k])/x_Std_Error[k];
    }
  }
  return List::create(
    _["Std_Matrix"] = NewMyMat, 
    _["Std_Mean"] = x_Mean, 
    _["Std_Error"] = x_Std_Error
  );
}

/*
# Date: 04/14/2021

This is a new function created from the existing one. This is useful for creating standardized matrix from
the original matrix. In this function, we provide our own values for mean and std_error. Useful in situation
when we wanted to standardized data using values different from mean and std_error; for example, when the
interest is not to standardized respnose, use x_Mean = 0 and x_Std_Error = 1.
*/
// [[Rcpp::export]]
List StdVar_C_NA_Supply_MeanSE(NumericMatrix MyMat,NumericVector x_Mean,NumericVector x_Std_Error){
  int n = MyMat.nrow();
  int K = MyMat.ncol();
  NumericMatrix NewMyMat(n,K);

  for(int k = 0; k < K; ++k){
    NumericVector Column_k(n);
    for(int i = 0; i < n; ++i){
      Column_k[i] = MyMat(i,k);
    }
    for(int i = 0; i < n; ++i){
      NewMyMat(i,k) = (Column_k[i] - x_Mean[k])/x_Std_Error[k];
    }
  }
  return List::create(
    _["Std_Matrix"] = NewMyMat, 
    _["Std_Mean"] = x_Mean, 
    _["Std_Error"] = x_Std_Error
  );
}

// [[Rcpp::export]]
List StdVar_C_NA(NumericMatrix MyMat){
  int n = MyMat.nrow();
  int K = MyMat.ncol();
  NumericMatrix NewMyMat(n,K);
  NumericVector x_Mean(K);
  double x_Std_Error_Temp;
  NumericVector x_Std_Error(K);
  for(int k = 0; k < K; ++k){
    NumericVector Column_k(n);
    for(int i = 0; i < n; ++i){
      Column_k[i] = MyMat(i,k);
    }
    x_Mean[k] = Mean_C_NA(Column_k);
    x_Std_Error_Temp = sqrt(Sum_C_NA( pow( Column_k - x_Mean[k],2.0  ) ));
    if(x_Std_Error_Temp == 0){
      x_Std_Error[k] = 1;
    }else
    {
      x_Std_Error[k] = x_Std_Error_Temp;
    }
    for(int i = 0; i < n; ++i){
      NewMyMat(i,k) = (Column_k[i] - x_Mean[k])/x_Std_Error[k];
    }
  }
  return List::create(
    _["Std_Matrix"] = NewMyMat, 
    _["Std_Mean"] = x_Mean, 
    _["Std_Error"] = x_Std_Error
  );
}


// [[Rcpp::export]]
IntegerVector Match_C(NumericVector x_subset, NumericVector x_set){
  int n = x_subset.size();
  int N = x_set.size();
  IntegerVector out(n);
  for(int ii = 0; ii < n; ++ii){
    out[ii] = NA_INTEGER;
  }
  for(int i = 0; i < n; ++i){
    double x_subset_Temp = x_subset[i];
    for(int j = 0; j < N; ++j){
      if(x_subset_Temp == x_set[j]){
        out[i] = j;
        break;
      }
    }
  }
  return out;
}


// [[Rcpp::export]]
IntegerVector Match_C_NA(NumericVector x_subset, NumericVector x_set){
  int n = x_subset.size();
  int N = x_set.size();
  IntegerVector out(n);
  for(int ii = 0; ii < n; ++ii){
    out[ii] = NA_INTEGER;
  }
  if(all(is_na(x_subset))){
    return out;
  }else
  {
    if(all(is_na(x_set))){
      return out;
    }else
    {
      for(int i = 0; i < n; ++i){
        double x_subset_Temp = x_subset[i];
        if(NumericVector::is_na(x_subset_Temp) ){
          out[i] = NA_INTEGER;
        }else
        {
          for(int j = 0; j < N; ++j){
            double x_set_Temp = x_set[j];
            if(NumericVector::is_na(x_set_Temp)){
              continue;
            }else{
              if(x_subset_Temp == x_set_Temp){
                out[i] = j;
                break;
              }               
            }
          }
        }
      }
      return out;
    }
  }
}


// [[Rcpp::export]]
IntegerVector Approx_Match_C(NumericVector x, NumericVector y){
  int n = x.size();
  IntegerVector out(n);
  for(int i = 0; i < n; ++i){
    out[i] = Which_Min_C( abs(x[i] - y) );
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector Approx_Match_C_NA(NumericVector x, NumericVector y){
  int n = x.size();
  IntegerVector out(n);
  for(int i = 0; i < n; ++i){
    out[i] = Which_Min_C_NA( abs(x[i] - y) );
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix Diag_Matrix_C(NumericVector x){
  int n = x.size();
  NumericMatrix x_Mat(n,n);
  for(int i = 0;i < n;++i){
    for(int j = 0; j < n; ++j){
      if(i == j){
        x_Mat(i,j) = x[i];
      }else
      {
        x_Mat(i,j) = 0; 
      } 
    }
  }
  return x_Mat;
}

// [[Rcpp::export]]
NumericMatrix Matrix_Sum_C(NumericMatrix x, NumericMatrix y){
  int Nr = x.nrow();
  int Nc = x.ncol();
  if(Nr != y.nrow() || Nc != y.ncol() ){
    stop("Dimensions do not match");
  }
  NumericMatrix Matrix_Sum(Nr,Nc);
  for(int i = 0; i < Nr; ++i){
    for(int j = 0; j < Nc; ++j){
      Matrix_Sum(i,j) = x(i,j) + y(i,j); 
    }
  }
  return Matrix_Sum;
}

// [[Rcpp::export]]
NumericMatrix Matrix_Sum_C_NA(NumericMatrix x, NumericMatrix y){
  int Nr = x.nrow();
  int Nc = x.ncol();
  if(Nr != y.nrow() || Nc != y.ncol() ){
    stop("Dimensions do not match");
  }
  NumericMatrix Matrix_Sum(Nr,Nc);
  for(int i = 0; i < Nr; ++i){
    for(int j = 0; j < Nc; ++j){
      double x_Temp = x(i,j);
      double y_Temp = y(i,j);
      if( NumericVector::is_na(x_Temp) ){
        x_Temp = 0;
      }
      if( NumericVector::is_na(y_Temp) ){
        y_Temp = 0;
      }
      Matrix_Sum(i,j) = x_Temp + y_Temp; 
    }
  }
  return Matrix_Sum;
}


// [[Rcpp::export]]
double l2Dist_Vector_C(NumericVector x1, NumericVector x2, List ID){
  int n = ID.size();
  NumericVector x_Mean(n);
  for(int i = 0; i < n;++i){
    IntegerVector ID_n = ID[i];
    int ni = ID_n.size();
    NumericVector x1_Temp(ni);
    NumericVector x2_Temp(ni);
    for(int j = 0; j < ni; ++j){
      x1_Temp[j] = x1[ ID_n[j]  ];
      x2_Temp[j] = x2[ ID_n[j]  ];
    }
    x_Mean[i] = Mean_C(pow(x1_Temp - x2_Temp,2.0));
  }
  return sqrt(Mean_C(x_Mean));
}


// [[Rcpp::export]]
double l2Dist_Vector_C_NA(NumericVector x1, NumericVector x2, List ID){
  double out;
  if( all(is_na(x1)) ){
    out = NA_REAL;
  }else
  {
    if(all(is_na(x2))){
      out = NA_REAL;
    }else
    {
      int n = ID.size();
      NumericVector x_Mean(n);
      for(int i = 0; i < n;++i){
        IntegerVector ID_n = ID[i];
        int ni = ID_n.size();
        NumericVector x1_Temp(ni);
        NumericVector x2_Temp(ni);
        for(int j = 0; j < ni; ++j){
          x1_Temp[j] = x1[ ID_n[j]  ];
          x2_Temp[j] = x2[ ID_n[j]  ];
        }
        x_Mean[i] = Mean_C_NA(pow(x1_Temp - x2_Temp,2.0));
      }
      out = sqrt(Mean_C_NA(x_Mean));
    }
  }
  return out;
}

/*
#---------------------------------------------------------------------------------- 
# Date: 12/20/2020
  
# Add the following 3 function so that vimp from predictBoostMLR and from vimp.BoostMLR
# functions match with each other.
#----------------------------------------------------------------------------------
*/

// [[Rcpp::export]]
void set_seed(unsigned long int seed) {
  Rcpp::Environment base_env("package:base");
  Rcpp::Function set_seed_r = base_env["set.seed"];
  set_seed_r(seed);  
}

unsigned long int add_seed = 1;

// [[Rcpp::export]]
NumericVector randomShuffle(NumericVector x, int size, bool setting_seed, unsigned long int seed_value, bool replace, sugar::probs_t p){
  if(setting_seed) {
    //Rcout << seed_value << "\n";
    set_seed(seed_value);
    return sample(x, size, replace, p);
  }else
  {
    return sample(x, size, replace, p);
  }
}

// [[Rcpp::export]]
IntegerVector int_randomShuffle(IntegerVector x, int size, bool setting_seed, unsigned long int seed_value, bool replace, sugar::probs_t p){
  if(setting_seed){
    set_seed(seed_value);
    //Rcout << seed_value << "\n";
    return sample(x, size, replace, p);
  }else
  {
    return sample(x, size, replace, p);
  }
}

// [[Rcpp::export]]
int single_integer_randomShuffle(IntegerVector x, int size, bool setting_seed, unsigned long int seed_value, bool replace, sugar::probs_t p){
  int out;
  if(setting_seed){
    set_seed(seed_value);
    //Rcout << seed_value << "\n";
    out = sample(x, size, replace, p)[0];
  }else
  {
    out = sample(x, size, replace, p)[0];
  }
  return out;
}

// [[Rcpp::export]]
IntegerVector Reverse_Ordering(IntegerVector a) {
  int n = a.size();
  IntegerVector b(n);
  for(int i = 0;i < n;++i){
    b[i] = n - i;
  }
  return b;
}


// [[Rcpp::export]]
NumericVector RemoveNA(NumericVector x){
  int n = x.size();
  if(any(is_na(x))){
    int n_new = length_C_NA(x);
    NumericVector x_new(n_new);
    int count = 0;
    for(int i = 0; i < n;++i){
      if(NumericVector::is_na(x[i]) ){
        continue; 
      }else
      {
        x_new[count] = x[i];
        count = count + 1;
      }
    }
    return x_new;
  }else
  {
    return x;
  }
}


// [[Rcpp::export]]
IntegerVector RemoveNA_Integer(IntegerVector x){
  int n = x.size();
  if(any(is_na(x))){
    int n_new = length_integer_C_NA(x);
    IntegerVector x_new(n_new);
    int count = 0;
    for(int i = 0; i < n;++i){
      if(IntegerVector::is_na(x[i]) ){
        continue; 
      }else
      {
        x_new[count] = x[i];
        count = count + 1;
      }
    }
    return x_new;
  }else
  {
    return x;
  }
}


// [[Rcpp::export]]
CharacterVector RemoveNA_Character(CharacterVector x){
  int n = x.size();
  if(any(is_na(x))){
    int n_new = length_character_C_NA(x);
    CharacterVector x_new(n_new);
    int count = 0;
    for(int i = 0; i < n;++i){
      if(CharacterVector::is_na(x[i]) ){
        continue; 
      }else
      {
        x_new[count] = x[i];
        count = count + 1;
      }
    }
    return x_new;
  }else
  {
    return x;
  }
}

// [[Rcpp::export]]
double Max_C_NA(NumericVector x){
  NumericVector x_new = RemoveNA(x);
  return max(x_new);
}

// [[Rcpp::export]]
double Max_upper_limit_C_NA(NumericVector x, double x_upper_limit){
  NumericVector x_complete = RemoveNA(x);
  int n = x_complete.size();
  NumericVector x_new;
  for(int i = 0; i < n; ++i){
    if( x_complete[i] < x_upper_limit){
      x_new.push_back(x_complete[i]);
    }
  } 
  return max(x_new);
}


// [[Rcpp::export]]
double Min_C_NA(NumericVector x){
  NumericVector x_new = RemoveNA(x);
  return min(x_new);
}

// [[Rcpp::export]]
NumericVector stl_sort(NumericVector x) {
  NumericVector y = clone(x);
  std::sort(y.begin(), y.end());
  return y;
}

// [[Rcpp::export]]
NumericVector stl_sort_NA(NumericVector x) {
  NumericVector x_new = RemoveNA(x);
  NumericVector y = clone(x_new);
  std::sort(y.begin(), y.end());
  return y;
}


// [[Rcpp::export]]
Rcpp::NumericVector stl_sort_reverse(Rcpp::NumericVector x) {
  Rcpp::NumericVector y = Rcpp::clone(x);
  y.sort(true);
  return y;
}

// [[Rcpp::export]]
Rcpp::NumericVector stl_sort_reverse_NA(Rcpp::NumericVector x) {
  NumericVector x_new = RemoveNA(x);
  Rcpp::NumericVector y = Rcpp::clone(x_new);
  y.sort(true);
  return y;
}

// [[Rcpp::export]]
NumericVector unique_C(NumericVector x) {
 NumericVector out = unique(x);
  return out;
}

// [[Rcpp::export]]
NumericVector unique_C_NA(NumericVector x) {
  NumericVector x_new = RemoveNA(x);
  NumericVector out = unique(x_new);
  return out;
}

// [[Rcpp::export]]
CharacterVector unique_character_C_NA(CharacterVector x){
  CharacterVector x_new = RemoveNA_Character(x);
  CharacterVector out = unique(x_new);
  return out;
} 

// [[Rcpp::export]]
IntegerVector unique_integer_C(IntegerVector x) {
  IntegerVector out = unique(x);
  return out;
}

// [[Rcpp::export]]
IntegerVector unique_integer_C_NA(IntegerVector x) {
  IntegerVector x_new = RemoveNA_Integer(x);
  IntegerVector out = unique(x_new);
  return out;
}

// [[Rcpp::export]]
int cumulative_integer_sum(IntegerVector x){
  int n = x.size();
  int cum_sum = 0;
  for(int i = 0; i < n; ++i){
    cum_sum = cum_sum + x[i];
  }
  return cum_sum;
}

// [[Rcpp::export]]
NumericVector sort_unique_C(NumericVector x) {
 NumericVector out = sort_unique(x);
  return out;
}

// [[Rcpp::export]]
NumericVector sort_unique_C_NA(NumericVector x) {
  NumericVector x_new = RemoveNA(x);
  NumericVector out = sort_unique(x_new);
  return out;
}

// [[Rcpp::export]]
IntegerVector sort_unique_integer_C(IntegerVector x) {
IntegerVector out = sort_unique(x);
  return out;
}

// [[Rcpp::export]]
IntegerVector sort_unique_integer_C_NA(IntegerVector x) {
IntegerVector x_new = RemoveNA_Integer(x);
IntegerVector out = sort_unique(x_new);
  return out;
}

// [[Rcpp::export]]
int pmax_two_integers(int x1,int x2){
  int out;
  IntegerVector create_vector = {x1,x2};
  out = max(create_vector);
  return out;
}

// [[Rcpp::export]]
IntegerVector Which_Max_Matrix(NumericMatrix x) {
  IntegerVector Output(2); 
  int N_R = x.nrow();
  int N_C = x.ncol();
  NumericVector Max_Col(N_C);
  for(int j = 0; j < N_C; ++j){
    NumericVector x_Col(N_R); 
    for(int i = 0; i < N_R; ++i){
      x_Col[i] = x(i,j);
    }
    Max_Col[j] = Which_Max_C(x_Col);
  }
  if(is_true(all(Max_Col == -1) )){
    Output[0] = -1;
    Output[1] = -1;
  }else
  {
    NumericVector x_Col_Max(N_C); 
    for(int jj = 0; jj < N_C; ++jj){
      x_Col_Max[jj] = x( Max_Col[jj] ,jj);
    }
    int Max_Col_update = Which_Max_C(x_Col_Max);
    Output[0] = Max_Col[Max_Col_update];
    Output[1] = Max_Col_update;
  }
  return Output;
}


// [[Rcpp::export]]
IntegerVector Which_Max_Matrix_NA(NumericMatrix x) 
{
  IntegerVector Output(2); 
  int N_R = x.nrow();
  int N_C = x.ncol();
  IntegerVector Max_Col(N_C);
  for(int j = 0; j < N_C; ++j)
  {
    NumericVector x_Col(N_R); 
    for(int i = 0; i < N_R; ++i)
    {
      x_Col[i] = x(i,j);
    }
    Max_Col[j] = Which_Max_C_NA(x_Col);
  }
  if(is_true(all(Max_Col == -1) ))
  {
    Output[0] = -1;
    Output[1] = -1;
  }else
  {
    NumericVector x_Col_Max(N_C); 
    for(int jj = 0; jj < N_C; ++jj)
    {
      if(NumericVector::is_na(Max_Col[jj]))
      {
        x_Col_Max[jj] = NA_REAL;
      }else
      {
        x_Col_Max[jj] = x( Max_Col[jj] ,jj);  
      }
    }
    int Max_Col_update = Which_Max_C_NA(x_Col_Max);
    Output[0] = Max_Col[Max_Col_update];
    Output[1] = Max_Col_update;
  }
  return Output;
}


// [[Rcpp::export]]
IntegerVector Which_Max_Matrix_NA_NRA(NumericMatrix x) 
{
  IntegerVector Output(2); 
  int N_R = x.nrow();
  int N_C = x.ncol();
  IntegerVector Max_Col(N_C);
  for(int j = 0; j < N_C; ++j)
  {
    NumericVector x_Col(N_R); 
    for(int i = 0; i < N_R; ++i)
    {
      x_Col[i] = x(i,j);
    }
    Max_Col[j] = Which_Max_C_NA_NRA(x_Col);
  }
  if(all(is_na(Max_Col)))
  {
    Output[0] = NA_INTEGER;
    Output[1] = NA_INTEGER;
  }else
  {
    NumericVector x_Col_Max(N_C); 
    for(int jj = 0; jj < N_C; ++jj)
    {
      if(NumericVector::is_na(Max_Col[jj]))
      {
        x_Col_Max[jj] = NA_REAL;
      }else
      {
        x_Col_Max[jj] = x( Max_Col[jj] ,jj);  
      }
    }
    int Max_Col_update = Which_Max_C_NA_NRA(x_Col_Max);
    Output[0] = Max_Col[Max_Col_update];
    Output[1] = Max_Col_update;
  }
  return Output;
}


// [[Rcpp::export]]
IntegerVector Which_Min_Matrix_NA(NumericMatrix x) 
{
  IntegerVector Output(2); 
  int N_R = x.nrow();
  int N_C = x.ncol();
  IntegerVector Max_Col(N_C);
  for(int j = 0; j < N_C; ++j)
  {
    NumericVector x_Col(N_R); 
    for(int i = 0; i < N_R; ++i)
    {
      x_Col[i] = x(i,j);
    }
    Max_Col[j] = Which_Min_C_NA_NRA(x_Col);
  }
  if(is_true(all(Max_Col == -1) ))
  {
    Output[0] = -1;
    Output[1] = -1;
  }else
  {
    NumericVector x_Col_Max(N_C); 
    for(int jj = 0; jj < N_C; ++jj)
    {
      if(NumericVector::is_na(Max_Col[jj]))
      {
        x_Col_Max[jj] = NA_REAL;
      }else
      {
        x_Col_Max[jj] = x( Max_Col[jj] ,jj);  
      }
    }
    int Max_Col_update = Which_Min_C_NA_NRA(x_Col_Max);
    Output[0] = Max_Col[Max_Col_update];
    Output[1] = Max_Col_update;
  }
  return Output;
}


// [[Rcpp::export]]
IntegerVector Which_Min_Matrix_NA_NRA(NumericMatrix x) 
{
  IntegerVector Output(2); 
  int N_R = x.nrow();
  int N_C = x.ncol();
  IntegerVector Max_Col(N_C);
  for(int j = 0; j < N_C; ++j)
  {
    NumericVector x_Col(N_R); 
    for(int i = 0; i < N_R; ++i)
    {
      x_Col[i] = x(i,j);
    }
    Max_Col[j] = Which_Min_C_NA_NRA(x_Col);
  }
  if(all(is_na(Max_Col)))
  {
    Output[0] = NA_INTEGER;
    Output[1] = NA_INTEGER;
  }else
  {
    NumericVector x_Col_Max(N_C); 
    for(int jj = 0; jj < N_C; ++jj)
    {
      if(NumericVector::is_na(Max_Col[jj]))
      {
        x_Col_Max[jj] = NA_REAL;
      }else
      {
        x_Col_Max[jj] = x( Max_Col[jj] ,jj);  
      }
    }
    int Max_Col_update = Which_Min_C_NA_NRA(x_Col_Max);
    Output[0] = Max_Col[Max_Col_update];
    Output[1] = Max_Col_update;
  }
  return Output;
}

// [[Rcpp::export]]
NumericVector rowSums_C(NumericMatrix x){
  int N_R = x.nrow();
  int N_C = x.ncol();
  NumericVector output(N_R);
  for(int i = 0; i < N_R; ++i){
    NumericVector Col_i(N_C);
    for(int j = 0; j < N_C; ++j){
      Col_i[j] = x(i,j);
    }
    output[i] = Sum_C(Col_i);
  }
  return output;
}

// [[Rcpp::export]]
NumericVector rowSums_C_NA(NumericMatrix x){
  int N_R = x.nrow();
  int N_C = x.ncol();
  NumericVector output(N_R);
  for(int i = 0; i < N_R; ++i){
    NumericVector Col_i(N_C);
    for(int j = 0; j < N_C; ++j){
      Col_i[j] = x(i,j);
    }
    output[i] = Sum_C_NA(Col_i);
  }
  return output;
}

// [[Rcpp::export]]
LogicalVector isNA(IntegerVector x) {
  int n = x.size();
  LogicalVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = IntegerVector::is_na(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
LogicalVector isNA_Numeric(NumericVector x) {
  int n = x.size();
  LogicalVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = NumericVector::is_na(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
bool any_function(LogicalVector x){
   // Note the use of is_true to return a bool type
   return is_true(any(x == TRUE));
}


// [[Rcpp::export]]
bool all_function(LogicalVector x){
  // Note the use of is_true to return a bool type
   return is_true(all(x == TRUE));
}

IntegerVector Which_missing_Numeric(NumericVector x){
  int n = x.size();
  IntegerVector out;
  if((any_function(isNA_Numeric(x)))){
    for(int i = 0; i < n; ++i){
    if((NumericVector::is_na(x[i]))){
      out.push_back(i);
    }
   }
  } else
  {
    out = NA_INTEGER;
  }  
  return out;
}


IntegerVector Which_missing_NumericDF(DataFrame x)
{
  int nC_DF_x = x.size();
  IntegerVector out;
  for(int i = 0; i < nC_DF_x; ++i){
    NumericVector xx = x[i];
    IntegerVector out_temp = Which_missing_Numeric(xx);
    int n_out_temp = out_temp.size();
    if(n_out_temp > 0){
    for(int j = 0; j < n_out_temp; ++j){
      out.push_back(out_temp[j]);
    }
    }
  }
 return out;
}

// [[Rcpp::export]]
double Rho_Inv_C(double Rho_Value,double N_Value){
  double eps = 0.01;
  double N_Rho = (N_Value - 1.0);
  double out = 0.0;
  if(N_Rho == 0){
    out = 0.0;
  }else
  {
    double rho_inv = 0.0;
    rho_inv = Rho_Value + (1/N_Rho);
    if(Rho_Value < 0 && std::abs(rho_inv) < eps){
      out = ((-1/N_Rho) + eps)/(N_Rho*eps);
    }else
    {
      out = Rho_Value/(1 + (N_Rho*Rho_Value));
    }
  }
  return out;
}

// [[Rcpp::export]]
arma::mat MatrixInversion_Equicorrelation_C(int N_Value, double phi, double rho){
  arma::mat Output;
  arma::mat Ii(N_Value,N_Value);
  Ii.fill(0);
  for(int j = 0; j < N_Value; ++j){
    Ii(j,j) = 1;
  }
  arma::mat Ji(N_Value,N_Value);
  Ji.fill(1);
  double N_Value_double = 1.0*N_Value;
  
  double rho_inv = Rho_Inv_C(rho,N_Value_double);
  arma::mat rho_inv_matrix(N_Value,N_Value);
  rho_inv_matrix.fill(rho_inv);
  
  double phi_rho_inv = 1/((1-rho)*phi);
  arma::mat phi_rho_inv_matrix(N_Value,N_Value);
  phi_rho_inv_matrix.fill(phi_rho_inv);
  
  Output = phi_rho_inv_matrix%((Ii - (rho_inv_matrix%Ji) ));
  return Output;
}

// [[Rcpp::export]]
NumericVector Matrix_Vector_Multiplication_C(arma::mat y,NumericVector x){
  int n = x.size();
  int y_row = y.n_rows;
  int y_col = y.n_cols;
  
  //if(y_row != y_col){
  //  stop("Matrix must be a symmetric square");
  //}

  if(n != y_col){
    stop("Number of columns differ from a length of the vector");
  }
  NumericVector Output(y_row);
  for(int i = 0; i < y_row; ++i){
    NumericVector Output_Temp(n);
    for(int j = 0; j < n; ++j){
      Output_Temp[j] = x[j]*y(i,j);
    }
    Output[i] = Sum_C_NA(Output_Temp);
  }
  return Output;
}

// [[Rcpp::export]]
NumericVector Matrix_Vector_Multiplication_C_naObs(arma::mat y,NumericVector x){
  int n = x.size();
  int y_row = y.n_rows;
  int y_col = y.n_cols;
  
  //if(y_row != y_col){
  //  stop("Matrix must be a symmetric square");
  //}

  if(n != y_col){
    stop("Number of columns differ from a length of the vector");
  }
  NumericVector Output(y_row);
  for(int i = 0; i < y_row; ++i){
    if(NumericVector::is_na(x[i]))
    {
      // Date Sep 18 2023
      // this step is not correct because index for x is from 1 : n and not y_row. Fix this later.
      Output[i] = NA_REAL;
    }else
    {
      NumericVector Output_Temp(n);
      for(int j = 0; j < n; ++j){
        Output_Temp[j] = x[j]*y(i,j);
      }
      Output[i] = Sum_C_NA(Output_Temp);
    }
  }
  return Output;
}


// [[Rcpp::export]]
IntegerVector interaction_two_integer_sets(IntegerVector x, IntegerVector y){
  int n_x = x.size();
  IntegerVector out;
  for(int ii = 0; ii < n_x; ++ii){
    if(any_function(x[ii] == y)){
      out.push_back(x[ii]);
    }
  }
  int n_out = out.size();
  if(n_out == 0){
    out = NA_INTEGER;
  }
  return out;
}

// [[Rcpp::export]]
arma::mat Matrix_Multiplication_Arma_C(arma::mat x,arma::mat y){
  int x_row = x.n_rows;
  int x_col = x.n_cols;
  int y_row = y.n_rows;
  int y_col = y.n_cols;
  if(x_col != y_row){
    stop("Dimensions of matrices are not corresponding for multiplication");
  }
  mat output(x_row, y_col,fill::zeros);

  if(all_function(arma::is_finite(x)) && all_function(arma::is_finite(y))){
    output = x*y;
  } else
  {
    for(int i = 0; i < x_row; ++i){
      for(int j = 0; j < y_col; ++j){
        output(i,j) = Sum_C_NA(as<NumericVector>(wrap(x.row(i) % trans(y.col(j)) ))); 
      }
    }
  }
  return output;
}


// [[Rcpp::export]]
arma::mat Matrix_Sum_Arma_C_NA(arma::mat x,arma::mat y){
  int x_row = x.n_rows;
  int x_col = x.n_cols;
  int y_row = y.n_rows;
  int y_col = y.n_cols;

  if(x_row != y_row || x_col != y_col ){
    stop("Dimensions do not match");
  }

  mat output(x_row, x_col,fill::zeros);

  if(all_function(arma::is_finite(x)) && all_function(arma::is_finite(y))){
    output = x + y;
  } else
  {
  for(int i = 0; i < x_row; ++i){
    for(int j = 0; j < x_col; ++j){
      double x_Temp = x(i,j);
      double y_Temp = y(i,j);
      if( NumericVector::is_na(x_Temp) ){
        x_Temp = 0;
      }
      if( NumericVector::is_na(y_Temp) ){
        y_Temp = 0;
      }
      output(i,j) = x_Temp + y_Temp; 
    }
  }
  }
  return output;
}

// [[Rcpp::export]]
bool all_equal(IntegerVector x, IntegerVector y){
   LogicalVector x_equal_y = (x == y);
   return is_true(all(x_equal_y == TRUE));
}

// [[Rcpp::export]]
String convert_double_string(double x) {
  String output;
  output = std::to_string(x);
  return output;
}

// [[Rcpp::export]]
NumericVector round_NumVector(NumericVector x, int d) {
  NumericVector output;
  output = round(x,d);
  return output;
}

// [[Rcpp::export]]
double round_double(double x, int d) {
  double output;
  NumericVector xx(1);
  NumericVector output_vec(1);
  xx[0] = x;
  output_vec = round(xx,d);
  output = output_vec[0];
  return output;
}

// [[Rcpp::export]]
NumericMatrix Convert_DF_into_NumMat(DataFrame x){
  int nR = x.nrows();
  int nC = x.size();
  NumericMatrix output(nR,nC);
    for(int i = 0; i < nC; ++i){
      output(_,i) = NumericVector(x[i]);
    }
  return output;  
}

// [[Rcpp::export]]
arma::mat DiagonalMatrix_C(int Dimension, double x){
  mat Output(Dimension,Dimension,fill::zeros);
  for(int i = 0; i < Dimension; ++i){
    Output(i,i) = x;
  }
  return Output;
}

// [[Rcpp::export]]
NumericMatrix MatrixInversion_R(NumericMatrix x){
  Function f("qr.solve");
  NumericMatrix Output = f(Named("a") = x); 
  return Output;
}

// [[Rcpp::export]]
arma::mat MatrixInversion_C(arma::mat x){
  int nC = x.n_cols;
  int nR = x.n_rows;
  mat Output(nR,nC);
  Output = inv_sympd(x);
  return Output;
}

// [[Rcpp::export]]
DataFrame Subsetting_Dataframe(DataFrame x, IntegerVector Index){
  int nC = x.size();
  //int nR = x.nrows();
  int n_index = Index.size();
  DataFrame output;
  for(int i = 0; i < nC; ++i){
    NumericVector x_column = x[i];
    NumericVector x_temp;
    for(int j = 0; j < n_index; ++j){
      x_temp.push_back(x_column[ Index[j] ]);
    }
    output.push_back(x_temp);
  }
return output;
}

// [[Rcpp::export]]
IntegerVector create_index_logical_vector(LogicalVector x){
  int n_x = x.size();
  IntegerVector out;
  for(int i = 0; i < n_x; ++i){
    if(x[i] == true){
      out.push_back(i);
    }
  }
  return out;
}


// [[Rcpp::export]]
NumericVector create_variable_by_substitution(NumericVector x, NumericVector x_sub){
  // In this function, if the values of x are replaced by values from x_sub when x_sub are equal to or slightly smaller than x_sub
  int n_x = x.size();
  int n_x_sub = x_sub.size();
  NumericVector out(n_x,NA_REAL);
  LogicalVector logic_out(n_x);
  for(int i = 0; i < n_x_sub; ++i){
     if(i == 0){
       logic_out = (x == x_sub[i]);
     }else
     {
       logic_out = (x > x_sub[i-1] & x <= x_sub[i]);
     }
      
     IntegerVector index_logical_vector = create_index_logical_vector(logic_out);
     int n_index_logical_vector = index_logical_vector.size();
     for(int j = 0; j < n_index_logical_vector; ++j){
       out[ index_logical_vector[j]  ] = x_sub[i];
     } 
  }
return out;
}

// [[Rcpp::export]]
NumericVector create_sequence(double min_value, double max_value, double by_amount){
   double out_temp;
   double count = 0.0;
   NumericVector out;
   out.push_back(min_value);
   while(any_function( Max_C_NA(out) < max_value)){
      count = count + 1.0;
      out_temp = min_value + (count*by_amount);
      out.push_back(out_temp);
   }
  return out;    
}

// [[Rcpp::export]]
arma::mat fit_component_gls(List x, List y, List v, bool add_intercept)
{
  int p;
  if(add_intercept)
  {
    p = 2;
  }else
  {
    p = 1;
  }
  mat output(p,1);
  mat output_zero(p,1,fill::zeros);
  int n = x.size();
  mat xTx(p,p);
  xTx.fill(0);
  mat xTy(p,1);
  xTy.fill(0);


  for(int i = 0; i < n; ++i)
  {
    //vec x_i = x[i];

    NumericVector temp_x_i = x[i];
    int ni = temp_x_i.size();
    vec x_i(ni);

    LogicalVector which_isNA = isNA_Numeric(temp_x_i);
    if( any_function(which_isNA) )
    {
      NumericVector temp_temp_x_i(ni);
      for(int j = 0; j < ni; ++j)
      {
        if(NumericVector::is_na(temp_x_i[j]))
        {
          temp_temp_x_i[j] = 0;
        }else
        {
          temp_temp_x_i[j] = temp_x_i[j];
        }
      }
      x_i = temp_temp_x_i;
    }else
    {
      x_i = temp_x_i;
    }

    NumericVector temp_y_i = y[i];
    vec y_i(ni);

    LogicalVector which_isNA_y = isNA_Numeric(temp_y_i);
    if( any_function(which_isNA_y) )
    {
      NumericVector temp_temp_y_i(ni);
      for(int j = 0; j < ni; ++j)
      {
        if(NumericVector::is_na(temp_y_i[j]))
        {
          temp_temp_y_i[j] = 0;
        }else
        {
          temp_temp_y_i[j] = temp_y_i[j];
        }
      }
      y_i = temp_temp_y_i;
    }else
    {
      y_i = temp_y_i;
    }

    //vec y_i = y[i];
    mat v_i = v[i];

    //int ni = x_i.size();

    mat mat_x_i(ni, p);
    if(add_intercept)
    {
      mat_x_i.col(0).ones(); 
      mat_x_i.col(1) = x_i;
    }else
    {
      mat_x_i.col(0) = x_i;
    }

    mat temp_xTx = trans(mat_x_i)*v_i*mat_x_i;
    mat temp_xTy = trans(mat_x_i)*v_i*y_i;

    xTx = xTx + temp_xTx;
    xTy = xTy + temp_xTy;
    //xTx = Matrix_Sum_Arma_C_NA(xTx , trans(mat_x_i)*v_i*mat_x_i );
    //xTy = Matrix_Sum_Arma_C_NA(xTy , trans(mat_x_i)*v_i*y_i );
  }

  //double diag_value = as<double>(wrap(diagvec( xTx, p-1 ) ));
  double deter_xTx = det(xTx);


  //Rcout << xTx << "\n";

  if( deter_xTx < 0.001 )
  {
    output = output_zero;
  }else
  {
    output = MatrixInversion_C(xTx)*xTy;
  }

  return output;
  
}



// [[Rcpp::export]]
double fit_component_gls_line_search(List x, List y, List v)
{
  double output;

  int n = x.size();
  mat xTx(1,1);
  xTx.fill(0);
  mat xTy(1,1);
  xTy.fill(0);

  for(int i = 0; i < n; ++i)
  {
    //vec x_i = x[i];
    //vec y_i = y[i];

    NumericVector temp_x_i = x[i];
    int ni = temp_x_i.size();
    vec x_i(ni);

    LogicalVector which_isNA = isNA_Numeric(temp_x_i);
    if( any_function(which_isNA) )
    {
      NumericVector temp_temp_x_i(ni);
      for(int j = 0; j < ni; ++j)
      {
        if(NumericVector::is_na(temp_x_i[j]))
        {
          temp_temp_x_i[j] = 0;
        }else
        {
          temp_temp_x_i[j] = temp_x_i[j];
        }
      }
      x_i = temp_temp_x_i;
    }else
    {
      x_i = temp_x_i;
    }

    NumericVector temp_y_i = y[i];
    vec y_i(ni);

    LogicalVector which_isNA_y = isNA_Numeric(temp_y_i);
    if( any_function(which_isNA_y) )
    {
      NumericVector temp_temp_y_i(ni);
      for(int j = 0; j < ni; ++j)
      {
        if(NumericVector::is_na(temp_y_i[j]))
        {
          temp_temp_y_i[j] = 0;
        }else
        {
          temp_temp_y_i[j] = temp_y_i[j];
        }
      }
      y_i = temp_temp_y_i;
    }else
    {
      y_i = temp_y_i;
    }

    mat v_i = v[i];

    xTx = Matrix_Sum_Arma_C_NA(xTx , trans(x_i)*v_i*x_i );
    xTy = Matrix_Sum_Arma_C_NA(xTy , trans(x_i)*v_i*y_i );
  }

  output = as<double>(wrap(MatrixInversion_C(xTx)*xTy));

  return output;
  
}

// [[Rcpp::export]]
double sq_residual(List x, List y, arma::mat beta, bool add_intercept)
{
  double output = 0.0;
  int n = x.size();
  int p;
  if(add_intercept)
  {
    p = 2;
  }else
  {
    p = 1;
  }

  for(int i = 0; i < n; ++i)
  {
    NumericVector temp_x_i = x[i];
    int ni = temp_x_i.size();
    vec x_i(ni);

    LogicalVector which_isNA = isNA_Numeric(temp_x_i);
    if( any_function(which_isNA) )
    {
      NumericVector temp_temp_x_i(ni);
      for(int j = 0; j < ni; ++j)
      {
        if(NumericVector::is_na(temp_x_i[j]))
        {
          temp_temp_x_i[j] = 0;
        }else
        {
          temp_temp_x_i[j] = temp_x_i[j];
        }
      }
      x_i = temp_temp_x_i;
    }else
    {
      x_i = temp_x_i;
    }

    NumericVector temp_y_i = y[i];
    vec y_i(ni);

    LogicalVector which_isNA_y = isNA_Numeric(temp_y_i);
    if( any_function(which_isNA_y) )
    {
      NumericVector temp_temp_y_i(ni);
      for(int j = 0; j < ni; ++j)
      {
        if(NumericVector::is_na(temp_y_i[j]))
        {
          temp_temp_y_i[j] = 0;
        }else
        {
          temp_temp_y_i[j] = temp_y_i[j];
        }
      }
      y_i = temp_temp_y_i;
    }else
    {
      y_i = temp_y_i;
    }

    mat mat_x_i(ni, p);
    if(add_intercept)
    {
      mat_x_i.col(0).ones(); 
      mat_x_i.col(1) = x_i;
    }else
    {
      mat_x_i.col(0) = x_i;
    }

    mat out_temp = trans(y_i - (mat_x_i*beta))*(y_i - (mat_x_i*beta));
    output = output + as<double>(wrap( out_temp )); 
  }

  return output/((double) n);
}


// [[Rcpp::export]]
arma::vec extract_linear_pred(int N, List Index, List x, arma::mat beta, bool add_intercept)
{
  vec output(N,fill::zeros);
  int n = x.size();
  int p;
  if(add_intercept)
  {
    p = 2;
  }else
  {
    p = 1;
  }

/*
Date May 8, 2023

We should not set missing values = 0 because it can create a problem where \mu which is a sum of all p terms of beta and x will be estimated inaccurately because some of the terms are 0, even though the corresponding beta may be non-zero. Therefore, i am commenting out following codes
*/



  for(int i = 0; i < n; ++i)
  {
    vec x_i = x[i];
    int ni = x_i.size();
    
    /*

    NumericVector temp_x = as<NumericVector>(wrap(x_i));
    vec x_mod(ni);
    LogicalVector which_isNA = isNA_Numeric(temp_x);
    if( any_function(which_isNA) )
    {
      NumericVector temp_temp_x(ni);
      for(int j = 0; j < ni; ++j)
      {
        if(NumericVector::is_na(temp_x[j]))
        {
          temp_temp_x[j] = 0;
        }else
        {
          temp_temp_x[j] = temp_x[j];
        }
      }
      x_mod = temp_temp_x;
    }else
    {
      x_mod = temp_x;
    }

    mat mat_x_i(ni, p);
    if(add_intercept)
    {
      mat_x_i.col(0).ones(); 
      mat_x_i.col(1) = x_mod;
    }else
    {
      mat_x_i.col(0) = x_mod;
    }

    */

    mat mat_x_i(ni, p);
    if(add_intercept)
    {
      mat_x_i.col(0).ones(); 
      mat_x_i.col(1) = x_i;
    }else
    {
      mat_x_i.col(0) = x_i;
    }

    vec x_beta =  mat_x_i*beta;
 
    IntegerVector temp_index = Index[i];

    for(int j = 0; j < ni; ++j)
    {
      output( temp_index[j] ) = x_beta[j];
    }

  }

  return output;
}

// [[Rcpp::export]]
NumericVector extract_base_learner(NumericVector x, arma::mat beta, bool add_intercept, int No_coef)
{
  int n = x.size();
  NumericVector output(n);

  mat mat_x_i(n, No_coef);
  vec vec_x(n);


  NumericVector temp_x = x;

  LogicalVector which_isNA = isNA_Numeric(temp_x);
  if( any_function(which_isNA) )
  {
    NumericVector temp_temp_x(n);
    for(int j = 0; j < n; ++j)
    {
      if(NumericVector::is_na(temp_x[j]))
      {
        temp_temp_x[j] = 0;
      }else
      {
        temp_temp_x[j] = temp_x[j];
      }
    }
    vec_x = temp_temp_x;
  }else
  {
    vec_x = temp_x;
  }

  if(add_intercept)
  {
    mat_x_i.col(0).ones(); 
    mat_x_i.col(1) = vec_x;
  }else
  {
    mat_x_i.col(0) = vec_x;
  }

  output = as<NumericVector>(wrap( mat_x_i*beta  ));

  return output;
}

// [[Rcpp::export]]
arma::vec extract_mu(arma::vec x, String link)
{
  vec output(x.size() ); 
  if(link == "continuous")
  {
    output = x;
  }else
  {
    if(link == "binary")
    {
      output = exp(x)/(1 + exp(x));
    }
  }

  return output;
}



// [[Rcpp::export]]
List DataProcessing_C(NumericMatrix Org_x,
                      NumericMatrix Org_y,
                      NumericVector id, 
                      NumericVector tm,
                      NumericVector id_x,
                      NumericVector tm_x,
                      NumericVector unq_id,
                      bool x_miss,
                      bool trace,
                      bool y_summary_supply,
                      NumericVector y_mean_supply,
                      NumericVector y_std_error_supply)
{
  if(trace)
  {
    Rcout << "Currently at data processing step" << "\n";
  }
  
  int n = unq_id.size();
  
  List id_index(n);
  for(int i = 0; i < n; ++i)
  {
    id_index[i] = Which_C_NA(unq_id[i],id);
  }

  List id_index_x(n);
  for(int i = 0; i < n; ++i)
  {
    id_index_x[i] = Which_C_NA(unq_id[i],id_x);
  }
  
  IntegerVector ni(n);
  for(int i = 0; i < n; ++i)
  {
    IntegerVector id_index_Temp = id_index[i];
    ni[i] = id_index_Temp.size();
  }
  
  IntegerVector ni_x(n);
  for(int i = 0; i < n; ++i)
  {
    IntegerVector id_index_Temp = id_index_x[i];
    ni_x[i] = id_index_Temp.size();
  }

  int N = Org_y.nrow();
  int N_x = Org_x.nrow();
  int L = Org_y.ncol();
  int K = Org_x.ncol();

  int count;
  count = 0;
  IntegerVector unlist_id_index(N);
  for(int i = 0; i < n; ++i)
  {
    NumericVector id_index_Temp = id_index[i];
    for(int j = 0; j < ni[i]; ++j)
    {
      unlist_id_index[count] = id_index_Temp[j];
      count = count + 1;
    }
  }

  count = 0;
  IntegerVector unlist_id_index_x(N_x);
  for(int i = 0; i < n; ++i)
  {
    NumericVector id_index_Temp = id_index_x[i];
    for(int j = 0; j < ni_x[i]; ++j)
    {
      unlist_id_index_x[count] = id_index_Temp[j];
      count = count + 1;
    }
  }

  NumericVector id_New(N);
  for(int ii = 0; ii < N; ++ii)
  {
    id_New[ii] = id[ unlist_id_index[ii] ];
  }
  
  NumericVector id_New_x(N_x);
  for(int ii = 0; ii < N_x; ++ii)
  {
    id_New_x[ii] = id_x[ unlist_id_index_x[ii] ];
  }



  LogicalVector id_Match(N);
  for(int ii = 0; ii < N; ++ii)
  {
    id_Match[ii] = (id[ii] == id_New[ii]);
  }

  LogicalVector id_Match_x(N_x);
  for(int ii = 0; ii < N_x; ++ii)
  {
    id_Match_x[ii] = (id_x[ii] == id_New_x[ii]);
  }
 
  if(is_false(all( id_Match == TRUE) ))
  {
    NumericVector tm_New(N);
    NumericMatrix Org_y_New(N,L);
    for(int ii = 0; ii < N; ++ii)
    {
      tm_New[ii] = tm[ unlist_id_index[ii]  ];
      id_New[ii] = id[ unlist_id_index[ii]  ];
      for(int l = 0; l < L; ++l)
      {
        Org_y_New(ii,l) = Org_y( unlist_id_index[ii], l  );
      }
    }
    tm = tm_New;
    id = id_New;
    Org_y = Org_y_New;  
  }


  if(is_false(all( id_Match_x == TRUE) ))
  {
    NumericVector tm_New_x(N_x);
    NumericMatrix Org_x_New(N_x,K);
    for(int ii = 0; ii < N_x; ++ii)
    {
      tm_New_x[ii] = tm_x[ unlist_id_index_x[ii]  ];
      id_New_x[ii] = id_x[ unlist_id_index_x[ii]  ];
      for(int k = 0; k < K; ++k)
      {
        Org_x_New(ii,k) = Org_x( unlist_id_index_x[ii], k  );
      }
    }
    tm_x = tm_New_x;
    id_x = id_New_x;
    Org_x = Org_x_New; 
  }


  NumericMatrix x(N_x,K);
  NumericVector x_Mean(K);
  NumericVector x_Std_Error(K);
  if(x_miss)
  {
    x = Org_x;
    x_Mean = 0.0;
    x_Std_Error = 1.0;
  } else
  {
    List Std_x = StdVar_C_NA(Org_x);
    x = as<NumericMatrix>(Std_x["Std_Matrix"]);
    x_Mean = as<NumericVector>(Std_x["Std_Mean"]);
    x_Std_Error = as<NumericVector>(Std_x["Std_Error"]);
  }
  

  /*
  # Date: 04/14/2021

  Here, we are using our own mean and std_error rather than letting the function calculate these values.
  In this way, we can choose not to standardized response; this can be done by using y_mean_supply = 0 and y_std_error_supply = 1. 
  */


 NumericMatrix y(N,L);
 NumericVector y_Mean(L);
 NumericVector y_Std_Error(L);

  if(y_summary_supply)
  {
    List Std_y = StdVar_C_NA_Supply_MeanSE(Org_y,y_mean_supply,y_std_error_supply);
    y = as<NumericMatrix>(Std_y["Std_Matrix"]);
    y_Mean = as<NumericVector>(Std_y["Std_Mean"]);
    y_Std_Error = as<NumericVector>(Std_y["Std_Error"]);
  }else
  {
    List Std_y = StdVar_C_NA(Org_y);
    y = as<NumericMatrix>(Std_y["Std_Matrix"]);
    y_Mean = as<NumericVector>(Std_y["Std_Mean"]);
    y_Std_Error = as<NumericVector>(Std_y["Std_Error"]);
  }
  

  List Data = List::create(
    _["Org_x"] = Org_x, 
    _["Org_y"] = Org_y,
    _["id"] = id,
    _["tm"] = tm,
    _["x"] = x, 
    _["y"] = y,
    _["id_x"] = id_x,
    _["tm_x"] = tm_x,
    _["x_Mean"] = x_Mean,
    _["x_Std_Error"] = x_Std_Error,
    _["y_Mean"] = y_Mean,
    _["y_Std_Error"] = y_Std_Error
  );
  List Dimensions = List::create(
    _["n"] = n,
    _["K"] = K,
    _["L"] = L,
    _["ni"] = ni,
    _["ni_x"] = ni_x,
    _["N"] = N,
    _["N_x"] = N_x
  );
  
  List Index = List::create(
    _["unq_id"] = unq_id,
    _["id_index"] = id_index,
    _["id_index_x"] = id_index_x
  );
  
  if(trace){
    Rcout << "Completed data processing step" << "\n";
  }
  
  return List::create(
    _["Data"] = Data,
    _["Dimensions"] = Dimensions,
    _["Index"] = Index
  );

}
  
// [[Rcpp::export]]
List BoostMLR_C(NumericMatrix Org_x,
                NumericMatrix Org_y,
                NumericVector id,
                NumericVector tm,
                NumericMatrix x,
                NumericMatrix y,
                NumericVector id_x,
                NumericVector tm_x,
                NumericVector x_Mean,
                NumericVector x_Std_Error,
                NumericVector y_Mean,
                NumericVector y_Std_Error,
                String y_scale_summary,
                bool intercept,
                List time_map,
                int n,
                int K,
                int L,
                int H,
                int G,
                IntegerVector Dk,
                IntegerVector ni,
                IntegerVector ni_x,
                int N,
                int N_x,
                NumericVector unq_id,
                NumericVector unq_tm,
                NumericVector unq_tm_x,
                List unq_x,
                List id_index,
                List id_index_x,
                NumericMatrix Bt,
                NumericMatrix Bt_x,
                List Bx,
                List Bx_Scale,
                double Bx_Scale_const,
                NumericMatrix Time_Add_New,
                LogicalVector Time_Unmatch,
                double nu,
                int M,
                bool Mod_Grad,
                LogicalVector UseRaw,
                NumericVector Lambda_Ridge_Vec,
                bool Ridge_Penalty,
                bool Shrink,
                double lower_perc,
                double upper_perc,
                double Lambda_Scale,
                int NLambda,
                bool VarFlag,
                NumericVector rho,
                NumericVector phi,
                bool setting_seed,
                unsigned long int seed_value,
                bool sq_residual_select,
                bool update_all_comp,
                List comp_x,
                List comp_y,
                bool Verbose,
                bool Trace)
  
{
  if(Trace)
  {
    Rcout << "At the start of BoostMLR function execution" << "\n";
  }

  List tm_index(n);
  for(int i = 0; i < n; ++i)
  {
    NumericVector tm_Temp(ni[i]);
    IntegerVector id_index_Temp = id_index[i];
    for(int j = 0; j < ni[i]; ++j)
    {
      tm_Temp[j] = tm[id_index_Temp[j]];
    }
    tm_index[i] = Match_C_NA(tm_Temp,unq_tm);
  }

  if(Trace)
  {
    Rcout << "List tm_index" << "\n";
  }

  List tm_index_x(n);
  for(int i = 0; i < n; ++i)
  {
    NumericVector tm_Temp(ni_x[i]);
    IntegerVector id_index_Temp = id_index_x[i];
    for(int j = 0; j < ni_x[i]; ++j)
    {
      tm_Temp[j] = tm_x[id_index_Temp[j]];
    }
    tm_index_x[i] = Match_C_NA(tm_Temp,unq_tm_x);
  }

  if(Trace)
  {
    Rcout << "List tm_index_x" << "\n";
  }

  List Bt_G(G);
  for(int g = 0; g < G; ++g)
  {
    List Bt_n(n);
    for(int i = 0; i < n; ++i)
    {
      IntegerVector tm_index_Temp = tm_index[i];
      NumericVector Bt_ni(ni[i]);
      for(int j = 0; j < ni[i]; ++j)
      {
        if(IntegerVector::is_na( tm_index_Temp[j]))
        {
          Bt_ni[j] = NA_REAL;
        }else
        {
          Bt_ni[j] = Bt(tm_index_Temp[j], g);  
        }
      }
      Bt_n[i] = Bt_ni;
    }
    Bt_G[g] = Bt_n;
  }

  if(Trace)
  {
    Rcout << "List Bt_G" << "\n";
  }

  List Bt_H(H);
  for(int h = 0; h < H; ++h)
  {
    List Bt_n(n);
    for(int i = 0; i < n; ++i)
    {
      IntegerVector tm_index_Temp = tm_index_x[i];
      NumericVector Bt_ni(ni_x[i]);
      for(int j = 0; j < ni_x[i]; ++j)
      {
        if(IntegerVector::is_na( tm_index_Temp[j]))
        {
          Bt_ni[j] = NA_REAL;
        }else
        {
          Bt_ni[j] = Bt_x(tm_index_Temp[j], h);  
        }
      }
      Bt_n[i] = Bt_ni;
    }
    Bt_H[h] = Bt_n;
  }

  if(Trace)
  {
    Rcout << "List Bt_H" << "\n";
  }

  List x_index(K);
  for(int k = 0; k < K; ++k)
  {
    if(UseRaw[k])
    {
      x_index[k] = NA_INTEGER;
    }else
    {
      NumericVector unq_x_Temp = unq_x[k];
      List x_index_Subject(n);
      for(int i = 0; i < n; ++i)
      {
        NumericVector x_Temp(ni_x[i]);
        IntegerVector id_index_Temp = id_index_x[i];
        for(int j = 0; j < ni_x[i]; ++j)
        {
          x_Temp[j] = x(id_index_Temp[j] , k);
        }
        x_index_Subject[i] = Match_C_NA(x_Temp,unq_x_Temp);
      }
      x_index[k] = x_index_Subject;
    }
  }

  if(Trace)
  {
    Rcout << "List x_index" << "\n";
  }
  
  int count = -1;
  List Time_Add_index(K);
  for(int k = 0; k < K; ++k)
  {
    if(Time_Unmatch[k])
    {
      count = count + 1;
      List Time_Add_index_Subject(n);
      for(int i = 0; i < n; ++i)
      {
        NumericVector Time_Add_Temp(ni_x[i]);
        IntegerVector id_index_Temp = id_index_x[i];
        for(int j = 0; j < ni_x[i]; ++j)
        {
          Time_Add_Temp[j] = Time_Add_New(id_index_Temp[j],count); 
        }
        Time_Add_index_Subject[i] = Approx_Match_C_NA(Time_Add_Temp,unq_tm_x);
      }
      Time_Add_index[k] = Time_Add_index_Subject;
    }else 
    {
      Time_Add_index[k] = NA_INTEGER;
    }
  }

  if(Trace)
  {
    Rcout << "List Time_Add_index" << "\n";
  }
  
  IntegerVector DkT(K);
  for(int k = 0; k < K; ++k)
  {
    if(UseRaw[k])
    {
      DkT[k] = Dk[k];
    }else
    {
      if(Time_Unmatch[k])
      {
        DkT[k] = Dk[k]*H;
      }else
      {
        DkT[k] = Dk[k];  
      }
    }
  }
  
  List Bx_K(K);
  for(int k = 0; k < K; ++k)
  {
    NumericMatrix Bx_K_Temp = Bx[k];
    List Bx_Dk(DkT[k]);
    if(UseRaw[k])
    {
      for(int d = 0; d < DkT[k]; ++d)
      {
        List Bx_n(n);
        for(int i = 0; i < n; ++i)
        {
          IntegerVector id_index_Temp = id_index_x[i];
          NumericVector Bx_ni(ni_x[i]);
          for(int j = 0; j < ni_x[i]; ++j)
          {
            Bx_ni[j] = Bx_K_Temp( id_index_Temp[j],d) ;
          }
          Bx_n[i] = Bx_ni;
        }
        Bx_Dk[d] = Bx_n;
      }
    }else
    {
      count = -1;
      if(Time_Unmatch[k])
      {
        List x_index_Temp = x_index[k];
        List Time_Add_index_Temp = Time_Add_index[k];
          for(int d = 0; d < Dk[k]; ++d)
          {
            for(int h = 0; h < H; ++h)
            {
              count = count + 1;
              List Bx_n(n);
              for(int i = 0; i < n; ++i)
              {
                IntegerVector x_index_Temp_n = x_index_Temp[i];
                IntegerVector Time_Add_index_Temp_n = Time_Add_index_Temp[i];
                NumericVector Bx_ni(ni_x[i]);
                for(int j = 0; j < ni_x[i]; ++j)
                {
                  if(IntegerVector::is_na( x_index_Temp_n[j]))
                  {
                    Bx_ni[j] = NA_REAL;
                  }else
                  {
                    if(IntegerVector::is_na( Time_Add_index_Temp_n[j]))
                    {
                      Bx_ni[j] = NA_REAL;
                    }else
                    {
                      Bx_ni[j] = Bx_K_Temp(x_index_Temp_n[j],d)*Bt(Time_Add_index_Temp_n[j],h);
                    }
                  }
                }
                Bx_n[i] = Bx_ni;
              }
              Bx_Dk[count] = Bx_n;
            }
          } 
      }else 
      {
        List x_index_Temp = x_index[k];
        for(int d = 0; d < DkT[k]; ++d)
        {
          List Bx_n(n);
          for(int i = 0; i < n; ++i)
          {
            IntegerVector x_index_Temp_n = x_index_Temp[i];
            NumericVector Bx_ni(ni_x[i]);
            for(int j = 0; j < ni_x[i]; ++j)
            {
              if(IntegerVector::is_na( x_index_Temp_n[j]))
              {
                Bx_ni[j] = NA_REAL;
              }else
              {
                Bx_ni[j] = Bx_K_Temp(x_index_Temp_n[j],d);
              }
            }
            Bx_n[i] = Bx_ni;
          }
          Bx_Dk[d] = Bx_n;
        }
      }
    }
    Bx_K[k] = Bx_Dk;
  }

  if(Trace)
  {
    Rcout << "List Bx_K" << "\n";
  }
  
  Dk = DkT;
  
  List Bxt(K);
  for(int k = 0; k < K; ++k)
  {
    List Bx_Temp_K = Bx_K[k];
    List Bxt_Dk(Dk[k]);
    for(int d = 0; d < Dk[k]; ++d)
    {
      List Bx_Temp_Dk = Bx_Temp_K[d];
      List Bxt_H(H);
      for(int h = 0; h < H; ++h)
      {
        List Bt_Temp_H = Bt_H[h];
        List Bxt_n(n);
        for(int i = 0; i < n; ++i)
        {
          NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
          NumericVector Bt_Temp_n = Bt_Temp_H[i];
          NumericVector Bxt_ni(ni_x[i]);
          for(int j = 0; j < ni_x[i]; ++j)
          {
            Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];
          }
          Bxt_n[i] = Bxt_ni;
        }
        Bxt_H[h] = Bxt_n;
      }
      Bxt_Dk[d] = Bxt_H;
    }
    Bxt[k] = Bxt_Dk;
  }

  if(Trace)
  {
    Rcout << "List Bxt" << "\n";
  }

  List Bxt_time_map(K);
  for(int k = 0; k < K; ++k)
  {
    List temp_Bxt_K = Bxt[k];
    List Bxt_time_map_Dk(Dk[k]);
    for(int d = 0; d < Dk[k]; ++d)
    {
      List temp_Bxt_Dk = temp_Bxt_K[d];
      List Bxt_time_map_H(H);
      for(int h = 0; h < H; ++h)
      {
        List temp_Bxt_H = temp_Bxt_Dk[h];
        List Bxt_time_map_n(n);
        for(int i = 0; i < n; ++i)
        {
          NumericVector temp_Bxt_n = temp_Bxt_H[i];
          mat temp_time_map = time_map[i];
          NumericVector temp_Bxt_time_map_n = Matrix_Vector_Multiplication_C(temp_time_map,temp_Bxt_n );
           Bxt_time_map_n[i] = temp_Bxt_time_map_n;
        }
        Bxt_time_map_H[h] = Bxt_time_map_n;
      }
      Bxt_time_map_Dk[d] = Bxt_time_map_H;
    }
    Bxt_time_map[k] = Bxt_time_map_Dk;
  }

  if(Trace)
  {
    Rcout << "List Bxt_time_map" << "\n";
  }


  List Byt_xt_time_map(K);
  for(int k = 0; k < K; ++k)
  {
    List temp_Bxt_time_map_K = Bxt_time_map[k];
    List Byt_xt_time_map_Dk(Dk[k]);
    for(int d = 0; d < Dk[k]; ++d)
    {
      List temp_Bxt_time_map_Dk = temp_Bxt_time_map_K[d];
      List Byt_xt_time_map_H(H);
      for(int h = 0; h < H; ++h)
      {
        List temp_Bxt_time_map_H = temp_Bxt_time_map_Dk[h];
        List Byt_xt_time_map_G(G);
        for(int g = 0; g < G; ++g)
        {
          List temp_Bt_G = Bt_G[g];
          List Byt_xt_time_map_n(n);
          for(int i = 0; i < n; ++i)
          {
            NumericVector temp_Bt_n = temp_Bt_G[i];
            NumericVector temp_Bxt_time_map_n = temp_Bxt_time_map_H[i];
            NumericVector Byt_xt_time_map_ni(ni[i]);
            for(int j = 0; j < ni[i]; ++j)
            {
              Byt_xt_time_map_ni[j] = temp_Bt_n[j]*temp_Bxt_time_map_n[j];
            }
            Byt_xt_time_map_n[i] = Byt_xt_time_map_ni;
          }
          Byt_xt_time_map_G[g] = Byt_xt_time_map_n;
        }
        Byt_xt_time_map_H[h] = Byt_xt_time_map_G;
      }
      Byt_xt_time_map_Dk[d] = Byt_xt_time_map_H;
    }
    Byt_xt_time_map[k] = Byt_xt_time_map_Dk;
  }

  if(Trace)
  {
    Rcout << "List Byt_xt_time_map" << "\n";
  }
  
  if(Trace)
  {
    Rcout << "Completed setting-up of B-spline matrices" << "\n";
  }

/*
Date: 02/07/2023

In this version, I am handling binary responses as well, in addition to continuous responses, it makes sense that I use mean of y_{l} for mu_zero. That way even if the response is continuous, and we are going to use standardization of the response, then mean of y_{l} = 0, which works well.
*/

  NumericVector y_Mean_mu_zero(L);
  for(int l = 0; l < L; ++l)
  {
    y_Mean_mu_zero[l] = Mean_C_NA(y(_,l));
  } 


  NumericMatrix mu_zero(N,L);
  for(int l = 0; l < L; ++l)
  {
    for(int i = 0; i < N; ++i)
    {
      mu_zero(i,l) = y_Mean_mu_zero[l];
    }
  }

  //mat linear_pred(N,L,fill::zeros);
  
  //NumericMatrix mu(N,L);
  //mu = mu_zero;
  
  mat mu(N,L);
  for(int l = 0; l < L; ++l)
  {
    vec temp_mu_vec(N);
    temp_mu_vec.fill( y_Mean_mu_zero[l]);
    mu.col(l) = temp_mu_vec;
  }


  NumericVector Vec_zero(N);
  for(int i = 0; i < N; ++i)
  {
    Vec_zero[i] = 0.0;
  }
  
  int No_coef;
  if(intercept)
  {
    No_coef = 2;           
  }else
  { 
    No_coef = 1;
  }

  List Beta(K);
  for(int k = 0; k < K; ++k)
  {
    List Beta_K(Dk[k]);
    for(int d = 0; d < Dk[k]; ++d)
    {
      List Beta_Dk(H);
      for(int h = 0; h < H; ++h)
      {
        List Beta_H(G);
        for(int g = 0; g < G; ++g)
        {
          List Beta_G(L);
          for(int l = 0; l < L; ++l)
          {
            mat temp_Beta_G(No_coef,1);
            temp_Beta_G.fill(0);
            Beta_G[l] = temp_Beta_G;
          }
          Beta_H[g] = Beta_G;
        }
        Beta_Dk[h] = Beta_H;
      }
      Beta_K[d] = Beta_Dk;
    }
    Beta[k] = Beta_K;
  }

  NumericMatrix Error_Rate(M,L);
  List Variable_Select(M);
  List Response_Select(M);
  List Beta_Hat_List(M);
  List Sum_Beta_Hat_List(M);
  List Beta_Hat_List_Iter(M);
  List mu_List(M);
  List sq_residual_gm(M);
  List Lambda_List(M);
  NumericMatrix Phi(M,L);
  NumericMatrix Rho(M,L);
  NumericVector line_search_est(L);
  List negative_gradient(M);
  List list_linear_pred(M);
  List list_mu_std(M);

  // remove this later
  List List_Mat_Sum_Beta_Hat_iter(M);

  for(int m = 0; m < M; ++m)
  {
    Variable_Select[m] = NA_INTEGER;
    Response_Select[m] = NA_INTEGER;
    Beta_Hat_List[m] = NA_REAL;
    Sum_Beta_Hat_List[m] = NA_REAL;
    sq_residual_gm[m] = NA_REAL;
    Beta_Hat_List_Iter[m] = NA_REAL;

    // remove this later
    List_Mat_Sum_Beta_Hat_iter[m] = NA_REAL;

    mu_List[m] = NA_REAL;
    Lambda_List[m] = NA_REAL;
    for(int l = 0; l < L; ++l)
    {
      Error_Rate(m,l) = NA_REAL;
      Phi(m,l) = NA_REAL;
      Rho(m,l) = NA_REAL;
    }
  }

  List Beta_Hat_Old(K);
  Beta_Hat_Old = clone(Beta);
  List V_inv(L);

  if(Trace)
  {
    Rcout << "Boosting iterations begin" << "\n";
  }

  // Boosting iteration starts here...  
  
  for(int m = 0; m <  M; ++m)
  {
    
    if(Verbose)
    {
      if( (m+1)%(M/10) == 0.0)
      {
        double FracBoosting;
        FracBoosting = ( (m+1)*100)/M;
        int Perc_boosting = FracBoosting;
         Rcout << Perc_boosting << "%" << " ";
      }
    }
    
    if(m == 0 || VarFlag == TRUE)
    {
      if(Trace)
      {
        Rcout << "Extraction begins for V_inv" << "\n";
      }
      for(int l = 0; l < L; ++l)
      {
        List Vi_inv(n);
        for(int i = 0; i < n;++i)
        {
          arma::mat Vi_inv_Temp;
          Vi_inv_Temp = MatrixInversion_Equicorrelation_C(ni[i],phi[l],rho[l]);
          Vi_inv[i] = Vi_inv_Temp;
        }
        V_inv[l] = Vi_inv;
      }
      if(Trace)
      {
        Rcout << "Extraction ends for V_inv" << "\n";
      }
    }
    
    if(Trace)
    {
      Rcout << "Extraction begins for negative gradient" << "\n";
    }
    List gm(L);
    for(int l = 0; l < L; ++l)
    {
      List gm_n(n);
      List V_inv_l = V_inv[l];
      for(int i = 0; i < n; ++i)
      {
        arma::mat V_inv_i = V_inv_l[i];
        IntegerVector id_index_Temp = id_index[i];
        NumericVector gm_ni(ni[i]);

        if(y_scale_summary == "continuous")
        {
          for(int j = 0; j < ni[i]; ++j)
          {
            gm_ni[j] = (y(id_index_Temp[j],l) - mu(id_index_Temp[j],l));
          }
          if(Mod_Grad)
          {
            gm_n[i] = gm_ni;  
          }else 
          {
            gm_n[i] = Matrix_Vector_Multiplication_C(V_inv_i,gm_ni);
          }
        }else
        {
          if(y_scale_summary == "binary")
          {
            NumericVector Delta_gm(ni[i]);
            for(int j = 0; j < ni[i]; ++j)
            {
              gm_ni[j] = y(id_index_Temp[j],l) - mu(id_index_Temp[j],l);
              Delta_gm[j] = mu(id_index_Temp[j],l)*(1 - mu(id_index_Temp[j],l) );
            }
              NumericMatrix temp_mat_Delta_gm = Diag_Matrix_C(Delta_gm);
              arma::mat mat_Delta_gm = arma::mat(temp_mat_Delta_gm.begin(), temp_mat_Delta_gm.nrow(), temp_mat_Delta_gm.ncol(), false);
            if(Mod_Grad)
            {
              gm_n[i] = Matrix_Vector_Multiplication_C_naObs(mat_Delta_gm,gm_ni);
            }else
            {
              gm_n[i] = Matrix_Vector_Multiplication_C_naObs(Matrix_Multiplication_Arma_C(mat_Delta_gm,V_inv_i), gm_ni);
            }
          }
        }
      }
      gm[l] = gm_n;
    }


    negative_gradient[m] = gm;
    if(Trace)
    {
      Rcout << "Extraction ends for negative gradient" << "\n";
    }

    if(Trace)
    {
      Rcout << "Extraction begins for initialization of beta" << "\n";
    }
    List Beta_Hat(K);
    if(m > 0)
    {
    if(Trace)
    {
      Rcout << "Estimation begins for beta for the " << m << " iteration" << "\n";
    }
      for(int k = 0; k < K; ++k)
      {
        List Beta_Hat_Old_Temp_Dk = Beta_Hat_Old[k];
        List Bxt_Temp_K = Byt_xt_time_map[k];
        NumericVector Bx_Scale_K = Bx_Scale[k];
        List Beta_Hat_Dk(Dk[k]);
        for(int d = 0; d < Dk[k]; ++d)
        {
          List Beta_Hat_Old_Temp_H = Beta_Hat_Old_Temp_Dk[d];
          List Bxt_Temp_Dk = Bxt_Temp_K[d];
          List Beta_Hat_H(H);
          for(int h = 0; h < H; ++h)
          {
            List Beta_Hat_Old_Temp_G = Beta_Hat_Old_Temp_H[h];
            List Bxt_Temp_H = Bxt_Temp_Dk[h];
            List Beta_Hat_G(G);
            for(int g = 0; g < G; ++g)
            {
              List Beta_Hat_Old_Temp_L = Beta_Hat_Old_Temp_G[g];
              List Bxt_Temp_G = Bxt_Temp_H[g];
              List Beta_Hat_L(L);
              for(int l = 0; l < L; ++l)
              {
                mat Beta_Hat_Old_Temp_Temp = Beta_Hat_Old_Temp_L[l];
                double Beta_Hat_Old_Temp;
                if(intercept)
                {
                  Beta_Hat_Old_Temp = Beta_Hat_Old_Temp_Temp(1,0);
                }else
                {
                  Beta_Hat_Old_Temp = Beta_Hat_Old_Temp_Temp(0,0);
                }
                if(Beta_Hat_Old_Temp == 0.0)
                {
                mat temp_Beta_G(No_coef,1);
                temp_Beta_G.fill(0); 
                Beta_Hat_L[l] = temp_Beta_G;               
                } else
                {
                  List V_inv_l = V_inv[l];
                  List gm_Temp_L = gm[l];
                  mat Temp_Beta_Hat_L(No_coef,1);
                  Temp_Beta_Hat_L = fit_component_gls(Bxt_Temp_G , gm_Temp_L, V_inv_l, intercept);
                  mat Temp_Beta_Hat_L_Bx_Scale(size(Temp_Beta_Hat_L));
                  Temp_Beta_Hat_L_Bx_Scale = Temp_Beta_Hat_L/Bx_Scale_const;
                  Beta_Hat_L[l] = Temp_Beta_Hat_L_Bx_Scale;
                }
              }
              Beta_Hat_G[g] = Beta_Hat_L;
            }
            Beta_Hat_H[h] = Beta_Hat_G; 
          }
          Beta_Hat_Dk[d] = Beta_Hat_H;
        }
        Beta_Hat[k] = Beta_Hat_Dk;
      }
    if(Trace)
    {
      Rcout << "Estimation ends for beta for the "<< m <<" iteration" << "\n";
    }
    }else
    {
      if(Trace)
      {
        Rcout << "First boosting iteration begins" << "\n";
      }
      if(Trace)
      {
        Rcout << "Estimation begins for beta for the "<< m <<" iteration" << "\n";
      }
      for(int k = 0; k < K; ++k)
      {
        List Bxt_Temp_K = Byt_xt_time_map[k];
        NumericVector Bx_Scale_K = Bx_Scale[k];
        List Beta_Hat_Dk(Dk[k]);
        for(int d = 0; d < Dk[k]; ++d)
        {
          List Bxt_Temp_Dk = Bxt_Temp_K[d];
          List Beta_Hat_H(H);
          for(int h = 0; h < H; ++h)
          {
            List Bxt_Temp_H = Bxt_Temp_Dk[h];
            List Beta_Hat_G(G);
            for(int g = 0; g < G; ++g)
            {
              List Bxt_Temp_G = Bxt_Temp_H[g];
              List Beta_Hat_L(L);
              for(int l = 0; l < L; ++l)
              {
                List V_inv_l = V_inv[l];
                List gm_Temp_L = gm[l];
                mat Temp_Beta_Hat_L(No_coef,1);
                Temp_Beta_Hat_L = fit_component_gls(Bxt_Temp_G , gm_Temp_L, V_inv_l, intercept);
                mat Temp_Beta_Hat_L_Bx_Scale(size(Temp_Beta_Hat_L));
                Temp_Beta_Hat_L_Bx_Scale = Temp_Beta_Hat_L/Bx_Scale_const;
                Beta_Hat_L[l] = Temp_Beta_Hat_L_Bx_Scale;
              }
              Beta_Hat_G[g] = Beta_Hat_L;
            }
            Beta_Hat_H[h] = Beta_Hat_G; 
          }
          Beta_Hat_Dk[d] = Beta_Hat_H;
        }
        Beta_Hat[k] = Beta_Hat_Dk;
      }
      if(Trace)
      {
        Rcout << "Estimation ends for beta for the "<< m <<" iteration" << "\n";
      }
      if(Trace)
      {
        Rcout << "First boosting iteration ends" << "\n";
      } 
    }
    
    Beta_Hat_List_Iter[m] = Beta_Hat;
    Beta_Hat_Old = Beta_Hat;
    
    if(Trace)
    {
      Rcout << "Estimation begins for List_Mat_Sum_Beta_Hat for the "<< m <<" iteration" << "\n";
    }

    List List_Mat_Sum_Beta_Hat(H);
    for(int h = 0; h < H; ++h)
    {
      List temp_Mat_Sum_Beta_Hat(G);
      for(int g = 0; g < G; ++g)
      {
        NumericMatrix Mat_Sum_Beta_Hat(K,L);
        for(int k = 0; k < K; ++k)
        {
          List Beta_Hat_Temp_K = Beta_Hat[k]; 
          for(int l = 0; l < L; ++l)
          {
            for(int d = 0; d < Dk[k]; ++d)
            {
              double Mult_Factor = 1.0;
              List Beta_Hat_Temp_Dk = Beta_Hat_Temp_K[d];
              List Beta_Hat_Temp_H = Beta_Hat_Temp_Dk[h];
              List Beta_Hat_Temp_G = Beta_Hat_Temp_H[g];
              mat Beta_Hat_Temp_L = Beta_Hat_Temp_G[l];
              double Beta_Hat_Temp_L_elm;
              if(intercept)
              {
                Beta_Hat_Temp_L_elm = Beta_Hat_Temp_L(1,0);
              }else
              {
                Beta_Hat_Temp_L_elm = Beta_Hat_Temp_L(0,0);
              }
              Mat_Sum_Beta_Hat(k,l) = Mat_Sum_Beta_Hat(k,l) + ((Beta_Hat_Temp_L_elm*Beta_Hat_Temp_L_elm)/Mult_Factor);
            }
          }
        }
        temp_Mat_Sum_Beta_Hat[g] = Mat_Sum_Beta_Hat;
      }
      List_Mat_Sum_Beta_Hat[h] = temp_Mat_Sum_Beta_Hat;
    }

    List_Mat_Sum_Beta_Hat_iter[m] = List_Mat_Sum_Beta_Hat;

    if(Trace)
    {
      Rcout << "Estimation ends for List_Mat_Sum_Beta_Hat for the "<< m <<" iteration" << "\n";
    }

  if(Trace)
  {
    Rcout << "Estimation begins for Sum_Beta_Zero for the "<< m <<" iteration" << "\n";
  }
  LogicalVector Sum_Beta_Zero(H);
  for(int h = 0; h < H; ++h)
  {
    List temp_List_Mat_Sum_Beta_Hat = List_Mat_Sum_Beta_Hat[h];
    NumericVector Vec_Sum_Beta_Hat_G(G);
    for(int g = 0; g < G; ++g)
    {
      NumericMatrix Mat_Sum_Beta_Hat = temp_List_Mat_Sum_Beta_Hat[g];
      NumericVector Vec_Sum_Beta_Hat_K(K);
      for(int k = 0; k < K; ++k)
      {
        NumericVector Vec_Sum_Beta_Hat_L(L);
        for(int l = 0; l < L; ++l)
        {
          Vec_Sum_Beta_Hat_L[l] = Mat_Sum_Beta_Hat(k,l);
        }
        Vec_Sum_Beta_Hat_K[k] = Sum_C_NA(Vec_Sum_Beta_Hat_L);
      }
      Vec_Sum_Beta_Hat_G[g] = Sum_C_NA(Vec_Sum_Beta_Hat_K);
    }
    Sum_Beta_Zero[h] = is_true(all(Vec_Sum_Beta_Hat_G == 0.0));
  }
  if(Trace)
  {
    Rcout << "Estimation ends for Sum_Beta_Zero for the "<< m <<" iteration" << "\n";
  }
  
  if(is_true(all(Sum_Beta_Zero)) )
  {
    M = (m-1);
    if(Trace)
    {
      Rcout << "Breaking iteration at " << M << "\n";
    }
    break;
  }

    if(Trace)
    {
      Rcout << "Estimation begins for List_Mat_sq_residual_gm for the "<< m <<" iteration" << "\n";
    }
    List List_Mat_sq_residual_gm(H);
    for(int h = 0; h < H; ++h)
    {
      List temp_List_Mat_sq_residual_gm(G);
      for(int g = 0; g < G; ++g)
      {
        NumericMatrix Mat_sq_residual_gm(K,L);
        for(int k = 0; k < K; ++k)
        {
          List Bxt_Temp_K = Byt_xt_time_map[k];
          List Beta_Hat_Temp_K = Beta_Hat[k]; 
          for(int l = 0; l < L; ++l)
          {
            List gm_Temp_L = gm[l];
            for(int d = 0; d < Dk[k]; ++d)
            {
              List Bxt_Temp_Dk = Bxt_Temp_K[d];
              List Bxt_Temp_H = Bxt_Temp_Dk[h];
              List Bxt_Temp_G = Bxt_Temp_H[g];

              List Beta_Hat_Temp_Dk = Beta_Hat_Temp_K[d];
              List Beta_Hat_Temp_H = Beta_Hat_Temp_Dk[h];
              List Beta_Hat_Temp_G = Beta_Hat_Temp_H[g];
              mat Beta_Hat_Temp_L = Beta_Hat_Temp_G[l];

              double output = sq_residual(Bxt_Temp_G, gm_Temp_L, Beta_Hat_Temp_L, intercept);
              Mat_sq_residual_gm(k,l) = Mat_sq_residual_gm(k,l) + output;
            }
          }
        }
        temp_List_Mat_sq_residual_gm[g] = Mat_sq_residual_gm;
      }
      List_Mat_sq_residual_gm[h] = temp_List_Mat_sq_residual_gm;
    }
  
  if(Trace)
  {
  Rcout << "Estimation ends for List_Mat_sq_residual_gm for the "<< m <<" iteration" << "\n";
  }
  sq_residual_gm[m] = List_Mat_sq_residual_gm;

  if(Trace)
  {
    Rcout << "Estimation begins for km_H & lm_H for the "<< m <<" iteration" << "\n";
  }

  /*
  Date 03/08/2023

  When I use List_Mat_sq_residual_gm, where I look at the squared difference between the response (negative gradient) and its estimate (x*beta), I see that some responses are getting selected very often compared to other responses, even when those other responses are also important. In order to avoid this, I am going to consider square beta estimate for response and covariate selection 
  */

  IntegerMatrix km_GH(G,H);
  IntegerMatrix lm_GH(G,H);

  for(int h = 0; h < H; ++h)
  {
    List temp_List_Mat_sq_residual_gm = List_Mat_sq_residual_gm[h];
    List temp_List_Mat_Sum_Beta_Hat = List_Mat_Sum_Beta_Hat[h];
    for(int g = 0; g < G; ++g)
    {
      IntegerVector km_lm(2);
      if(sq_residual_select)
      {
        NumericMatrix Mat_sq_residual_gm = temp_List_Mat_sq_residual_gm[g];
        km_lm = Which_Min_Matrix_NA_NRA(Mat_sq_residual_gm);
      }else
      {
        NumericMatrix Mat_sq_beta = temp_List_Mat_Sum_Beta_Hat[g];
        km_lm = Which_Max_Matrix_NA_NRA(Mat_sq_beta);
      }
      km_GH(g,h) = km_lm[0];
      lm_GH(g,h) = km_lm[1];
    }
  }

  Variable_Select[m] = km_GH;
  Response_Select[m] = lm_GH;

  if(Trace)
  {
    Rcout << "Estimation ends for km_H & lm_H for the "<< m <<" iteration" << "\n";
  }

  List base_learner_H(L);
  if(y_scale_summary == "binary")
  {
    if(Trace)
    {
      Rcout << "Estimation begins for base_learner for the "<< m <<" iteration" << "\n";
    }
    List base_learner(L);
    for(int l = 0; l < L; ++l)
    {
      List base_learner_n(n);
      for(int i = 0; i < n; ++i)
      {
      NumericVector temp_base_learner(ni[i]);
      NumericVector temp_temp_base_learner(ni[i]);
      for(int h = 0; h < H; ++h)
      {
        for(int g = 0; g < G; ++g)
        {
          for(int k = 0; k < K; ++k)
          {
              /*
              Date 02/10/2023

              Here, as a part of derivation of base learner, I am summing over k = km_h. However, later when I derive Sum_Beta, I am going to sum over Beta_Hat for k = km_h and l = lm_h. I am not sure if this will end up double summing. I don't think this is the case because here I am using estimate corresponding to km_h for estimation of line search parameter and there I used for actual summing of beta. HOWEVER, IT IS WORTH DOUBLE CHECKING; COME BACK TO THIS ISSUE.
              */
              if(update_all_comp)
              {
                List Beta_Hat_K = Beta_Hat[k];
                List Bxt_Temp_K = Byt_xt_time_map[k];
                for(int d = 0; d < Dk[k]; ++d)
                {
                  List Beta_Hat_Dk = Beta_Hat_K[d];
                  List Beta_Hat_H = Beta_Hat_Dk[h];
                  List Beta_Hat_G = Beta_Hat_H[g];
                  mat Beta_Hat_L =  Beta_Hat_G[l];

                  List Bxt_Temp_Dk = Bxt_Temp_K[d];
                  List Bxt_Temp_H = Bxt_Temp_Dk[h];
                  List Bxt_Temp_G = Bxt_Temp_H[g];
                
                  temp_temp_base_learner =  extract_base_learner(Bxt_Temp_G[i], Beta_Hat_L, intercept,No_coef);
                  temp_base_learner = Sum_numericVectors(temp_base_learner, temp_temp_base_learner);
                }
              }else
              {
                /*
                Note that here I have specifically decided not to update all resposes for calculating base learner; otherwise it would have created a problem for estimating line search parameter.
                */
                int km_gh = km_GH(g,h);
                if( !(IntegerVector::is_na(km_gh)) && k == km_gh)
                {
                  IntegerVector temp_comp_x = comp_x[k];
                  for(int kk = 0; kk < temp_comp_x.size(); ++kk)
                  {
                    List Beta_Hat_K = Beta_Hat[  temp_comp_x[kk]   ];
                    List Bxt_Temp_K = Byt_xt_time_map[  temp_comp_x[kk] ];
                    for(int dd = 0; dd < Dk[ temp_comp_x[kk]  ]; ++dd)
                    {
                      List Beta_Hat_Dk = Beta_Hat_K[dd];
                      List Beta_Hat_H = Beta_Hat_Dk[h];
                      List Beta_Hat_G = Beta_Hat_H[g];
                      mat Beta_Hat_L =  Beta_Hat_G[l];

                      List Bxt_Temp_Dk = Bxt_Temp_K[dd];
                      List Bxt_Temp_H = Bxt_Temp_Dk[h];
                      List Bxt_Temp_G = Bxt_Temp_H[g];

                      temp_temp_base_learner =  extract_base_learner(Bxt_Temp_G[i], Beta_Hat_L, intercept,No_coef);
                      temp_base_learner = Sum_numericVectors(temp_base_learner, temp_temp_base_learner); 
                    }
                  }
                }
              }
            
          }
        }
      }
      base_learner_n[i] = temp_base_learner;
      }
      base_learner[l] = base_learner_n;
    }

    if(Trace)
    {
      Rcout << "Estimation ends for base_learner for the "<< m <<" iteration" << "\n";
    }

    if(Trace)
    {
      Rcout << "Estimation begins for base_learner_H for the "<< m <<" iteration" << "\n";
    }

    for(int l = 0; l < L; ++l)
    {
      List temp_base_learner = base_learner[l];
      List base_learner_H_n(n);
      for(int i = 0; i < n; ++i)
      {
        vec temp_base_learner_n = temp_base_learner[i];
        vec mu_temp(ni[i]); 
        IntegerVector temp_id_index = id_index[i];
        for(int j = 0; j < ni[i]; ++j)
        {
          mu_temp[j] = mu( temp_id_index[j] , l);
        }
        vec mu_temp_temp = mu_temp % (1 - mu_temp);
        mat diag_mu_temp_temp = diagmat(mu_temp_temp);
        /*
        Date 02/07/2023
        Here, I am multiplying a diagonal matrix with an object vec.
        */
        base_learner_H_n[i] = diag_mu_temp_temp*temp_base_learner_n;
      }
      base_learner_H[l] = base_learner_H_n;
    }

    if(Trace)
    {
      Rcout << "Estimation ends for base_learner_H for the "<< m <<" iteration" << "\n";
    }
  }

  if(Trace)
  {
    Rcout << "Estimation begins for line_search_est for the "<< m <<" iteration" << "\n";
  }

  for(int l = 0; l < L; ++l)
  {
    if(y_scale_summary == "binary")
    {
    List residual_line_search(n);
    for(int i = 0; i < n; ++i)
    {
      vec temp_response(ni[i]); 
      vec temp_mu(ni[i]); 
      IntegerVector temp_id_index = id_index[i];
      for(int j = 0; j < ni[i]; ++j)
      {
        temp_response[j] = y(temp_id_index[j] , l);
        temp_mu[j] = mu( temp_id_index[j] , l);
      }
      residual_line_search[i] = temp_response - temp_mu;
    }

    line_search_est[l] = fit_component_gls_line_search(base_learner_H[l], residual_line_search, V_inv[l]);
    }else
    {
      if(y_scale_summary == "continuous")
      {
        line_search_est[l] = 1.0;
      }
    }

  }
  if(Trace)
  {
    Rcout << "Estimation ends for line_search_est for the "<< m <<" iteration" << "\n";
  }

  if(Trace)
  {
    Rcout << "Estimation begins for Sum_Beta for the "<< m <<" iteration" << "\n";
  }

 // version 1
  /*
    List Sum_Beta(K);
    for(int k = 0; k < K; ++k)
    {
      List Beta_Hat_K = Beta_Hat[k];
      List Beta_K = Beta[k];
      List Sum_Beta_Dk(Dk[k]);
      for(int d = 0; d < Dk[k]; ++d)
      {
        List Beta_Hat_Dk = Beta_Hat_K[d];
        List Beta_Dk = Beta_K[d];
        List Sum_Beta_H(H);
        for(int h = 0; h < H; ++h)
        {
          List Beta_Hat_H = Beta_Hat_Dk[h];
          List Beta_H = Beta_Dk[h];
          List Sum_Beta_G(G);
          for(int g = 0; g < G; ++g)
          {
            List Beta_Hat_G = Beta_Hat_H[g];
            List Beta_G = Beta_H[g];
            List Sum_Beta_L(L);
            IntegerVector km_lm_Temp(2);
            km_lm_Temp[0] = km_GH(g,h);
            km_lm_Temp[1] = lm_GH(g,h);

            for(int l = 0; l < L; ++l)
            {
              double temp_line_search_est = line_search_est[l];
              // 
              //Date 02/10/2023

              //Here, in two places, I have copied elements from one list to other list, where the individual elements are object mat.
              //
              if(update_all_comp)
              {
                mat temp_Sum_Beta_L(No_coef,1);// NOT SURE ABOUT THIS when Dk > 1
                mat temp_Beta_G = Beta_G[l];
                mat temp_Beta_Hat_G = Beta_Hat_G[l];
                double prod_nu_line_search = (nu*temp_line_search_est);
                mat mat_prod_nu_line_search(No_coef,1);
                mat_prod_nu_line_search.fill(prod_nu_line_search);
                temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);
                Sum_Beta_L[l] =  temp_Sum_Beta_L; 
              }else
              {
                if( is_true(any( km_lm_Temp == -1 )) || IntegerVector::is_na(km_lm_Temp[0]) || IntegerVector::is_na(km_lm_Temp[1]) )
                {
                  Sum_Beta_L[l] = Beta_G[l];
                }else
                {
                  if(k == km_lm_Temp[0] && l == km_lm_Temp[1])
                  {
                    mat temp_Sum_Beta_L(No_coef,1);
                    mat temp_Beta_G = Beta_G[l];
                    mat temp_Beta_Hat_G = Beta_Hat_G[l];
                    double prod_nu_line_search = (nu*temp_line_search_est);
                    mat mat_prod_nu_line_search(No_coef,1);
                    mat_prod_nu_line_search.fill(prod_nu_line_search);
                    temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);
                    Sum_Beta_L[l] =  temp_Sum_Beta_L; 
                  }else
                  {
                    Sum_Beta_L[l] = Beta_G[l];
                  } 
                }
              }
            }
            Sum_Beta_G[g] = Sum_Beta_L;
          }
          Sum_Beta_H[h] = Sum_Beta_G;
        }
        Sum_Beta_Dk[d] = Sum_Beta_H;
      }
      Sum_Beta[k] = Sum_Beta_Dk;
    }
    
    
// version 2
    List Sum_Beta(K);
    for(int h = 0; h < H; ++h)
    {
      for(int g = 0; g < G; ++g)
      {
        IntegerVector km_lm_Temp(2);
        km_lm_Temp[0] = km_GH(g,h);
        km_lm_Temp[1] = lm_GH(g,h);
        for(int l = 0; l < L; ++l)
        {
          double temp_line_search_est = line_search_est[l];
          for(int k = 0; k < K; ++k)
          {
            if(update_all_comp)
            {
              List Beta_Hat_K = Beta_Hat[k];
              List Beta_K = Beta[k];
              List Sum_Beta_Dk(Dk[k]);
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Beta_Hat_Dk = Beta_Hat_K[d];
                List Beta_Dk = Beta_K[d];
                List Sum_Beta_H(H);

                List Beta_Hat_H = Beta_Hat_Dk[h];
                List Beta_H = Beta_Dk[h];
                List Sum_Beta_G(G);

                List Beta_Hat_G = Beta_Hat_H[g];
                List Beta_G = Beta_H[g];
                List Sum_Beta_L(L);

                mat temp_Sum_Beta_L(No_coef,1);// NOT SURE ABOUT THIS when Dk > 1
                mat temp_Beta_G = Beta_G[l];
                mat temp_Beta_Hat_G = Beta_Hat_G[l];
                double prod_nu_line_search = (nu*temp_line_search_est);
                mat mat_prod_nu_line_search(No_coef,1);
                mat_prod_nu_line_search.fill(prod_nu_line_search);
                temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);
                Sum_Beta_L[l] =  temp_Sum_Beta_L;
                Sum_Beta_G[g] = Sum_Beta_L;
                Sum_Beta_H[h] = Sum_Beta_G;
                Sum_Beta_Dk[d] = Sum_Beta_H;
              }
              Sum_Beta[k] = Sum_Beta_Dk;
            }else
            {
              if(k == km_lm_Temp[0])
              {
                IntegerVector temp_comp_x = comp_x[k];
                for(int kk = 0; kk < temp_comp_x.size(); ++kk)
                {
                  List Beta_Hat_K = Beta_Hat[ temp_comp_x[kk] ];
                  List Beta_K = Beta[ temp_comp_x[kk] ];
                  List Sum_Beta_Dk(Dk[ temp_comp_x[kk] ]);
                  for(int dd = 0; dd < Dk[ temp_comp_x[kk]  ]; ++dd)
                  {
                    List Beta_Hat_Dk = Beta_Hat_K[dd];
                    List Beta_Dk = Beta_K[dd];
                    List Sum_Beta_H(H);

                    List Beta_Hat_H = Beta_Hat_Dk[h];
                    List Beta_H = Beta_Dk[h];
                    List Sum_Beta_G(G);

                    List Beta_Hat_G = Beta_Hat_H[g];
                    List Beta_G = Beta_H[g];
                    List Sum_Beta_L(L);

                    mat temp_Sum_Beta_L(No_coef,1);
                    mat temp_Beta_G = Beta_G[l];
                    mat temp_Beta_Hat_G = Beta_Hat_G[l];

                    double prod_nu_line_search = (nu*temp_line_search_est);
                    mat mat_prod_nu_line_search(No_coef,1);
                    mat_prod_nu_line_search.fill(prod_nu_line_search);

                    temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);

                    Sum_Beta_L[l] =  temp_Sum_Beta_L;

                    Sum_Beta_G[g] = Sum_Beta_L;

                    Sum_Beta_H[h] = Sum_Beta_G;

                    Sum_Beta_Dk[dd] = Sum_Beta_H;
                  }
                  Sum_Beta[ temp_comp_x[kk]    ] = Sum_Beta_Dk;
                }
              }else
              {
                List Beta_Hat_K = Beta_Hat[k];
                List Beta_K = Beta[k];
                List Sum_Beta_Dk(Dk[k]);

                for(int d = 0; d < Dk[k]; ++d)
                {
                  List Beta_Hat_Dk = Beta_Hat_K[d];
                  List Beta_Dk = Beta_K[d];
                  List Sum_Beta_H(H);
                  
                  List Beta_Hat_H = Beta_Hat_Dk[h];
                  List Beta_H = Beta_Dk[h];
                  List Sum_Beta_G(G);

                  List Beta_Hat_G = Beta_Hat_H[g];
                  List Beta_G = Beta_H[g];
                  List Sum_Beta_L(L);

                  Sum_Beta_L[l] = Beta_G[l];
                  Sum_Beta_G[g] = Sum_Beta_L;
                  Sum_Beta_H[h] = Sum_Beta_G;
                  Sum_Beta_Dk[d] = Sum_Beta_H;
                }
                Sum_Beta[k] = Sum_Beta_Dk;
              }
            }
          }
        }
      }
    }



     // version 3
  // Note that I need a way to save Sum_Beta and then keep it updated everytime k is included in multiple elements of comp_x.

    List Sum_Beta(K);
    for(int k = 0; k < K; ++k)
    {
      for(int h = 0; h < H; ++h)
      {
        for(int g = 0; g < G; ++g)
        {
          IntegerVector km_lm_Temp(2);
          km_lm_Temp[0] = km_GH(g,h);
          km_lm_Temp[1] = lm_GH(g,h);
          if(k < 5)//km_lm_Temp[0]
          {
            IntegerVector temp_comp_x = comp_x[k];
            for(int kk = 0; kk < temp_comp_x.size(); ++kk)
            {
              List Beta_Hat_K = Beta_Hat[ temp_comp_x[kk] ];
              List Beta_K = Beta[ temp_comp_x[kk] ];
              List Sum_Beta_Dk(Dk[ temp_comp_x[kk] ]);
              for(int dd = 0; dd < Dk[ temp_comp_x[kk]  ]; ++dd)
              {
                List Beta_Hat_Dk = Beta_Hat_K[dd];
                List Beta_Dk = Beta_K[dd];
                List Sum_Beta_H(H);

                List Beta_Hat_H = Beta_Hat_Dk[h];
                List Beta_H = Beta_Dk[h];
                List Sum_Beta_G(G);

                List Beta_Hat_G = Beta_Hat_H[g];
                List Beta_G = Beta_H[g];
                List Sum_Beta_L(L);

                for(int l = 0; l < L; ++l)
                {
                  double temp_line_search_est = line_search_est[l];
                  mat temp_Sum_Beta_L(No_coef,1);
                  mat temp_Beta_G = Beta_G[l];
                  mat temp_Beta_Hat_G = Beta_Hat_G[l];

                  double prod_nu_line_search = (nu*temp_line_search_est);
                  mat mat_prod_nu_line_search(No_coef,1);
                  mat_prod_nu_line_search.fill(prod_nu_line_search);

                  temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);

                  Sum_Beta_L[l] = temp_Sum_Beta_L;
                  Rcout << temp_Sum_Beta_L << "\n";
                }
                Sum_Beta_G[g] = Sum_Beta_L;
                Sum_Beta_H[h] = Sum_Beta_G;
                Sum_Beta_Dk[dd] = Sum_Beta_H;
              }
              Sum_Beta[ temp_comp_x[kk]    ] = Sum_Beta_Dk;
            }
          }else
          {
            List Beta_Hat_K = Beta_Hat[k];
            List Beta_K = Beta[k];
            List Sum_Beta_Dk(Dk[k]);

            for(int d = 0; d < Dk[k]; ++d)
            {
              List Beta_Hat_Dk = Beta_Hat_K[d];
              List Beta_Dk = Beta_K[d];
              List Sum_Beta_H(H);
              
              List Beta_Hat_H = Beta_Hat_Dk[h];
              List Beta_H = Beta_Dk[h];
              List Sum_Beta_G(G);

              List Beta_Hat_G = Beta_Hat_H[g];
              List Beta_G = Beta_H[g];
              List Sum_Beta_L(L);

              for(int l = 0; l < L; ++l)
              {
                mat temp_Beta_G = Beta_G[l];
                Sum_Beta_L[l] = temp_Beta_G;
                Rcout << temp_Beta_G << "\n";
              }
              Sum_Beta_G[g] = Sum_Beta_L;
              Sum_Beta_H[h] = Sum_Beta_G;
              Sum_Beta_Dk[d] = Sum_Beta_H;
            }
            Sum_Beta[k] = Sum_Beta_Dk;
          }
        }
      }
    }



  // version 4
  // Note that I need a way to save Sum_Beta and then keep it updated everytime k is included in multiple elements of comp_x.

  List Sum_Beta = clone(Beta);
  for(int kk = 0; kk < K; ++kk)
  {
    IntegerVector temp_comp_x = comp_x[kk];
    for(int k = 0; k < temp_comp_x.size(); ++k)
    {
      List Beta_Hat_K = Beta_Hat[ temp_comp_x[k] ];
      List Beta_K = Sum_Beta[ temp_comp_x[k] ];
      List Sum_Beta_Dk(Dk[ temp_comp_x[k] ]);
      for(int d = 0; d < Dk[ temp_comp_x[k] ]; ++d)
      {
        List Beta_Hat_Dk = Beta_Hat_K[d];
        List Beta_Dk = Beta_K[d];
        List Sum_Beta_H(H);
        for(int h = 0; h < H; ++h)
        {
          List Beta_Hat_H = Beta_Hat_Dk[h];
          List Beta_H = Beta_Dk[h];
          List Sum_Beta_G(G);
          for(int g = 0; g < G; ++g)
          {
            List Beta_Hat_G = Beta_Hat_H[g];
            List Beta_G = Beta_H[g];
            List Sum_Beta_L(L);

            IntegerVector km_lm_Temp(2);
            km_lm_Temp[0] = km_GH(g,h);
            km_lm_Temp[1] = lm_GH(g,h);
            for(int l = 0; l < L; ++l)
            {
              if(kk == km_lm_Temp[0])
              {
                double temp_line_search_est = line_search_est[l];
                mat temp_Sum_Beta_L(No_coef,1);
                mat temp_Beta_G = Beta_G[l];
                mat temp_Beta_Hat_G = Beta_Hat_G[l];

                double prod_nu_line_search = (nu*temp_line_search_est);
                mat mat_prod_nu_line_search(No_coef,1);
                mat_prod_nu_line_search.fill(prod_nu_line_search);

                temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);

                Sum_Beta_L[l] = temp_Sum_Beta_L;

              }else
              {
                Sum_Beta_L[l] = Beta_G[l];
              }
            }
            Sum_Beta_G[g] = Sum_Beta_L;
          }
          Sum_Beta_H[h] = Sum_Beta_G;
        }
        Sum_Beta_Dk[d] = Sum_Beta_H;
      }
      Sum_Beta[kk] = Sum_Beta_Dk;
    } 
  } 



  // version 5
  // Note that I need a way to save Sum_Beta and then keep it updated everytime k is included in multiple elements of comp_x.

  List Sum_Beta(K); // = clone(Beta);
  for(int kk = 0; kk < K; ++kk)
  {
    IntegerVector temp_comp_x = comp_x[kk];
    for(int k = 0; k < temp_comp_x.size(); ++k)
    {
      List Beta_Hat_K = Beta_Hat[ temp_comp_x[k] ];
      //List Beta_K = Sum_Beta[ temp_comp_x[k] ];
      List Beta_K = Beta[ temp_comp_x[k] ];
      List Sum_Beta_Dk(Dk[ temp_comp_x[k] ]);
      for(int d = 0; d < Dk[ temp_comp_x[k] ]; ++d)
      {
        List Beta_Hat_Dk = Beta_Hat_K[d];
        List Beta_Dk = Beta_K[d];
        List Sum_Beta_H(H);
        for(int h = 0; h < H; ++h)
        {
          List Beta_Hat_H = Beta_Hat_Dk[h];
          List Beta_H = Beta_Dk[h];
          List Sum_Beta_G(G);
          for(int g = 0; g < G; ++g)
          {
            List Beta_Hat_G = Beta_Hat_H[g];
            List Beta_G = Beta_H[g];
            List Sum_Beta_L(L);

            IntegerVector km_lm_Temp(2);
            km_lm_Temp[0] = km_GH(g,h);
            km_lm_Temp[1] = lm_GH(g,h);

            for(int ll = 0; ll < L; ++ll)
            {
              IntegerVector temp_comp_y = comp_y[ll];
              for(int l = 0; l < temp_comp_y.size(); ++l)
              {
                if(kk == km_lm_Temp[0] && ll == km_lm_Temp[1]) 
                {
                  Rcout << "g is = " << g << " And kk is = " << kk << " And ll is = " << ll << " And lm is = " << km_lm_Temp[1] << " And l is = " << temp_comp_y[l] << "\n";
                  double temp_line_search_est = line_search_est[ temp_comp_y[l] ];
                  mat temp_Sum_Beta_L(No_coef,1);
                  mat temp_Beta_G = Beta_G[ temp_comp_y[l] ];
                  mat temp_Beta_Hat_G = Beta_Hat_G[temp_comp_y[l]];
                  Rcout << "line search = " << temp_line_search_est << "\n";
                  Rcout << "temp_Beta_G = " << temp_Beta_G << "\n";
                  Rcout << "temp_Beta_Hat_G = " << temp_Beta_Hat_G << "\n";

                  double prod_nu_line_search = (nu*temp_line_search_est);
                  mat mat_prod_nu_line_search(No_coef,1);
                  mat_prod_nu_line_search.fill(prod_nu_line_search);

                  temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);

                  Rcout << "temp_Sum_Beta_L = " << temp_Sum_Beta_L << "\n";

                  Sum_Beta_L[ temp_comp_y[l] ] = temp_Sum_Beta_L;

                }else
                {
                  Sum_Beta_L[ temp_comp_y[l] ] = Beta_G[ temp_comp_y[l] ];
                }
              }
            }
            Sum_Beta_G[g] = Sum_Beta_L;
          }
          Sum_Beta_H[h] = Sum_Beta_G;
        }
        Sum_Beta_Dk[d] = Sum_Beta_H;
      }
      Sum_Beta[ temp_comp_x[k] ] = Sum_Beta_Dk;
    } 
  }


*/

  // version 6
  // Note that I need a way to save Sum_Beta and then keep it updated everytime k is included in multiple elements of comp_x.

  List Sum_Beta(K); // = clone(Beta);
  for(int kk = 0; kk < K; ++kk)
  {
      IntegerVector temp_comp_x = comp_x[kk];
      for(int k = 0; k < temp_comp_x.size(); ++k)
      {
        List Beta_Hat_K = Beta_Hat[ temp_comp_x[k] ];
        //List Beta_K = Sum_Beta[ temp_comp_x[k] ];
        List Beta_K = Beta[ temp_comp_x[k] ];
        List Sum_Beta_Dk(Dk[ temp_comp_x[k] ]);
        for(int d = 0; d < Dk[ temp_comp_x[k] ]; ++d)
        {
          List Beta_Hat_Dk = Beta_Hat_K[d];
          List Beta_Dk = Beta_K[d];
          List Sum_Beta_H(H);
          for(int h = 0; h < H; ++h)
          {
            List Beta_Hat_H = Beta_Hat_Dk[h];
            List Beta_H = Beta_Dk[h];
            List Sum_Beta_G(G);
            for(int g = 0; g < G; ++g)
            {
              List Beta_Hat_G = Beta_Hat_H[g];
              List Beta_G = Beta_H[g];
              List Sum_Beta_L(L);

              IntegerVector km_lm_Temp(2);
              km_lm_Temp[0] = km_GH(g,h);
              km_lm_Temp[1] = lm_GH(g,h);

              for(int ll = 0; ll < L; ++ll)
              {
                  if(kk == km_lm_Temp[0] && ll == km_lm_Temp[1]) 
                  {
                    IntegerVector temp_comp_y = comp_y[ll];
                    for(int l = 0; l < temp_comp_y.size(); ++l)
                    {
                      double temp_line_search_est = line_search_est[ temp_comp_y[l] ];
                      mat temp_Sum_Beta_L(No_coef,1);
                      mat temp_Beta_G = Beta_G[ temp_comp_y[l] ];
                      mat temp_Beta_Hat_G = Beta_Hat_G[temp_comp_y[l]];
                      double prod_nu_line_search = (nu*temp_line_search_est);
                      mat mat_prod_nu_line_search(No_coef,1);
                      mat_prod_nu_line_search.fill(prod_nu_line_search);
                      temp_Sum_Beta_L =  temp_Beta_G + ( mat_prod_nu_line_search % temp_Beta_Hat_G);
                      Sum_Beta_L[ temp_comp_y[l] ] = temp_Sum_Beta_L;
                    }
                  }else
                  {
                    Sum_Beta_L[ ll ] = Beta_G[ ll ];
                  }
                
              }
              Sum_Beta_G[g] = Sum_Beta_L;
            }
            Sum_Beta_H[h] = Sum_Beta_G;
          }
          Sum_Beta_Dk[d] = Sum_Beta_H;
        }
        Sum_Beta[ temp_comp_x[k] ] = Sum_Beta_Dk;
      } 

  }

    Beta = clone(Sum_Beta);
    Beta_Hat_List[m] = Beta;
    
  if(Trace)
  {
    Rcout << "Estimation ends for Sum_Beta for the "<< m <<" iteration" << "\n";
  }

  if(Trace)
  {
    Rcout << "Estimation begins for linear_pred for the "<< m <<" iteration" << "\n";
  }
    mat linear_pred(N,L,fill::zeros);
    for(int l = 0; l < L; ++l)
    {
      vec Temp_linear_pred(N,fill::zeros);
      for(int k = 0; k < K; ++k)
      {
        List Bxt_Temp_K = Byt_xt_time_map[k];
        List Beta_Temp_K = Beta[k];
        for(int d = 0; d < Dk[k]; ++d)
        {
          List Bxt_Temp_Dk = Bxt_Temp_K[d];
          List Beta_Temp_Dk = Beta_Temp_K[d];
          for(int h = 0; h < H; ++h)
          {
            List Bxt_Temp_H = Bxt_Temp_Dk[h];
            List Beta_Temp_H = Beta_Temp_Dk[h]; 
            for(int g = 0; g < G; ++g)
            {
              List Bxt_Temp_G = Bxt_Temp_H[g];
              List Beta_Temp_G = Beta_Temp_H[g];
              mat Beta_Temp_L = Beta_Temp_G[l];
              vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
              Temp_linear_pred = Temp_linear_pred + Temp_Temp_linear_pred;
            }
          }
        }
      }
      linear_pred.col(l) = Temp_linear_pred;
    }

    list_linear_pred[m] = linear_pred;


    mat Org_linear_pred(N,L);
    for(int l = 0; l < L; ++l)
    {
      Org_linear_pred.col(l) = ( linear_pred.col(l) * y_Std_Error[l] ) + y_Mean[l];
    }

    //NumericMatrix Org_linear_pred(N,L);
    //for(int l = 0; l < L; ++l){
    //  for(int i = 0; i < N; ++i){
    //    Org_linear_pred(i,l) = ( linear_pred(i,l) * y_Std_Error[l] ) + y_Mean[l];
    //  }
    //}



  if(Trace)
  {
    Rcout << "Estimation ends for linear_pred for the "<< m <<" iteration" << "\n";
  }

  if(Trace)
  {
    Rcout << "Estimation begins for mu for the "<< m <<" iteration" << "\n";
  }

    for(int l = 0; l < L; ++l)
    {
      vec Temp_linear_pred_mu = linear_pred.col(l);
      mu.col(l) = extract_mu(Temp_linear_pred_mu,y_scale_summary );
    }

    //for(int l = 0; l < L; ++l)
    //{
    //  for(int i = 0; i < N; ++i)
    //  {
    //    mu(i,l) = linear_pred(i,l);
    //  }
    //}

    list_mu_std[m] = mu;

    //NumericMatrix Org_mu(N,L);
    //for(int l = 0; l < L; ++l)
    //{
    //  for(int i = 0; i < N; ++i)
    //  {
    //    Org_mu(i,l) = Org_linear_pred(i,l);
    //  }
    //}

    mat Org_mu(N, L);
    for(int l = 0; l < L; ++l)
    {
      vec Temp_Org_linear_pred = Org_linear_pred.col(l);
      Org_mu.col(l) = extract_mu(Temp_Org_linear_pred,y_scale_summary );
    }

    mu_List[m] = Org_mu;
    
  if(Trace)
  {
    Rcout << "Estimation ends for mu for the "<< m <<" iteration" << "\n";
  }
    

    if(VarFlag)
    {
      if(Trace)
      {
        Rcout << "Estimation begins for rho for the "<< m <<" iteration" << "\n";
      }
      for(int l = 0; l < L; ++l)
      {
        NumericVector Temp_Resid(N);
        for(int i = 0; i < N; ++i)
        {
         Temp_Resid[i] = Org_y(i,l) - Org_mu(i,l); 
        }

        NumericVector Resid(N);

        LogicalVector which_isNA = isNA_Numeric(Temp_Resid);
        if( any_function(which_isNA) )
        {
          for(int i = 0; i < N; ++i)
          {
            if( NumericVector::is_na(Temp_Resid[ i ]) )
            {
              Resid[i] = 0;
            }else
            {
              Resid[i] = Temp_Resid[i];
            }
          }
        }else
        {
          Resid = Temp_Resid;
        }

        double phi_Temp;
        double phi_Init = 0.0;
        double phi_count = 0.0;
        double K_var = (double) K;
        for(int i = 0; i < n; ++i)
        {
          IntegerVector id_index_Temp = id_index[i];
          for(int j = 0; j < ni[i]; ++j)
          {
           phi_Init = phi_Init + (Resid[id_index_Temp[j]]*Resid[id_index_Temp[j]]);
           phi_count = phi_count + 1.0;  
          }
        }

        if(phi_count <= K_var)
        {
          phi_Temp = phi_Init/phi_count;
        } else
        {
          phi_Temp = phi_Init/(phi_count - K_var);  
        }

        if(phi_Temp <= 0.0)
        {
          phi[l] = 1.0;
        } else 
          {
            phi[l] = phi_Temp;
          }
        Phi(m,l) = phi[l];
        
        double rho_Temp;
        double rho_Init = 0.0;
        double rho_count = 0.0;
        for(int i = 0; i < n; ++i)
        {
          IntegerVector id_index_Temp = id_index[i];
          if(ni[i] > 1)
          {
            for(int j = 0; j < (ni[i]-1); ++j)
            {
              for(int jj = (j+1); jj < ni[i]; ++jj)
              {
                rho_Init = rho_Init + (Resid[id_index_Temp[j]]*Resid[id_index_Temp[jj]]);
                rho_count = rho_count + 1.0;  
              }
            }            
          }
        } 


        if(rho_count <= K_var)
        {
          rho_Temp = rho_Init/(rho_count*phi[l]);
        } else
        {
          rho_Temp = rho_Init/((rho_count - K_var)*phi[l]);  
        }


        if(rho_Temp < -1 || rho_Temp > 1)
        {
          rho[l] = 0.0;
        } else
        {
          rho[l] = rho_Temp;
        }
        Rho(m,l) = rho[l];
      }
      if(Trace)
      {
        Rcout << "Estimation ends for rho for the "<< m <<" iteration" << "\n";
      }
    }
    
    if(Trace)
    {
      Rcout << "Estimation begins for Error_Rate for the "<< m <<" iteration" << "\n";
    }
    for(int l = 0; l < L; ++l)
    {
      NumericVector Org_y_Temp(N);
      NumericVector mu_Temp(N);
      for(int i = 0; i < N; ++i)
      {
        Org_y_Temp[i] = Org_y(i,l);
        mu_Temp[i] = Org_mu(i,l);
      }
      if(y_scale_summary == "continuous")
      {
      Error_Rate(m,l) = l2Dist_Vector_C_NA(Org_y_Temp, mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index); 
      }else
      {
        if(y_scale_summary == "binary")
        {
          Error_Rate(m,l) = l2Dist_Vector_C_NA(Org_y_Temp, mu_Temp, id_index);
        }
      }

    }

    if(Trace)
    {
      Rcout << "Estimation ends for Error_Rate for the "<< m <<" iteration" << "\n";
    }
    
  } 
  
  if(Trace)
  {
    Rcout << "Boosting iterations end" << "\n";
  }

  if(Trace)
  {
    Rcout << "Estimation begins for Tm_Beta_C" << "\n";
  }
  List Tm_Beta_C(K);
  for(int k = 0; k < K; ++k)
  {
    if(UseRaw[k])
    {
      List Beta_K = Beta[k]; 
      List Tm_Beta_Dk(Dk[k]);
      for(int d = 0; d < Dk[k]; ++d)
      {
        List Beta_Dk = Beta_K[d];
        List Tm_Beta_H(H);
        for(int h = 0; h < H; ++h)
        {
          List Beta_H = Beta_Dk[h];
          List Bt_H_n = Bt_H[h];
          List Tm_Beta_G(G);
          for(int g = 0; g < G; ++g)
          {
            List Beta_G = Beta_H[g];
            List Bt_G_n = Bt_G[g];
            List Tm_Beta_L(L);
            for(int l = 0; l < L; ++l)
            {
              mat Beta_L = Beta_G[l];
              double Beta_est;
              if(intercept)
              {
                Beta_est = Beta_L(1,0);
              }else
              {
                Beta_est = Beta_L(0,0);
              }
              List Tm_Beta_n(n);
              for(int i = 0; i < n; ++i)
              {
                NumericVector Bt_H_i = Bt_H_n[i];
                NumericVector Bt_G_i = Bt_G_n[i];
                NumericMatrix Tm_Beta_ni(ni[i],ni_x[i]);
                mat temp_time_map = time_map[i];
                for(int jj = 0; jj < ni[i]; ++jj)
                {
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    Tm_Beta_ni(jj,j) = (Bt_G_i[jj]*Bt_H_i[j]*temp_time_map(jj,j)*Beta_est*y_Std_Error[l])/x_Std_Error[k];
                  }
                }  
                Tm_Beta_n[i] = Tm_Beta_ni;
              }
              Tm_Beta_L[l] = Tm_Beta_n;
            }
            Tm_Beta_G[g] = Tm_Beta_L;
          }
          Tm_Beta_H[h] = Tm_Beta_G;
        }
        Tm_Beta_Dk[d] = Tm_Beta_H;
      }
      Tm_Beta_C[k] = Tm_Beta_Dk;
    }else
    {
      Tm_Beta_C[k] = NA_REAL;
    }
  }
  if(Trace)
  {
    Rcout << "Estimation ends for Tm_Beta_C" << "\n";
  }

/*
  #---------------------------------------------------------------------------------- 
  # Date: 12/11/2020
  
  # It was realized that it makes more sense to show plots of beta on the standardized
  # scale rather than on the original scale. Therefore, along with Tm_Beta_C, I
  # have calculated Tm_Beta_Std_C in the following codes.

  # We added this in the list Beta_Estimate which is the output from this function.
  #----------------------------------------------------------------------------------
*/

  if(Trace)
  {
    Rcout << "Estimation begins for Tm_Beta_Std_C" << "\n";
  }

  List Tm_Beta_Std_C(K);
  for(int k = 0; k < K; ++k)
  {
    if(UseRaw[k])
    {
      List Beta_K = Beta[k];
      List Tm_Beta_Dk(Dk[k]);
      for(int d = 0; d < Dk[k]; ++d)
      {
        List Beta_Dk = Beta_K[d];
        List Tm_Beta_H(H);
        for(int h = 0; h < H; ++h)
        {
          List Beta_H = Beta_Dk[h];
          List Bt_H_n = Bt_H[h];
          List Tm_Beta_G(G);
          for(int g = 0; g < G; ++g)
          {
            List Beta_G = Beta_H[g];
            List Bt_G_n = Bt_G[g];
            List Tm_Beta_L(L);
            for(int l = 0; l < L; ++l)
            {
              mat Beta_L = Beta_G[l];
              double Beta_est;
              if(intercept)
              {
                Beta_est = Beta_L(1,0);
              }else
              {
                Beta_est = Beta_L(0,0);
              }
              List Tm_Beta_n(n);
              for(int i = 0; i < n; ++i)
              {
                NumericVector Bt_H_i = Bt_H_n[i];
                NumericVector Bt_G_i = Bt_G_n[i];
                NumericMatrix Tm_Beta_ni(ni[i],ni_x[i]);
                mat temp_time_map = time_map[i];
                for(int jj = 0; jj < ni[i]; ++jj)
                {
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    Tm_Beta_ni(jj,j) = (Bt_G_i[jj]*Bt_H_i[j]*temp_time_map(jj,j)*Beta_est);
                  }
                }  
                Tm_Beta_n[i] = Tm_Beta_ni;
              }
              Tm_Beta_L[l] = Tm_Beta_n;
            }
            Tm_Beta_G[g] = Tm_Beta_L;
          }
          Tm_Beta_H[h] = Tm_Beta_G;
        }
        Tm_Beta_Dk[d] = Tm_Beta_H;
      }
      Tm_Beta_Std_C[k] = Tm_Beta_Dk;
    }else
    {
      Tm_Beta_Std_C[k] = NA_REAL;
    }
  }

  if(Trace)
  {
    Rcout << "Estimation ends for Tm_Beta_Std_C" << "\n";
  }


  List Data = List::create(
    _["Org_x"] = Org_x, 
    _["Org_y"] = Org_y,
    _["id"] = id,
    _["tm"] = tm,
    _["x"] = x, 
    _["y"] = y,
    _["id_x"] = id_x,
    _["tm_x"] = tm_x,
    _["x_Mean"] = x_Mean,
    _["x_Std_Error"] = x_Std_Error,
    _["y_Mean"] = y_Mean,
    _["y_Std_Error"] = y_Std_Error,
    _["y_Mean_mu_zero"] = y_Mean_mu_zero
  );
  List Dimensions = List::create(
    _["n"] = n,
    _["K"] = K,
    _["L"] = L,
    _["H"] = H,
    _["G"] = G,
    _["Dk"] = Dk,
    _["ni"] = ni,
    _["ni_x"] = ni_x,
    _["N"] = N,
    _["N_x"] = N_x
  );
  
  List Index = List::create(
    _["unq_id"] = unq_id,
    _["unq_tm"] = unq_tm,
    _["unq_tm_x"] = unq_tm_x,
    _["unq_x"] = unq_x,
    _["id_index"] = id_index,
    _["id_index_x"] = id_index_x,
    _["tm_index"] = tm_index,
    _["tm_index_x"] = tm_index_x,
    _["x_index"] = x_index
  );
  
  List BS = List::create(
    _["Bt"] = Bt,
    _["Bt_x"] = Bt_x,
    _["Bx"] = Bx,
    _["Bt_H"] = Bt_H,
    _["Bt_G"] = Bt_G,
    _["Bx_K"] = Bx_K,
    _["Bxt"] = Bxt,
    _["Bxt_time_map"] = Bxt_time_map,
    _["Byt_xt_time_map"] = Byt_xt_time_map,
    _["time_map"] = time_map,
    _["Bx_Scale"] = Bx_Scale
  );
  
  List Regulate = List::create(
    _["M"] = M,
    _["nu"] = nu,
    _["Lambda_Ridge_Vec"] = Lambda_Ridge_Vec,
    _["Shrink"] = Shrink,
    _["Ridge_Penalty"] = Ridge_Penalty,
    _["Lambda_Scale"] = Lambda_Scale,
    _["NLambda"] = NLambda,
    _["lower_perc"] = lower_perc,
    _["upper_perc"] = upper_perc
  );
  
  List Beta_Estimate = List::create(
    _["Beta"] = Beta,
    _["Beta_Hat_List"] = Beta_Hat_List,
    _["line_search_est"] = line_search_est,
    _["Beta_Hat_List_Iter"] = Beta_Hat_List_Iter,
    _["Sum_Beta_Hat_List"] = Sum_Beta_Hat_List,
    _["List_Mat_Sum_Beta_Hat_iter"] = List_Mat_Sum_Beta_Hat_iter,
    _["Tm_Beta_C"] = Tm_Beta_C,
    _["Tm_Beta_Std_C"] = Tm_Beta_Std_C,
    _["negative_gradient"] = negative_gradient,
    _["list_linear_pred"] = list_linear_pred,
    _["list_mu_std"] = list_mu_std
  );
  
  return List::create(
    _["Data"] = Data,
    _["Dimensions"] = Dimensions,
    _["Index"] = Index,
    _["BS"] = BS,
    _["Regulate"] = Regulate,
    _["Beta_Estimate"] = Beta_Estimate,
    _["Error_Rate"] = Error_Rate,
    _["Variable_Select"] = Variable_Select,
    _["Response_Select"] = Response_Select,
    _["mu"] = mu,
    _["mu_List"] = mu_List,
    _["Phi"] = Phi,
    _["Rho"] = Rho,
    _["Lambda_List"] = Lambda_List,
    _["mu_zero"] = mu_zero,
    _["Vec_zero"] = Vec_zero
  );
  if(Trace){
    Rcout << "Completed BoostMLR execution step" << "\n";
  }
}




// [[Rcpp::export]]
List update_BoostMLR_C(NumericMatrix Org_x,
                       NumericMatrix Org_y,
                       NumericVector id,
                       NumericVector tm,
                       NumericMatrix x,
                       NumericMatrix y,
                       NumericVector x_Mean,
                       NumericVector x_Std_Error,
                       NumericVector y_Mean,
                       NumericVector y_Std_Error,
                       int n,
                       int K,
                       int L,
                       int H,
                       IntegerVector Dk,
                       IntegerVector ni,
                       int N,
                       NumericVector unq_id,
                       NumericVector unq_tm,
                       List unq_x,
                       List id_index,
                       List tm_index,
                       List x_index,
                       NumericMatrix Bt,
                       List Bx,
                       List Bt_H,
                       List Bx_K,
                       List Bxt,
                       List Bx_Scale,
                       double nu,
                       int M,
                       int M_New,
                       LogicalVector UseRaw,
                       bool Shrink,
                       bool Ridge_Penalty,
                       NumericVector Lambda_Ridge_Vec,
                       double Lambda_Scale,
                       int NLambda,
                       double lower_perc,
                       double upper_perc,
                       List Lambda_List,
                       NumericMatrix mu,
                       List mu_List,
                       NumericMatrix mu_zero,
                       NumericVector Vec_zero,
                       NumericMatrix Error_Rate,
                       IntegerMatrix Variable_Select,
                       IntegerMatrix Response_Select,
                       List Beta_Hat_List,
                       List Sum_Beta_Hat_List,
                       List Beta,
                       List Beta_Hat_List_Iter,
                       List lower_Beta_Hat_Noise,
                       List upper_Beta_Hat_Noise,
                       List List_Trace_Bxt_gm,
                       bool Mod_Grad,
                       bool VarFlag,
                       NumericVector phi,
                       NumericVector rho,
                       NumericMatrix Phi,
                       NumericMatrix Rho,
                       bool setting_seed,
                       unsigned long int seed_value,
                       bool Verbose)
{
  
  NumericMatrix UP_Error_Rate(M_New,L);
  NumericMatrix UP_Phi(M_New,L);
  NumericMatrix UP_Rho(M_New,L);
  IntegerMatrix UP_Variable_Select(M_New,H);
  IntegerMatrix UP_Response_Select(M_New,H);
  List UP_Beta_Hat_List(M_New);
  List UP_Sum_Beta_Hat_List(M_New);
  List UP_Beta_Hat_List_Iter(M_New);
  List UP_mu_List(M_New);
  List UP_Lambda_List(M_New);
  List UP_List_Trace_Bxt_gm(M_New);
  
  for(int m = 0; m < M_New; ++m){
    if(m < M){
      for(int h = 0; h < H; ++h){
        int Int_Variable_Select;
        Int_Variable_Select = Variable_Select(m,h);
        int Int_Response_Select;
        Int_Response_Select = Response_Select(m,h);
        UP_Variable_Select(m,h) = Int_Variable_Select;
        UP_Response_Select(m,h) = Int_Response_Select;
      }
    }else
    {
      for(int h = 0; h < H; ++h){
        UP_Variable_Select(m,h) = NA_INTEGER;
        UP_Response_Select(m,h) = NA_INTEGER;
      }
    }
  }
  
  for(int m = 0; m < M_New; ++m){
    
    if(m < M){
      UP_Beta_Hat_List[m] = Beta_Hat_List[m];
      UP_Sum_Beta_Hat_List[m] = Sum_Beta_Hat_List[m];
      UP_Beta_Hat_List_Iter[m] = Beta_Hat_List_Iter[m];
      UP_mu_List[m] = mu_List[m];
      UP_Lambda_List[m] = Lambda_List[m];
      UP_List_Trace_Bxt_gm[m] = List_Trace_Bxt_gm[m];
      for(int l = 0; l < L; ++l){
        UP_Error_Rate(m,l) = Error_Rate(m,l);
        UP_Phi(m,l) = Phi(m,l);
        UP_Rho(m,l) = Rho(m,l);
      }
    }else
    {
      UP_Beta_Hat_List[m] = NA_REAL;
      UP_Sum_Beta_Hat_List[m] = NA_REAL;
      UP_Beta_Hat_List_Iter[m] = NA_REAL;
      UP_mu_List[m] = NA_REAL;
      UP_Lambda_List[m] = NA_REAL;
      UP_List_Trace_Bxt_gm[m] = List_Trace_Bxt_gm;
      for(int l = 0; l < L; ++l){
        UP_Error_Rate(m,l) = NA_REAL;
        UP_Phi(m,l) = NA_REAL;
        UP_Rho(m,l) = NA_REAL;
      }
    }
  }
  
  Variable_Select = UP_Variable_Select;
  Response_Select = UP_Response_Select;
  Beta_Hat_List = UP_Beta_Hat_List;
  Sum_Beta_Hat_List = UP_Sum_Beta_Hat_List;
  Beta_Hat_List_Iter = UP_Beta_Hat_List_Iter;
  mu_List = UP_mu_List;
  Lambda_List = UP_Lambda_List;
  List_Trace_Bxt_gm = UP_List_Trace_Bxt_gm;
  Error_Rate = UP_Error_Rate;
  Phi = UP_Phi;
  Rho = UP_Rho;
  List Beta_Hat_Old(K);
  Beta_Hat_Old = Beta;
  List V_inv(L);
  
  for(int m = M; m <  M_New; ++m){
    
    if(Verbose){
      if( (m + 1 - M)%((M_New - M)/10) == 0.0){
        double FracBoosting;
        FracBoosting = ((m + 1 - M)*100)/(M_New - M);
        int Perc_boosting = FracBoosting;
         Rcout << Perc_boosting << "%" << " ";
      }
    }
    
    if(m == M || VarFlag == TRUE){
      for(int l = 0; l < L; ++l){
        List Vi_inv(n);
        for(int i = 0; i < n;++i){
          arma::mat Vi_inv_Temp;
          Vi_inv_Temp = MatrixInversion_Equicorrelation_C(ni[i],phi[l],rho[l]);
          Vi_inv[i] = Vi_inv_Temp;
        }
        V_inv[l] = Vi_inv;
      }
    }
    
    List gm(L);
    for(int l = 0; l < L; ++l){
      List gm_n(n);
      List V_inv_l = V_inv[l];
      for(int i = 0; i < n; ++i){
        arma::mat V_inv_i = V_inv_l[i];
        IntegerVector id_index_Temp = id_index[i];
        NumericVector gm_ni(ni[i]);
        for(int j = 0; j < ni[i]; ++j){
          gm_ni[j] = y(id_index_Temp[j],l) - mu(id_index_Temp[j],l);
        }
        if(Mod_Grad){
          gm_n[i] = gm_ni;  
        }
        else {
          gm_n[i] = Matrix_Vector_Multiplication_C(V_inv_i,gm_ni);
        }
      }
      gm[l] = gm_n;
    }
    
    List Temp_Trace_Bxt_gm(K);
    List Lambda(K);
    List Beta_Hat(K);
    if(m > 0){
      for(int k = 0; k < K; ++k){
        List Beta_Hat_Old_Temp_Dk = Beta_Hat_Old[k];
        List Bxt_Temp_K = Bxt[k];
        NumericVector Bx_Scale_K = Bx_Scale[k];
        List Lambda_Dk(Dk[k]);
        List Beta_Hat_Dk(Dk[k]);
        List Temp_Trace_Bxt_gm_Dk(Dk[k]);
        for(int d = 0; d < Dk[k]; ++d){
          List Beta_Hat_Old_Temp_H = Beta_Hat_Old_Temp_Dk[d];
          List Bxt_Temp_Dk = Bxt_Temp_K[d];
          List Lambda_H(H);
          List Beta_Hat_H(H);
          List Temp_Trace_Bxt_gm_H(H);
          for(int h = 0; h < H; ++h){
            NumericVector Beta_Hat_Old_Temp_L = Beta_Hat_Old_Temp_H[h];
            List Bxt_Temp_H = Bxt_Temp_Dk[h];
            NumericVector Lambda_L(L);
            NumericVector Beta_Hat_L(L);
            NumericVector Temp_Trace_Bxt_gm_L(L);
            for(int l = 0; l < L; ++l){
              double Beta_Hat_Old_Temp = Beta_Hat_Old_Temp_L[l];
              if(Beta_Hat_Old_Temp == 0.0){
                Beta_Hat_L[l] = 0.0;
              } else
              {
                List V_inv_l = V_inv[l];
                List gm_Temp_L = gm[l];
                NumericVector Bxt_n_2(n);
                NumericVector Bxt_gm_n(n);
                NumericVector gm_n_2(n);
                for(int i = 0; i < n; ++i){
                  arma::mat V_inv_i = V_inv_l[i];
                  NumericVector Bxt_Temp_n = Bxt_Temp_H[i];
                  NumericVector gm_Temp_n = gm_Temp_L[i];
                  NumericVector Bxt_V = Matrix_Vector_Multiplication_C(V_inv_i,Bxt_Temp_n);
                  NumericVector Bxt_ni_2(ni[i]);
                  NumericVector Bxt_gm_ni(ni[i]);
                  NumericVector gm_ni_2(ni[i]);
                  for(int j = 0; j < ni[i]; ++j){
                    Bxt_ni_2[j] = Bxt_V[j] * Bxt_Temp_n[j];
                    Bxt_gm_ni[j] = Bxt_V[j] * gm_Temp_n[j];
                    gm_ni_2[j] = gm_Temp_n[j] * gm_Temp_n[j];
                  }
                  Bxt_n_2[i] = Sum_C_NA(Bxt_ni_2);
                  Bxt_gm_n[i] = Sum_C_NA(Bxt_gm_ni);
                  gm_n_2[i] = Sum_C_NA(gm_ni_2);
                }
                double Trace_Bxt    = Sum_C_NA(Bxt_n_2);
                double Trace_Bxt_gm = Sum_C_NA(Bxt_gm_n);
                Temp_Trace_Bxt_gm_L[l] = Trace_Bxt_gm;
                if(Trace_Bxt < 0.001 ){
                  Beta_Hat_L[l] = 0.0;
                }else
                {
                  if(Ridge_Penalty){
                    /* 
                     Come back to this later since I don't know how to calculate
                     (Vi)^{-1/2} require for calculating lambda for Ridge penalty.
                     Therefore, default setting Ridge_Penalty = FALSE should not be changed.
                     Trace_gm specified below will be incorrect because it does not include Vi_inv matrix.
                     */
                    double Trace_gm       = Sum_C_NA(gm_n_2);
                    double Beta_Hat_NL    = Beta_Hat_Old_Temp;
                    double Trace_eps      = Trace_gm + (Beta_Hat_NL * Beta_Hat_NL * Trace_Bxt) - (2 * Beta_Hat_NL * Trace_Bxt_gm);
                    double Lambda_L_Temp  = (Trace_Bxt/(Trace_gm - Trace_eps))*Lambda_Scale;
                    if(Lambda_L_Temp > 5000){
                      Lambda_L[l] = 5000;
                    }else
                    {
                      if(Lambda_L_Temp < 0){
                        Lambda_L[l] = Lambda_Ridge_Vec[k];
                      }else
                      {
                        Lambda_L[l] = Lambda_L_Temp;  
                      }
                    }
                  }else
                  {
                    Lambda_L[l]         = Lambda_Ridge_Vec[k];   
                  }
                  Beta_Hat_L[l]         = (Trace_Bxt_gm/(Trace_Bxt + Lambda_L[l]))/Bx_Scale_K[d];
                }
              }
            }
            Lambda_H[h] = Lambda_L;
            Beta_Hat_H[h] = Beta_Hat_L; 
            Temp_Trace_Bxt_gm_H[h] = Temp_Trace_Bxt_gm_L;
          }
          Lambda_Dk[d] = Lambda_H;
          Beta_Hat_Dk[d] = Beta_Hat_H;
          Temp_Trace_Bxt_gm_Dk[d] = Temp_Trace_Bxt_gm_H;
        }
        Lambda[k] = Lambda_Dk;
        Beta_Hat[k] = Beta_Hat_Dk;
        Temp_Trace_Bxt_gm[k] = Temp_Trace_Bxt_gm_Dk;
      }
    }else
    {
      for(int k = 0; k < K; ++k){
        List Bxt_Temp_K = Bxt[k];
        NumericVector Bx_Scale_K = Bx_Scale[k];
        List Lambda_Dk(Dk[k]);
        List Beta_Hat_Dk(Dk[k]);
        List Temp_Trace_Bxt_gm_Dk(Dk[k]);
        for(int d = 0; d < Dk[k]; ++d){
          List Bxt_Temp_Dk = Bxt_Temp_K[d];
          List Lambda_H(H);
          List Beta_Hat_H(H);
          List Temp_Trace_Bxt_gm_H(H);
          for(int h = 0; h < H; ++h){
            List Bxt_Temp_H = Bxt_Temp_Dk[h];
            NumericVector Lambda_L(L);
            NumericVector Beta_Hat_L(L);
            NumericVector Temp_Trace_Bxt_gm_L(L);
            for(int l = 0; l < L; ++l){
              List V_inv_l = V_inv[l];
              List gm_Temp_L = gm[l];
              NumericVector Bxt_n_2(n);
              NumericVector Bxt_gm_n(n);
              NumericVector gm_n_2(n);
              for(int i = 0; i < n; ++i){
                arma::mat V_inv_i = V_inv_l[i];
                NumericVector Bxt_Temp_n = Bxt_Temp_H[i];
                NumericVector gm_Temp_n = gm_Temp_L[i];
                NumericVector Bxt_V = Matrix_Vector_Multiplication_C(V_inv_i,Bxt_Temp_n);
                NumericVector Bxt_ni_2(ni[i]);
                NumericVector Bxt_gm_ni(ni[i]);
                NumericVector gm_ni_2(ni[i]);
                for(int j = 0; j < ni[i]; ++j){
                  Bxt_ni_2[j] = Bxt_V[j] * Bxt_Temp_n[j];
                  Bxt_gm_ni[j] = Bxt_V[j] * gm_Temp_n[j];
                  gm_ni_2[j] = gm_Temp_n[j] * gm_Temp_n[j];
                }
                Bxt_n_2[i] = Sum_C_NA(Bxt_ni_2);
                Bxt_gm_n[i] = Sum_C_NA(Bxt_gm_ni);
                gm_n_2[i] = Sum_C_NA(gm_ni_2);
              }
              double Trace_Bxt    = Sum_C_NA(Bxt_n_2);
              double Trace_Bxt_gm = Sum_C_NA(Bxt_gm_n);
              Temp_Trace_Bxt_gm_L[l] = Trace_Bxt_gm;
              if(Trace_Bxt < 0.001 ){
                Beta_Hat_L[l] = 0.0;
              }else
              {
                if(Ridge_Penalty){
                  /* 
                   Come back to this later since I don't know how to calculate
                   (Vi)^{-1/2} require for calculating lambda for Ridge penalty.
                   Therefore, default setting Ridge_Penalty = FALSE should not be changed.
                   Trace_gm specified below will be incorrect because it does not include Vi_inv matrix.
                   */
                  double Trace_gm       = Sum_C_NA(gm_n_2);
                  double Beta_Hat_NL    = Trace_Bxt_gm/(Trace_Bxt);
                  double Trace_eps      = Trace_gm + (Beta_Hat_NL * Beta_Hat_NL * Trace_Bxt) - (2 * Beta_Hat_NL * Trace_Bxt_gm);
                  double Lambda_L_Temp  = (Trace_Bxt/(Trace_gm - Trace_eps))*Lambda_Scale;
                  if(Lambda_L_Temp > 5000){
                    Lambda_L[l] = 5000;
                  }else
                  {
                    if(Lambda_L_Temp < 0){
                      Lambda_L[l] = Lambda_Ridge_Vec[k];
                    }else
                    {
                      Lambda_L[l] = Lambda_L_Temp;  
                    }
                  }
                }else
                {
                  Lambda_L[l]         = Lambda_Ridge_Vec[k];
                }
                Beta_Hat_L[l]         = (Trace_Bxt_gm/(Trace_Bxt + Lambda_L[l]))/Bx_Scale_K[d]; 
              }
            }
            Lambda_H[h] = Lambda_L;
            Beta_Hat_H[h] = Beta_Hat_L; 
            Temp_Trace_Bxt_gm_H[h] = Temp_Trace_Bxt_gm_L;
          }
          Lambda_Dk[d] = Lambda_H;
          Beta_Hat_Dk[d] = Beta_Hat_H;
          Temp_Trace_Bxt_gm_Dk[d] = Temp_Trace_Bxt_gm_H;
        }
        Lambda[k] = Lambda_Dk;
        Beta_Hat[k] = Beta_Hat_Dk;
        Temp_Trace_Bxt_gm[k] = Temp_Trace_Bxt_gm_Dk;
      } 
    }
    
    List_Trace_Bxt_gm[m] = Temp_Trace_Bxt_gm;
    Lambda_List[m] = Lambda;
    
    if(Shrink){
      if(m == 0){
        for(int k = 0; k < K; ++k){
          List Bxt_Temp_K = Bxt[k];
          NumericVector Bx_Scale_K = Bx_Scale[k];
          List lower_Beta_Hat_Dk(Dk[k]);
          List upper_Beta_Hat_Dk(Dk[k]);
          for(int d = 0; d < Dk[k]; ++d){
            List Bxt_Temp_Dk = Bxt_Temp_K[d];
            List lower_Beta_Hat_H(H);
            List upper_Beta_Hat_H(H);
            for(int h = 0; h < H; ++h){
              List Bxt_Temp_H = Bxt_Temp_Dk[h];
              NumericVector lower_Beta_Hat_L(L);
              NumericVector upper_Beta_Hat_L(L);
              for(int l = 0; l < L; ++l){
                List V_inv_l = V_inv[l];
                NumericVector Beta_Hat_Noise_Temp(NLambda);
                for(int l_m = 0; l_m < NLambda; ++l_m){
                  List gm_Temp_L = gm[l];
                  NumericVector gm_unlist(N);
                  int count = 0;
                  for(int i = 0; i < n; ++i){
                    NumericVector gm_Temp_n = gm_Temp_L[i];
                    for(int j = 0; j < ni[i]; ++j){
                      gm_unlist[count] = gm_Temp_n[j];
                      count = count + 1;
                    }
                  }
                  seed_value = seed_value + add_seed;
                  NumericVector gm_unlist_noise = randomShuffle(gm_unlist, N,setting_seed,seed_value, false, R_NilValue);
                  count = 0;
                  List gm_noise_n(n);
                  for(int i = 0; i < n; ++i){
                    NumericVector gm_noise_ni(ni[i]);
                    for(int j = 0; j < ni[i]; ++j){
                      gm_noise_ni[j] = gm_unlist_noise[count];
                      count = count + 1;
                    }
                    gm_noise_n[i] = gm_noise_ni;
                  }
                  NumericVector Bxt_n_2(n);
                  NumericVector Bxt_gm_n(n);
                  NumericVector gm_noise_n_2(n);
                  for(int i = 0; i < n; ++i){
                    arma::mat V_inv_i = V_inv_l[i];
                    NumericVector Bxt_Temp_n = Bxt_Temp_H[i];
                    NumericVector gm_noise_Temp_n = gm_noise_n[i];
                    NumericVector Bxt_V = Matrix_Vector_Multiplication_C(V_inv_i,Bxt_Temp_n);
                    NumericVector Bxt_ni_2(ni[i]);
                    NumericVector Bxt_gm_ni(ni[i]);
                    NumericVector gm_noise_ni_2(ni[i]);
                    for(int j = 0; j < ni[i]; ++j){
                      Bxt_ni_2[j] = Bxt_V[j] * Bxt_Temp_n[j];
                      Bxt_gm_ni[j] = Bxt_V[j] * gm_noise_Temp_n[j];
                      gm_noise_ni_2[j] = gm_noise_Temp_n[j] * gm_noise_Temp_n[j];
                    }
                    Bxt_n_2[i] = Sum_C_NA(Bxt_ni_2);
                    Bxt_gm_n[i] = Sum_C_NA(Bxt_gm_ni);
                    gm_noise_n_2[i] = Sum_C_NA(gm_noise_ni_2);
                  }
                  double Trace_Bxt    = Sum_C_NA(Bxt_n_2);
                  double Trace_Bxt_gm = Sum_C_NA(Bxt_gm_n);
                  if(Trace_Bxt == 0.0){
                    Beta_Hat_Noise_Temp[l_m]  = 0.0;
                  }else
                  {
                    if(Ridge_Penalty){
                      /* 
                       Come back to this later since I don't know how to calculate
                       (Vi)^{-1/2} require for calculating lambda for Ridge penalty.
                       Therefore, default setting Ridge_Penalty = FALSE should not be changed.
                       Trace_gm specified below will be incorrect because it does not include Vi_inv matrix.
                       */
                      double Trace_gm       = Sum_C_NA(gm_noise_n_2);
                      double Beta_Hat_NL    = Trace_Bxt_gm/(Trace_Bxt);
                      double Trace_eps      = Trace_gm + (Beta_Hat_NL * Beta_Hat_NL * Trace_Bxt) - (2 * Beta_Hat_NL * Trace_Bxt_gm);
                      double Lambda_L_Temp  = (Trace_Bxt/(Trace_gm - Trace_eps))*Lambda_Scale;
                      if(Lambda_L_Temp > 5000){
                        Lambda_L_Temp = 5000;
                      }else
                      {
                        if(Lambda_L_Temp < 0){
                          Lambda_L_Temp = Lambda_Ridge_Vec[k];
                        } //else
                          // {
                          // Lambda_L_Temp = Lambda_L_Temp;  
                          // }
                      }
                      Beta_Hat_Noise_Temp[l_m]         = (Trace_Bxt_gm/(Trace_Bxt + Lambda_L_Temp))/Bx_Scale_K[d];
                    }else
                    {
                      Beta_Hat_Noise_Temp[l_m]         = (Trace_Bxt_gm/(Trace_Bxt + Lambda_Ridge_Vec[k]))/Bx_Scale_K[d]; 
                    }
                  }
                }
                NumericVector sort_Beta_Hat_Noise = stl_sort_NA(Beta_Hat_Noise_Temp);
                int lower_range = (NLambda*lower_perc);
                int upper_range = (NLambda*upper_perc);
                lower_Beta_Hat_L[l] = sort_Beta_Hat_Noise[lower_range];
                upper_Beta_Hat_L[l] = sort_Beta_Hat_Noise[upper_range];
              }
              lower_Beta_Hat_H[h] = lower_Beta_Hat_L;
              upper_Beta_Hat_H[h] = upper_Beta_Hat_L;
            }
            lower_Beta_Hat_Dk[d] = lower_Beta_Hat_H;
            upper_Beta_Hat_Dk[d] = upper_Beta_Hat_H;
          }
          lower_Beta_Hat_Noise[k] = lower_Beta_Hat_Dk;
          upper_Beta_Hat_Noise[k] = upper_Beta_Hat_Dk;
        }
      }
      
      List Beta_Hat_New(K);
      for(int k = 0; k < K; ++k){
        List Beta_Hat_K = Beta_Hat[k];
        List lower_Beta_Hat_Noise_K = lower_Beta_Hat_Noise[k];
        List upper_Beta_Hat_Noise_K = upper_Beta_Hat_Noise[k];
        List Beta_Hat_New_Dk(Dk[k]);
        for(int d = 0; d < Dk[k]; ++d){
          List Beta_Hat_Dk = Beta_Hat_K[d];
          List lower_Beta_Hat_Noise_Dk = lower_Beta_Hat_Noise_K[d];
          List upper_Beta_Hat_Noise_Dk = upper_Beta_Hat_Noise_K[d];
          List Beta_Hat_New_H(H);
          for(int h = 0; h < H; ++h){
            NumericVector Beta_Hat_H = Beta_Hat_Dk[h];
            NumericVector lower_Beta_Hat_Noise_H = lower_Beta_Hat_Noise_Dk[h];
            NumericVector upper_Beta_Hat_Noise_H = upper_Beta_Hat_Noise_Dk[h];
            NumericVector Beta_Hat_New_L(L);
            for(int l = 0; l < L; ++l){
              double Beta_Hat_L = Beta_Hat_H[l];
              double lower_Beta_Hat_Noise_L = lower_Beta_Hat_Noise_H[l];
              double upper_Beta_Hat_Noise_L = upper_Beta_Hat_Noise_H[l];
              if( (Beta_Hat_L >=  lower_Beta_Hat_Noise_L) && (Beta_Hat_L <= upper_Beta_Hat_Noise_L) ){
                Beta_Hat_New_L[l] = 0.0;
              }else
              {
                Beta_Hat_New_L[l] = Beta_Hat_L;
              }
            }
            Beta_Hat_New_H[h] = Beta_Hat_New_L;
          }
          Beta_Hat_New_Dk[d] = Beta_Hat_New_H;
        }
        Beta_Hat_New[k] = Beta_Hat_New_Dk;
      }
      Beta_Hat = Beta_Hat_New;
    }
    
    Beta_Hat_List_Iter[m] = Beta_Hat;
    Beta_Hat_Old = Beta_Hat;
    
    List List_Mat_Sum_Beta_Hat(H);
    for(int h = 0; h < H; ++h){
      NumericMatrix Mat_Sum_Beta_Hat(K,L);
      for(int k = 0; k < K; ++k){
        List Beta_Hat_Temp_K = Beta_Hat[k]; //Temp_Trace_Bxt_gm[k];
        for(int l = 0; l < L; ++l){
          for(int d = 0; d < Dk[k]; ++d){
            double Mult_Factor = 1.0; //(Dk[k]*H);
            List Beta_Hat_Temp_Dk = Beta_Hat_Temp_K[d];
            NumericVector Beta_Hat_Temp_H = Beta_Hat_Temp_Dk[h];
            Mat_Sum_Beta_Hat(k,l) = Mat_Sum_Beta_Hat(k,l) + ((Beta_Hat_Temp_H[l]*Beta_Hat_Temp_H[l])/Mult_Factor);
          }
        }
      }
      List_Mat_Sum_Beta_Hat[h] = Mat_Sum_Beta_Hat;
    }
    
    LogicalVector Sum_Beta_Zero(H);
    for(int h = 0; h < H; ++h){
      NumericMatrix Mat_Sum_Beta_Hat = List_Mat_Sum_Beta_Hat[h];
      NumericVector Vec_Sum_Beta_Hat_K(K);
      for(int k = 0; k < K; ++k){
        NumericVector Vec_Sum_Beta_Hat_L(L);
        for(int l = 0; l < L; ++l){
          Vec_Sum_Beta_Hat_L[l] = Mat_Sum_Beta_Hat(k,l);
        }
        Vec_Sum_Beta_Hat_K[k] = Sum_C_NA(Vec_Sum_Beta_Hat_L);
      }
      Sum_Beta_Zero[h] = is_true(all(Vec_Sum_Beta_Hat_K == 0.0));
    }
    
    if(is_true(all(Sum_Beta_Zero)) ){
      M_New = (m-1);
      break;
    }
    
    Sum_Beta_Hat_List[m] = List_Mat_Sum_Beta_Hat;
    IntegerVector km_H(H);
    IntegerVector lm_H(H);
    for(int h = 0; h < H; ++h){
      NumericMatrix Mat_Sum_Beta_Hat = List_Mat_Sum_Beta_Hat[h];
      IntegerVector km_lm = Which_Max_Matrix_NA(Mat_Sum_Beta_Hat);
      km_H[h] = km_lm[0];
      lm_H[h] = km_lm[1];
      Variable_Select(m,h) = km_lm[0];
      Response_Select(m,h) = km_lm[1];
    }
    
    List Sum_Beta(K);
    for(int k = 0; k < K; ++k){
      List Beta_Hat_K = Beta_Hat[k];
      List Beta_K = Beta[k];
      List Sum_Beta_Dk(Dk[k]);
      for(int d = 0; d < Dk[k]; ++d){
        List Beta_Hat_Dk = Beta_Hat_K[d];
        List Beta_Dk = Beta_K[d];
        List Sum_Beta_H(H);
        for(int h = 0; h < H; ++h){
          NumericVector Beta_Hat_H = Beta_Hat_Dk[h];
          NumericVector Beta_H = Beta_Dk[h];
          NumericVector Sum_Beta_L(L);
          IntegerVector km_lm_Temp(2);
          km_lm_Temp[0] = km_H[h];
          km_lm_Temp[1] = lm_H[h];
          for(int l = 0; l < L; ++l){
            if(is_true(any( km_lm_Temp == -1 ))){
              Sum_Beta_L[l] = Beta_H[l];
            }else
            {
              if(k == km_H[h] && l == lm_H[h]){
                Sum_Beta_L[l] = Beta_H[l] + (nu*Beta_Hat_H[l]);  
              }else
              {
                Sum_Beta_L[l] = Beta_H[l];
              } 
            }
          }
          Sum_Beta_H[h] = Sum_Beta_L;
        }
        Sum_Beta_Dk[d] = Sum_Beta_H;
      }
      Sum_Beta[k] = Sum_Beta_Dk;
    }
    
    Beta = Sum_Beta;
    Beta_Hat_List[m] = Beta;
    
    mu = mu_zero;
    for(int k = 0; k < K; ++k){
      List Bxt_Temp_K = Bxt[k];
      List Beta_Temp_K = Beta[k];
      for(int d = 0; d < Dk[k]; ++d){
        List Bxt_Temp_Dk = Bxt_Temp_K[d];
        List Beta_Temp_Dk = Beta_Temp_K[d];
        for(int h = 0; h < H; ++h){
          List Bxt_Temp_H = Bxt_Temp_Dk[h];
          NumericVector Beta_Temp_H = Beta_Temp_Dk[h];
          NumericMatrix mu_Hat(N,L);
          for(int l = 0; l < L; ++l){
            for(int i = 0; i < n; ++i){
              IntegerVector id_index_Temp = id_index[i];
              NumericVector Bxt_Temp_n = Bxt_Temp_H[i];
              for(int j = 0; j < ni[i]; ++j){
                mu_Hat( id_index_Temp[j], l) = Bxt_Temp_n[j]*Beta_Temp_H[l];
              }
            }
          }
          mu = Matrix_Sum_C_NA(mu, mu_Hat);
        }
      }
    }
    
    NumericMatrix Org_mu(N,L);
    for(int l = 0; l < L; ++l){
      for(int i = 0; i < N; ++i){
        Org_mu(i,l) = ( mu(i,l) * y_Std_Error[l] ) + y_Mean[l];
      }
    }
    
    mu_List[m] = Org_mu;
    
    
    if(VarFlag){
      for(int l = 0; l < L; ++l){
        NumericVector Resid(N);
        for(int i = 0; i < N; ++i){
          Resid[i] = Org_y(i,l) - Org_mu(i,l); 
        }
        double phi_Temp;
        double phi_Init = 0.0;
        int phi_count = 0;
        for(int i = 0; i < n; ++i){
          IntegerVector id_index_Temp = id_index[i];
          for(int j = 0; j < ni[i]; ++j){
            phi_Init = phi_Init + (Resid[id_index_Temp[j]]*Resid[id_index_Temp[j]]);
            phi_count = phi_count + 1;  
          }
        }
        if(phi_count <= K){
          phi_Temp = phi_Init/phi_count;
        } else
        {
          phi_Temp = phi_Init/(phi_count - K);  
        }
        
        if(phi_Temp <= 0.0){
          phi[l] = 1.0;
        } else 
        {
          phi[l] = phi_Temp;
        }
        Phi(m,l) = phi[l];
        
        double rho_Temp;
        double rho_Init = 0.0;
        int rho_count = 0;
        for(int i = 0; i < n; ++i){
          IntegerVector id_index_Temp = id_index[i];
          if(ni[i] > 1){
            for(int j = 0; j < (ni[i]-1); ++j){
              for(int jj = (j+1); jj < ni[i]; ++jj){
                rho_Init = rho_Init + (Resid[id_index_Temp[j]]*Resid[id_index_Temp[jj]]);
                rho_count = rho_count + 1;  
              }
            }            
          }
        }   
        if(rho_count <= K){
          rho_Temp = rho_Init/(rho_count*phi[l]);
        } else
        {
          rho_Temp = rho_Init/((rho_count - K)*phi[l]);  
        }
        if(rho_Temp < -1 || rho_Temp > 1){
          rho[l] = 0.0;
        } else
        {
          rho[l] = rho_Temp;
        }
        Rho(m,l) = rho[l];
      }
    }
    
    
    for(int l = 0; l < L; ++l){
      NumericVector Org_y_Temp(N);
      NumericVector Org_mu_Temp(N);
      for(int i = 0; i < N; ++i){
        Org_y_Temp[i] = Org_y(i,l);
        Org_mu_Temp[i] = Org_mu(i,l);
      }
      Error_Rate(m,l) = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index); 
    }
    
  } 
  
  List Tm_Beta_C(K);
  for(int k = 0; k < K; ++k){
    if(UseRaw[k]){
      List Beta_K = Beta[k];
      List Tm_Beta_Dk(Dk[k]);
      for(int d = 0; d < Dk[k]; ++d){
        List Beta_Dk = Beta_K[d];
        List Tm_Beta_H(H);
        for(int h = 0; h < H; ++h){
          NumericVector Beta_H = Beta_Dk[h];
          List Bt_n = Bt_H[h];
          List Tm_Beta_L(L);
          for(int l = 0; l < L; ++l){
            List Tm_Beta_n(n);
            for(int i = 0; i < n; ++i){
              NumericVector Bt_i = Bt_n[i];
              NumericVector Tm_Beta_ni(ni[i]);
              for(int j = 0; j < ni[i]; ++j){
                Tm_Beta_ni[j] = (Bt_i[j]*Beta_H[l]*y_Std_Error[l])/x_Std_Error[k];
              }
              Tm_Beta_n[i] = Tm_Beta_ni;
            }
            Tm_Beta_L[l] = Tm_Beta_n;
          }
          Tm_Beta_H[h] = Tm_Beta_L;
        }
        Tm_Beta_Dk[d] = Tm_Beta_H;
      }
      Tm_Beta_C[k] = Tm_Beta_Dk;
    }else
    {
      Tm_Beta_C[k] = NA_REAL;
    }
  }


  /*
  #---------------------------------------------------------------------------------- 
  # Date: 12/11/2020
  
  # It was realized that it makes more sense to show plots of beta on the standardized
  # scale rather than on the original scale. Therefore, along with Tm_Beta_C, I
  # have calculated Tm_Beta_Std_C in the following codes.

  # We added this in the list Beta_Estimate which is the output from this function.
  #----------------------------------------------------------------------------------
*/

  List Tm_Beta_Std_C(K);
  for(int k = 0; k < K; ++k){
    if(UseRaw[k]){
      List Beta_K = Beta[k];
      List Tm_Beta_Dk(Dk[k]);
      for(int d = 0; d < Dk[k]; ++d){
        List Beta_Dk = Beta_K[d];
        List Tm_Beta_H(H);
        for(int h = 0; h < H; ++h){
          NumericVector Beta_H = Beta_Dk[h];
          List Bt_n = Bt_H[h];
          List Tm_Beta_L(L);
          for(int l = 0; l < L; ++l){
            List Tm_Beta_n(n);
            for(int i = 0; i < n; ++i){
              NumericVector Bt_i = Bt_n[i];
              NumericVector Tm_Beta_ni(ni[i]);
              for(int j = 0; j < ni[i]; ++j){
                Tm_Beta_ni[j] = Bt_i[j]*Beta_H[l];
              }
              Tm_Beta_n[i] = Tm_Beta_ni;
            }
            Tm_Beta_L[l] = Tm_Beta_n;
          }
          Tm_Beta_H[h] = Tm_Beta_L;
        }
        Tm_Beta_Dk[d] = Tm_Beta_H;
      }
      Tm_Beta_Std_C[k] = Tm_Beta_Dk;
    }else
    {
      Tm_Beta_Std_C[k] = NA_REAL;
    }
  }
  
  List Data = List::create(
    _["Org_x"] = Org_x, 
    _["Org_y"] = Org_y,
    _["id"] = id,
    _["tm"] = tm,
    _["x"] = x, 
    _["y"] = y,
    _["x_Mean"] = x_Mean,
    _["x_Std_Error"] = x_Std_Error,
    _["y_Mean"] = y_Mean,
    _["y_Std_Error"] = y_Std_Error
  );
  List Dimensions = List::create(
    _["n"] = n,
    _["K"] = K,
    _["L"] = L,
    _["H"] = H,
    _["Dk"] = Dk,
    _["ni"] = ni,
    _["N"] = N
  );
  
  List Index = List::create(
    _["unq_id"] = unq_id,
    _["unq_tm"] = unq_tm,
    _["unq_x"] = unq_x,
    _["id_index"] = id_index,
    _["tm_index"] = tm_index,
    _["x_index"] = x_index
  );
  
  List BS = List::create(
    _["Bt"] = Bt,
    _["Bx"] = Bx,
    _["Bt_H"] = Bt_H,
    _["Bx_K"] = Bx_K,
    _["Bxt"] = Bxt,
    _["Bx_Scale"] = Bx_Scale
  );
  
  List Regulate = List::create(
    _["M"] = M_New,
    _["nu"] = nu,
    _["Lambda_Ridge_Vec"] = Lambda_Ridge_Vec,
    _["Shrink"] = Shrink,
    _["Ridge_Penalty"] = Ridge_Penalty,
    _["Lambda_Scale"] = Lambda_Scale,
    _["NLambda"] = NLambda,
    _["lower_perc"] = lower_perc,
    _["upper_perc"] = upper_perc
  );
  
  List Beta_Estimate = List::create(
    _["Beta"] = Beta,
    _["Beta_Hat_List"] = Beta_Hat_List,
    _["Beta_Hat_List_Iter"] = Beta_Hat_List_Iter,
    _["Sum_Beta_Hat_List"] = Sum_Beta_Hat_List,
    _["Tm_Beta_C"] = Tm_Beta_C,
    _["Tm_Beta_Std_C"] = Tm_Beta_Std_C,
    _["lower_Beta_Hat_Noise"] = lower_Beta_Hat_Noise,
    _["upper_Beta_Hat_Noise"] = upper_Beta_Hat_Noise,
    _["List_Trace_Bxt_gm"] = List_Trace_Bxt_gm
  );
  
  return List::create(
    _["Data"] = Data,
    _["Dimensions"] = Dimensions,
    _["Index"] = Index,
    _["BS"] = BS,
    _["Regulate"] = Regulate,
    _["Beta_Estimate"] = Beta_Estimate,
    _["Error_Rate"] = Error_Rate,
    _["Variable_Select"] = Variable_Select,
    _["Response_Select"] = Response_Select,
    _["mu"] = mu,
    _["mu_List"] = mu_List,
    _["Phi"] = Phi,
    _["Rho"] = Rho,
    _["Lambda_List"] = Lambda_List,
    _["mu_zero"] = mu_zero,
    _["Vec_zero"] = Vec_zero
  );
}

// [[Rcpp::export]]
List predict_BoostMLR_C(NumericMatrix Org_x,
                        NumericVector tm,
                        NumericVector id,
                        NumericMatrix Org_y,
                        NumericVector id_x,
                        NumericVector tm_x,
                        NumericVector x_Mean,
                        NumericVector x_Std_Error,
                        NumericVector y_Mean,
                        NumericVector y_Std_Error,
                        bool intercept,
                        String y_scale_summary,
                        List time_map,
                        int K,
                        int L,
                        int H,
                        int G,
                        IntegerVector Dk,
                        NumericVector unq_id,
                        NumericVector unq_tm,
                        NumericVector unq_tm_x,
                        List unq_x,
                        NumericMatrix Bt,
                        NumericMatrix Bt_x,
                        List Bx,
                        LogicalVector UseRaw,
                        NumericMatrix Time_Add_New,
                        LogicalVector Time_Unmatch,
                        List Beta,
                        List Beta_Hat_List,
                        bool testFlag,
                        int M,
                        double nu,
                        bool Time_Varying,
                        bool vimpFlag,
                        bool vimpFlag_Coef,
                        double eps,
                        bool setting_seed,
                        unsigned long int seed_value)
{                           
  
  int n = unq_id.size();
  
  List id_index(n);
  for(int i = 0; i < n; ++i)
  {
    id_index[i] = Which_C_NA(unq_id[i],id);
  }
  
  List id_index_x(n);
  for(int i = 0; i < n; ++i)
  {
    id_index_x[i] = Which_C_NA(unq_id[i],id_x);
  }

  IntegerVector ni(n);
  for(int i = 0; i < n; ++i)
  {
    IntegerVector id_index_Temp = id_index[i];
    ni[i] = id_index_Temp.size();
  }
  
  IntegerVector ni_x(n);
  for(int i = 0; i < n; ++i)
  {
    IntegerVector id_index_Temp = id_index_x[i];
    ni_x[i] = id_index_Temp.size();
  }

  int N = Org_y.nrow();
  int N_x = Org_x.nrow();
  
  IntegerVector unlist_id_index(N);
  int count = 0;
  for(int i = 0; i < n; ++i){
    NumericVector id_index_Temp = id_index[i];
    for(int j = 0; j < ni[i]; ++j){
      unlist_id_index[count] = id_index_Temp[j];
      count = count + 1;
    }
  }
  
  IntegerVector unlist_id_index_x(N_x);
  count = 0;
  for(int i = 0; i < n; ++i){
    NumericVector id_index_Temp = id_index_x[i];
    for(int j = 0; j < ni_x[i]; ++j){
      unlist_id_index_x[count] = id_index_Temp[j];
      count = count + 1;
    }
  }

  NumericVector id_New(N);
  for(int ii = 0; ii < N; ++ii){
    id_New[ii] = id[ unlist_id_index[ii] ];
  }

  NumericVector id_New_x(N_x);
  for(int ii = 0; ii < N_x; ++ii){
    id_New_x[ii] = id_x[ unlist_id_index_x[ii] ];
  }

  
  LogicalVector id_Match(N);
  for(int ii = 0; ii < N; ++ii){
    id_Match[ii] = (id[ii] == id_New[ii]);
  }
  
  LogicalVector id_Match_x(N_x);
  for(int ii = 0; ii < N_x; ++ii){
    id_Match_x[ii] = (id_x[ii] == id_New_x[ii]);
  }

  if(is_false(all( id_Match == TRUE) )){
    NumericVector tm_New(N);
    NumericMatrix Org_y_New(N,L);
    for(int ii = 0; ii < N; ++ii){
      tm_New[ii] = tm[ unlist_id_index[ii]  ];
      id_New[ii] = id[ unlist_id_index[ii]  ];
      for(int l = 0; l < L; ++l){
        Org_y_New(ii,l) = Org_y( unlist_id_index[ii], l  );
      }
    }
    tm = tm_New;
    id = id_New;
    Org_y = Org_y_New;  
  }


  if(is_false(all( id_Match_x == TRUE) )){
    NumericVector tm_New_x(N_x);
    NumericMatrix Org_x_New_x(N_x,K);
    for(int ii = 0; ii < N_x; ++ii){
      tm_New_x[ii] = tm_x[ unlist_id_index_x[ii]  ];
      id_New_x[ii] = id_x[ unlist_id_index_x[ii]  ];
      for(int k = 0; k < K; ++k){
        Org_x_New_x(ii,k) = Org_x( unlist_id_index_x[ii], k  );
      }
    }
    tm_x = tm_New_x;
    id_x = id_New_x;
    Org_x = Org_x_New_x;
  }

  List tm_index(n);
  for(int i = 0; i < n; ++i){
    NumericVector tm_Temp(ni[i]);
    IntegerVector id_index_Temp = id_index[i];
    for(int j = 0; j < ni[i]; ++j){
      tm_Temp[j] = tm[ id_index_Temp[j] ];
    }
    tm_index[i] = Approx_Match_C_NA(tm_Temp,unq_tm);
  }

  List tm_index_x(n);
  for(int i = 0; i < n; ++i){
    NumericVector tm_Temp(ni_x[i]);
    IntegerVector id_index_Temp = id_index_x[i];
    for(int j = 0; j < ni_x[i]; ++j){
      tm_Temp[j] = tm_x[ id_index_Temp[j] ];
    }
    tm_index_x[i] = Approx_Match_C_NA(tm_Temp,unq_tm_x);
  }

  List Bt_G(G);
  for(int g = 0; g < G; ++g)
  {
    List Bt_n(n);
    for(int i = 0; i < n; ++i)
    {
      IntegerVector tm_index_Temp = tm_index[i];
      NumericVector Bt_ni(ni[i]);
      for(int j = 0; j < ni[i]; ++j)
      {
        if(IntegerVector::is_na(tm_index_Temp[j]))
        {
          Bt_ni[j] = NA_REAL;
        }else
        {
          Bt_ni[j] = Bt(tm_index_Temp[j], g);          
        }
      }
      Bt_n[i] = Bt_ni;
    }
    Bt_G[g] = Bt_n;
  }

  List Bt_H(H);
  for(int h = 0; h < H; ++h)
  {
    List Bt_n(n);
    for(int i = 0; i < n; ++i)
    {
      IntegerVector tm_index_Temp = tm_index_x[i];
      NumericVector Bt_ni(ni_x[i]);
      for(int j = 0; j < ni_x[i]; ++j)
      {
        if(IntegerVector::is_na( tm_index_Temp[j]))
        {
          Bt_ni[j] = NA_REAL;
        }else
        {
          Bt_ni[j] = Bt_x(tm_index_Temp[j], h);  
        }
      }
      Bt_n[i] = Bt_ni;
    }
    Bt_H[h] = Bt_n;
  }
  
  int n_unq_tm = Bt.nrow(); 
  IntegerVector Index_Bt(n_unq_tm);
  for(int i = 0; i < n_unq_tm; ++i)
  {
    Index_Bt[i] = i;
  }
  
  int n_unq_tm_x = Bt_x.nrow(); 
  IntegerVector Index_Bt_x(n_unq_tm_x);
  for(int i = 0; i < n_unq_tm_x; ++i)
  {
    Index_Bt_x[i] = i;
  }

  List unq_x_New(K);
  for(int k = 0; k < K; ++k){
    NumericVector unq_x_Temp = unq_x[k];
    int unq_x_Temp_size = unq_x_Temp.size();
    NumericVector unq_x_New_Temp(unq_x_Temp_size);
    for(int i = 0; i < unq_x_Temp_size; ++i){
      unq_x_New_Temp[i] = (unq_x_Temp[i]*x_Std_Error[k]) + x_Mean[k];
    }
    unq_x_New[k] = unq_x_New_Temp;
  }
  
  List x_index(K);
  for(int k = 0; k < K; ++k)
  {
    if(UseRaw[k])
    {
      x_index[k] = NA_INTEGER;
    }
    else
    {
      NumericVector unq_x_Temp = unq_x_New[k];
      List x_index_Subject(n);
      for(int i = 0; i < n; ++i)
      {
        NumericVector x_Temp(ni_x[i]);
        IntegerVector id_index_Temp = id_index_x[i];
        for(int j = 0; j < ni_x[i]; ++j)
        {
          x_Temp[j] = Org_x(id_index_Temp[j] , k);
        }
        x_index_Subject[i] = Approx_Match_C_NA(x_Temp,unq_x_Temp);
      }
      x_index[k] = x_index_Subject;
    }
  }
  
  List Bx_K(K);
  for(int k = 0; k < K; ++k)
  {
    NumericMatrix Bx_K_Temp = Bx[k];
    List Bx_Dk(Dk[k]);
    if(UseRaw[k])
    {
      for(int d = 0; d < Dk[k]; ++d)
      {
        List Bx_n(n);
        for(int i = 0; i < n; ++i)
        {
          IntegerVector id_index_Temp = id_index_x[i];
          NumericVector Bx_ni(ni_x[i]);
          for(int j = 0; j < ni_x[i]; ++j)
          {
            Bx_ni[j] = (Org_x(id_index_Temp[j],k) - x_Mean[k])/x_Std_Error[k];
          }
          Bx_n[i] = Bx_ni;
        }
        Bx_Dk[d] = Bx_n;
      }
    }
    else{
      List x_index_Temp = x_index[k];
      for(int d = 0; d < Dk[k]; ++d)
      {
        List Bx_n(n);
        for(int i = 0; i < n; ++i)
        {
          IntegerVector x_index_Temp_n = x_index_Temp[i];
          NumericVector Bx_ni(ni_x[i]);
          for(int j = 0; j < ni_x[i]; ++j){
            if(IntegerVector::is_na(x_index_Temp_n[j])){
              Bx_ni[j] = NA_REAL;
            }else
            {
              Bx_ni[j] = Bx_K_Temp(x_index_Temp_n[j],d);  
            }
          }
          Bx_n[i] = Bx_ni;
        }
        Bx_Dk[d] = Bx_n;
      }
    }
    Bx_K[k] = Bx_Dk;
  }
  
  List Bxt(K);
  for(int k = 0; k < K; ++k)
  {
    List Bx_Temp_K = Bx_K[k];
    List Bxt_Dk(Dk[k]);
    for(int d = 0; d < Dk[k]; ++d)
    {
      List Bx_Temp_Dk = Bx_Temp_K[d];
      List Bxt_H(H);
      for(int h = 0; h < H; ++h)
      {
        List Bt_Temp_H = Bt_H[h];
        List Bxt_n(n);
        for(int i = 0; i < n; ++i)
        {
          NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
          NumericVector Bt_Temp_n = Bt_Temp_H[i];
          NumericVector Bxt_ni(ni_x[i]);
          for(int j = 0; j < ni_x[i]; ++j)
          {
            Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];
          }
          Bxt_n[i] = Bxt_ni;
        }
        Bxt_H[h] = Bxt_n;
      }
      Bxt_Dk[d] = Bxt_H;
    }
    Bxt[k] = Bxt_Dk;
  }


  List Bxt_time_map(K);
  for(int k = 0; k < K; ++k)
  {
    List temp_Bxt_K = Bxt[k];
    List Bxt_time_map_Dk(Dk[k]);
    for(int d = 0; d < Dk[k]; ++d)
    {
      List temp_Bxt_Dk = temp_Bxt_K[d];
      List Bxt_time_map_H(H);
      for(int h = 0; h < H; ++h)
      {
        List temp_Bxt_H = temp_Bxt_Dk[h];
        List Bxt_time_map_n(n);
        for(int i = 0; i < n; ++i)
        {
          NumericVector temp_Bxt_n = temp_Bxt_H[i];
          mat temp_time_map = time_map[i];
          NumericVector temp_Bxt_time_map_n = Matrix_Vector_Multiplication_C(temp_time_map,temp_Bxt_n );
           Bxt_time_map_n[i] = temp_Bxt_time_map_n;
        }
        Bxt_time_map_H[h] = Bxt_time_map_n;
      }
      Bxt_time_map_Dk[d] = Bxt_time_map_H;
    }
    Bxt_time_map[k] = Bxt_time_map_Dk;
  }


  List Byt_xt_time_map(K);
  for(int k = 0; k < K; ++k)
  {
    List temp_Bxt_time_map_K = Bxt_time_map[k];
    List Byt_xt_time_map_Dk(Dk[k]);
    for(int d = 0; d < Dk[k]; ++d)
    {
      List temp_Bxt_time_map_Dk = temp_Bxt_time_map_K[d];
      List Byt_xt_time_map_H(H);
      for(int h = 0; h < H; ++h)
      {
        List temp_Bxt_time_map_H = temp_Bxt_time_map_Dk[h];
        List Byt_xt_time_map_G(G);
        for(int g = 0; g < G; ++g)
        {
          List temp_Bt_G = Bt_G[g];
          List Byt_xt_time_map_n(n);
          for(int i = 0; i < n; ++i)
          {
            NumericVector temp_Bt_n = temp_Bt_G[i];
            NumericVector temp_Bxt_time_map_n = temp_Bxt_time_map_H[i];
            NumericVector Byt_xt_time_map_ni(ni[i]);
            for(int j = 0; j < ni[i]; ++j)
            {
              Byt_xt_time_map_ni[j] = temp_Bt_n[j]*temp_Bxt_time_map_n[j];
            }
            Byt_xt_time_map_n[i] = Byt_xt_time_map_ni;
          }
          Byt_xt_time_map_G[g] = Byt_xt_time_map_n;
        }
        Byt_xt_time_map_H[h] = Byt_xt_time_map_G;
      }
      Byt_xt_time_map_Dk[d] = Byt_xt_time_map_H;
    }
    Byt_xt_time_map[k] = Byt_xt_time_map_Dk;
  }

  
  NumericMatrix mu_zero(N,L);
  for(int l = 0; l < L; ++l){
    for(int i = 0; i < N; ++i){
      mu_zero(i,l) = 0;
    }
  }
  
  NumericMatrix mu_zero_vec(N,1);
  for(int i = 0; i < N; ++i){
    mu_zero_vec(i,0) = 0;
  }

  
  //NumericMatrix Org_mu(N,L);
  mat Org_mu(N,L);
  List mu_List(M);
  for(int m = 0; m < M; ++m){
    mu_List[m] = NA_REAL;
  }
  //NumericMatrix Org_mu_Mopt(N,L);
  mat Org_mu_Mopt(N,L);
  NumericVector Vec_zero(N);
  for(int i = 0; i < N; ++i){
    Vec_zero[i] = 0.0;
  }
  
  NumericMatrix Error_Rate(M,L);
  for(int m = 0; m < M; ++m){
    for(int l = 0; l < L; ++l){
      Error_Rate(m,l) = NA_REAL;
    }
  }
  
  IntegerVector Mopt(L);
  NumericVector rmse(L);
  for(int l = 0; l < L; ++l){
    Mopt[l] = NA_INTEGER;
    rmse[l] = NA_REAL;
  }
  int Mopt_Max;
  
  List vimp(L);
  List vimp_Coef(L);
  NumericMatrix mu_vimp(N,1);
  NumericMatrix Org_mu_vimp(N,1);
  double Error_Rate_vimp;
  NumericMatrix Org_x_New(N_x,K);
    
  if(testFlag)
  { 
    for(int m = 0; m < M; ++m)
    {
      
      List Beta_Hat = Beta_Hat_List[m];
      
      mat linear_pred(N,L,fill::zeros);
      for(int l = 0; l < L; ++l)
      {
        vec Temp_linear_pred(N,fill::zeros);
        for(int k = 0; k < K; ++k)
        {
          List Bxt_Temp_K = Byt_xt_time_map[k];
          List Beta_Temp_K = Beta_Hat[k];
          for(int d = 0; d < Dk[k]; ++d)
          {
            List Bxt_Temp_Dk = Bxt_Temp_K[d];
            List Beta_Temp_Dk = Beta_Temp_K[d];
            for(int h = 0; h < H; ++h)
            {
              List Bxt_Temp_H = Bxt_Temp_Dk[h];
              List Beta_Temp_H = Beta_Temp_Dk[h];
              for(int g = 0; g < G; ++g)
              {
                List Bxt_Temp_G = Bxt_Temp_H[g];
                List Beta_Temp_G = Beta_Temp_H[g];
                mat Beta_Temp_L = Beta_Temp_G[l];
                vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                Temp_linear_pred = Temp_linear_pred + Temp_Temp_linear_pred;
              }
            }
          }
        }
        linear_pred.col(l) = Temp_linear_pred;
      }

      mat Org_linear_pred(N,L);
      for(int l = 0; l < L; ++l)
      {
        Org_linear_pred.col(l) = ( linear_pred.col(l) * y_Std_Error[l] ) + y_Mean[l];
      }

      mat mu(N,L);
      for(int l = 0; l < L; ++l)
      {
        vec Temp_linear_pred_mu = linear_pred.col(l);
        mu.col(l) = extract_mu(Temp_linear_pred_mu,y_scale_summary );
      }
      
      //mat Org_mu(N, L);
      for(int l = 0; l < L; ++l)
      {
        vec Temp_Org_linear_pred = Org_linear_pred.col(l);
        Org_mu.col(l) = extract_mu(Temp_Org_linear_pred,y_scale_summary );
      }
      
      /* 
       Work on code: mu_List[m] = Org_mu; in future. mu_List save Org_mu 
       at every iteration, however when you come out of the loop, all elements of list
       will have Org_mu corresponding to the last boosting iteration.
       For now, I have different way of calculating mu_Mopt rather
       than using mu_List. mu_List will not be save for output until
       I fixed the issue.
       */
      mu_List[m] = Org_mu;

      for(int l = 0; l < L; ++l)
      {
        NumericVector Org_y_Temp(N);
        NumericVector mu_Temp(N);
        for(int i = 0; i < N; ++i)
        {
          Org_y_Temp[i] = Org_y(i,l);
          mu_Temp[i] = Org_mu(i,l);
        }
        if(y_scale_summary == "continuous")
        {
        Error_Rate(m,l) = l2Dist_Vector_C_NA(Org_y_Temp, mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index); 
        }else
        {
          if(y_scale_summary == "binary")
          {
            Error_Rate(m,l) = l2Dist_Vector_C_NA(Org_y_Temp, mu_Temp, id_index);
          }
        }
      }
      
    }
    
  } else
  {
      mat linear_pred(N,L,fill::zeros);
      for(int l = 0; l < L; ++l)
      {
        vec Temp_linear_pred(N,fill::zeros);
        for(int k = 0; k < K; ++k)
        {
          List Bxt_Temp_K = Byt_xt_time_map[k];
          List Beta_Temp_K = Beta[k];
          for(int d = 0; d < Dk[k]; ++d)
          {
            List Bxt_Temp_Dk = Bxt_Temp_K[d];
            List Beta_Temp_Dk = Beta_Temp_K[d];
            for(int h = 0; h < H; ++h)
            {
              List Bxt_Temp_H = Bxt_Temp_Dk[h];
              List Beta_Temp_H = Beta_Temp_Dk[h];
              for(int g = 0; g < G; ++g)
              {
                List Bxt_Temp_G = Bxt_Temp_H[g];
                List Beta_Temp_G = Beta_Temp_H[g];
                mat Beta_Temp_L = Beta_Temp_G[l];
                vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                Temp_linear_pred = Temp_linear_pred + Temp_Temp_linear_pred;
              }
            }
          }
        }
        linear_pred.col(l) = Temp_linear_pred;
      }

      mat Org_linear_pred(N,L);
      for(int l = 0; l < L; ++l)
      {
        Org_linear_pred.col(l) = ( linear_pred.col(l) * y_Std_Error[l] ) + y_Mean[l];
      }

      //mat Org_mu(N, L);
      for(int l = 0; l < L; ++l)
      {
        vec Temp_Org_linear_pred = Org_linear_pred.col(l);
        Org_mu.col(l) = extract_mu(Temp_Org_linear_pred,y_scale_summary );
      }

  }
  
  if(testFlag)
  {
    for(int l = 0; l < L; ++l)
    {
      NumericVector Error_Rate_L(M);
      for(int m = 0; m < M; ++m)
      {
        Error_Rate_L[m] = Error_Rate(m,l);
      }
      double Min_Error_Rate = min(Error_Rate_L);
      NumericVector Diff_Error(M);
      for(int m = 0; m < M; ++m)
      {
        Diff_Error[m] = ( Error_Rate_L[m] - Min_Error_Rate );
      }
      Diff_Error = abs(Diff_Error);
      for(int m = 0; m < M; ++m)
      {
        if(Diff_Error[m] < eps)
        {
          Mopt[l] = m;
          rmse[l] = Error_Rate(Mopt[l],l);
          break;
        }
      }
    }
    Mopt_Max = max(Mopt);
  }
  
  if(testFlag)
  {
    mat linear_pred(N,L,fill::zeros);
    for(int l = 0; l < L; ++l)
    {
      int Mopt_Temp = Mopt[l];
      vec Temp_linear_pred(N,fill::zeros);
      List Beta_Hat = Beta_Hat_List[Mopt_Temp];
      for(int k = 0; k < K; ++k)
      {
        List Bxt_Temp_K = Byt_xt_time_map[k];
        List Beta_Temp_K = Beta_Hat[k];
        for(int d = 0; d < Dk[k]; ++d)
        {
          List Bxt_Temp_Dk = Bxt_Temp_K[d];
          List Beta_Temp_Dk = Beta_Temp_K[d];
          for(int h = 0; h < H; ++h)
          {
            List Bxt_Temp_H = Bxt_Temp_Dk[h];
            List Beta_Temp_H = Beta_Temp_Dk[h];
            for(int g = 0; g < G; ++g)
            {
              List Bxt_Temp_G = Bxt_Temp_H[g];
              List Beta_Temp_G = Beta_Temp_H[g];
              mat Beta_Temp_L = Beta_Temp_G[l];
              vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
              Temp_linear_pred = Temp_linear_pred + Temp_Temp_linear_pred;
            }
          }
        }
      }
      linear_pred.col(l) = Temp_linear_pred;
    }

    mat Org_linear_pred(N,L);
    for(int l = 0; l < L; ++l)
    {
      Org_linear_pred.col(l) = ( linear_pred.col(l) * y_Std_Error[l] ) + y_Mean[l];
    }

    
    //mat Org_mu(N, L);
    for(int l = 0; l < L; ++l)
    {
      vec Temp_Org_linear_pred = Org_linear_pred.col(l);
      Org_mu_Mopt.col(l) = extract_mu(Temp_Org_linear_pred,y_scale_summary );
    }
  }  
  
  // VIMP MAIN PART IS DONE BOTH FOR VIMP AND VIMP_COEF BUT FOR THE TIME_VARYING PART OF VIMP AND VIMP_COEF IS YET TO BE DONE. THE CURRENT CODE FOR TIME_VARYING FOR BOTH VIMP AND VIMP_COEF IS NOT CORRECT. THIS IS BECAUSE THERE IS NO MECHANISM TO NOISE-UP THE K'TH COVARIATE, INSTEAD I AM NOISING ONLY G'TH AND H'TH ELEMENTS OF TIME, WHEN IN FACT I SHOULD NOISE UP K'TH COVARIATE, AND G'TH AND H'TH ELEMENTS SIMULTANEUOUSLY. WORK ON THIS LATER.
  if(vimpFlag)
  {    
    for(int l = 0; l < L; ++l)
    {
      List Beta_Mopt = Beta_Hat_List[ Mopt[l]  ];
      List vimp_main(K);
      List vimp_int(K);

      for(int kk = 0; kk < K; ++kk)
      {
        List vimp_Bxt(K);
        for(int k = 0; k < K; ++k)
        {
          if(k == kk)
          {
            NumericVector Org_x_kk(N_x);
            for(int i = 0; i < N_x; ++i)
            {
              Org_x_kk[i] = Org_x(i,kk);
            }
            seed_value = seed_value + add_seed;
            Org_x_kk = randomShuffle(Org_x_kk, N_x, setting_seed, seed_value,  false, R_NilValue);
            NumericMatrix Bx_K_Temp = Bx[k];
            List Bx_Dk(Dk[k]);
            if(UseRaw[k])
            {
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Bx_n(n);
                for(int i = 0; i < n; ++i)
                {
                  IntegerVector id_index_Temp = id_index_x[i];
                  NumericVector Bx_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    Bx_ni[j] = ( Org_x_kk[ id_index_Temp[j] ] - x_Mean[k])/x_Std_Error[k];
                  }
                  Bx_n[i] = Bx_ni;
                }
                Bx_Dk[d] = Bx_n;
              }
            }else
            {
              NumericVector unq_x_Temp = unq_x_New[k];
              List x_index_Subject(n);
              for(int i = 0; i < n; ++i)
              {
                NumericVector x_Temp(ni_x[i]);
                IntegerVector id_index_Temp = id_index_x[i];
                for(int j = 0; j < ni_x[i]; ++j)
                {
                  x_Temp[j] = Org_x_kk[id_index_Temp[j]];
                }
                x_index_Subject[i] = Approx_Match_C_NA(x_Temp,unq_x_Temp);
              }
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Bx_n(n);
                for(int i = 0; i < n; ++i)
                {
                  IntegerVector x_index_Temp_n = x_index_Subject[i];
                  NumericVector Bx_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    if(IntegerVector::is_na(x_index_Temp_n[j]))
                    {
                      Bx_ni[j] = NA_REAL;
                    }else
                    {
                      Bx_ni[j] = Bx_K_Temp(x_index_Temp_n[j],d);  
                    }
                  }
                  Bx_n[i] = Bx_ni;
                }
                Bx_Dk[d] = Bx_n;
              }
            }
            
            List Bxt_Dk(Dk[k]);
            for(int d = 0; d < Dk[k]; ++d)
            {
              List Bx_Temp_Dk = Bx_Dk[d];
              List Bxt_H(H);
              for(int h = 0; h < H; ++h)
              {
                List Bt_Temp_H = Bt_H[h];
                List Bxt_n(n);
                for(int i = 0; i < n; ++i)
                {
                  NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
                  NumericVector Bt_Temp_n = Bt_Temp_H[i];
                  NumericVector Bxt_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];
                  }
                  Bxt_n[i] = Bxt_ni;
                }
                Bxt_H[h] = Bxt_n;
              }
              Bxt_Dk[d] = Bxt_H;
            }
            vimp_Bxt[k] = Bxt_Dk;
            
          }else
          {
            vimp_Bxt[k] = Bxt[k];
          }
        }


        List vimp_Bxt_time_map(K);
        for(int k = 0; k < K; ++k)
        {
          List temp_Bxt_K = vimp_Bxt[k];
          List Bxt_time_map_Dk(Dk[k]);
          for(int d = 0; d < Dk[k]; ++d)
          {
            List temp_Bxt_Dk = temp_Bxt_K[d];
            List Bxt_time_map_H(H);
            for(int h = 0; h < H; ++h)
            {
              List temp_Bxt_H = temp_Bxt_Dk[h];
              List Bxt_time_map_n(n);
              for(int i = 0; i < n; ++i)
              {
                NumericVector temp_Bxt_n = temp_Bxt_H[i];
                mat temp_time_map = time_map[i];
                NumericVector temp_Bxt_time_map_n = Matrix_Vector_Multiplication_C(temp_time_map,temp_Bxt_n );
                Bxt_time_map_n[i] = temp_Bxt_time_map_n;
              }
              Bxt_time_map_H[h] = Bxt_time_map_n;
            }
            Bxt_time_map_Dk[d] = Bxt_time_map_H;
          }
          vimp_Bxt_time_map[k] = Bxt_time_map_Dk;
        }

        List vimp_Byt_xt_time_map(K);
        for(int k = 0; k < K; ++k)
        {
          List temp_Bxt_time_map_K = vimp_Bxt_time_map[k];
          List Byt_xt_time_map_Dk(Dk[k]);
          for(int d = 0; d < Dk[k]; ++d)
          {
            List temp_Bxt_time_map_Dk = temp_Bxt_time_map_K[d];
            List Byt_xt_time_map_H(H);
            for(int h = 0; h < H; ++h)
            {
              List temp_Bxt_time_map_H = temp_Bxt_time_map_Dk[h];
              List Byt_xt_time_map_G(G);
              for(int g = 0; g < G; ++g)
              {
                List temp_Bt_G = Bt_G[g];
                List Byt_xt_time_map_n(n);
                for(int i = 0; i < n; ++i)
                {
                  NumericVector temp_Bt_n = temp_Bt_G[i];
                  NumericVector temp_Bxt_time_map_n = temp_Bxt_time_map_H[i];
                  NumericVector Byt_xt_time_map_ni(ni[i]);
                  for(int j = 0; j < ni[i]; ++j)
                  {
                    Byt_xt_time_map_ni[j] = temp_Bt_n[j]*temp_Bxt_time_map_n[j];
                  }
                  Byt_xt_time_map_n[i] = Byt_xt_time_map_ni;
                }
                Byt_xt_time_map_G[g] = Byt_xt_time_map_n;
              }
              Byt_xt_time_map_H[h] = Byt_xt_time_map_G;
            }
            Byt_xt_time_map_Dk[d] = Byt_xt_time_map_H;
          }
          vimp_Byt_xt_time_map[k] = Byt_xt_time_map_Dk;
        }

        vec linear_pred_vimp(N,fill::zeros);
        for(int k = 0; k < K; ++k)
        {
          List Bxt_Temp_K = vimp_Byt_xt_time_map[k];
          List Beta_Temp_K = Beta_Mopt[k];
          for(int d = 0; d < Dk[k]; ++d)
          {
            List Bxt_Temp_Dk = Bxt_Temp_K[d];
            List Beta_Temp_Dk = Beta_Temp_K[d];
            for(int h = 0; h < H; ++h)
            {
              List Bxt_Temp_H = Bxt_Temp_Dk[h];
              List Beta_Temp_H = Beta_Temp_Dk[h];
              for(int g = 0; g < G; ++g)
              {
                List Bxt_Temp_G = Bxt_Temp_H[g];
                List Beta_Temp_G = Beta_Temp_H[g];
                mat Beta_Temp_L = Beta_Temp_G[l];
                vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                linear_pred_vimp = linear_pred_vimp + Temp_Temp_linear_pred;
              }
            }
          }
        }

        vec Org_linear_pred_vimp(N);
        Org_linear_pred_vimp = ( linear_pred_vimp * y_Std_Error[l] ) + y_Mean[l];
        
        vec Org_mu_vimp(N);
        Org_mu_vimp = extract_mu(Org_linear_pred_vimp,y_scale_summary );
      
        NumericVector Org_y_Temp(N);
        NumericVector Org_mu_Temp(N);
        for(int i = 0; i < N; ++i)
        {
          Org_y_Temp[i] = Org_y(i,l);
          Org_mu_Temp[i] = Org_mu_vimp(i);
        }
        if(y_scale_summary == "continuous")
        {
          Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index);
        }else
        {
          if(y_scale_summary == "binary")
          {
            Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index);
          }
        }
        vimp_main[kk] = (Error_Rate_vimp - rmse[l])/rmse[l];
      }

      if(Time_Varying)
      {
        for(int kk = 0; kk < K; ++kk)
        {
          NumericMatrix vimp_Temp(G,H);
          for(int hh = 0; hh < H; ++hh)
          {
            for(int gg = 0; gg < G; ++gg)
            {
              List vimp_Bxt(K);
              for(int k = 0; k < K; ++k)
              {
                List vimp_Bt_H(H);
                if(k == kk)
                {
                  seed_value = seed_value + add_seed;
                  IntegerVector Index_Bt_x_noise = int_randomShuffle(Index_Bt_x,n_unq_tm_x,setting_seed,seed_value, false, R_NilValue);            
                  NumericMatrix vimp_Bt_x(n_unq_tm_x,H);
                  for(int i = 0; i < n_unq_tm_x; ++i)
                  {
                    for(int j = 0; j < H; ++j)
                    {
                      if(j == hh)
                      {
                        vimp_Bt_x(i,j) = Bt_x(Index_Bt_x_noise[i],j);  
                      }else
                      {
                        vimp_Bt_x(i,j) = Bt_x(Index_Bt_x[i],j); 
                      }
                    }
                  }
                  for(int h = 0; h < H; ++h)
                  {
                    List Bt_n(n);
                    for(int i = 0; i < n; ++i)
                    {
                      IntegerVector tm_index_Temp = tm_index_x[i];
                      NumericVector Bt_ni(ni_x[i]);
                      for(int j = 0; j < ni_x[i]; ++j)
                      {
                        if(IntegerVector::is_na(tm_index_Temp[j]))
                        {
                          Bt_ni[j] = NA_REAL;
                        }else
                        {
                          Bt_ni[j] = vimp_Bt_x(tm_index_Temp[j], h);  
                        }
                      }
                      Bt_n[i] = Bt_ni;
                    }
                    vimp_Bt_H[h] = Bt_n;
                  }
                  
                  List Bx_Temp_K = Bx_K[k];
                  List Bxt_Dk(Dk[k]);
                  for(int d = 0; d < Dk[k]; ++d)
                  {
                    List Bx_Temp_Dk = Bx_Temp_K[d]; 
                    List Bxt_H(H);
                    for(int h = 0; h < H; ++h)
                    {
                      List Bt_Temp_H = vimp_Bt_H[h];
                      List Bxt_n(n);
                      for(int i = 0; i < n; ++i)
                      {
                        NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
                        NumericVector Bt_Temp_n = Bt_Temp_H[i];
                        NumericVector Bxt_ni(ni_x[i]);
                        for(int j = 0; j < ni_x[i]; ++j)
                        {
                        Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];  
                        }
                        Bxt_n[i] = Bxt_ni;
                      }
                      Bxt_H[h] = Bxt_n;
                    }
                    Bxt_Dk[d] = Bxt_H;
                  }
                  vimp_Bxt[k] = Bxt_Dk;
                }else
                {
                  vimp_Bxt[k] = Bxt[k];
                }
              }

              List vimp_Bxt_time_map(K);
              for(int k = 0; k < K; ++k)
              {
                List temp_Bxt_K = vimp_Bxt[k];
                List Bxt_time_map_Dk(Dk[k]);
                for(int d = 0; d < Dk[k]; ++d)
                {
                  List temp_Bxt_Dk = temp_Bxt_K[d];
                  List Bxt_time_map_H(H);
                  for(int h = 0; h < H; ++h)
                  {
                    List temp_Bxt_H = temp_Bxt_Dk[h];
                    List Bxt_time_map_n(n);
                    for(int i = 0; i < n; ++i)
                    {
                      NumericVector temp_Bxt_n = temp_Bxt_H[i];
                      mat temp_time_map = time_map[i];
                      NumericVector temp_Bxt_time_map_n = Matrix_Vector_Multiplication_C(temp_time_map,temp_Bxt_n );
                      Bxt_time_map_n[i] = temp_Bxt_time_map_n;
                    }
                    Bxt_time_map_H[h] = Bxt_time_map_n;
                  }
                  Bxt_time_map_Dk[d] = Bxt_time_map_H;
                }
                vimp_Bxt_time_map[k] = Bxt_time_map_Dk;
              }

              List vimp_Byt_xt_time_map(K);
              for(int k = 0; k < K; ++k)
              {
                List vimp_Bt_G(G);

                seed_value = seed_value + add_seed;
                IntegerVector Index_Bt_noise = int_randomShuffle(Index_Bt,n_unq_tm,setting_seed,seed_value, false, R_NilValue);            
                NumericMatrix vimp_Bt(n_unq_tm,G);
                for(int i = 0; i < n_unq_tm; ++i)
                {
                  for(int j = 0; j < G; ++j)
                  {
                    if(j == gg)
                    {
                      vimp_Bt(i,j) = Bt(Index_Bt_noise[i],j);  
                    }else
                    {
                      vimp_Bt(i,j) = Bt(Index_Bt[i],j); 
                    }
                  }
                }

                for(int g = 0; g < G; ++g)
                {
                  List Bt_n(n);
                  for(int i = 0; i < n; ++i)
                  {
                    IntegerVector tm_index_Temp = tm_index[i];
                    NumericVector Bt_ni(ni[i]);
                    for(int j = 0; j < ni[i]; ++j)
                    {
                      if(IntegerVector::is_na(tm_index_Temp[j]))
                      {
                        Bt_ni[j] = NA_REAL;
                      }else
                      {
                        Bt_ni[j] = vimp_Bt(tm_index_Temp[j], g);  
                      }
                    }
                    Bt_n[i] = Bt_ni;
                  }
                  vimp_Bt_G[g] = Bt_n;
                }
                
                List temp_Bxt_time_map_K = vimp_Bxt_time_map[k];
                List Byt_xt_time_map_Dk(Dk[k]);
                for(int d = 0; d < Dk[k]; ++d)
                {
                  List temp_Bxt_time_map_Dk = temp_Bxt_time_map_K[d];
                  List Byt_xt_time_map_H(H);
                  for(int h = 0; h < H; ++h)
                  {
                    List temp_Bxt_time_map_H = temp_Bxt_time_map_Dk[h];
                    List Byt_xt_time_map_G(G);
                    for(int g = 0; g < G; ++g)
                    {
                      List temp_Bt_G = vimp_Bt_G[g];
                      List Byt_xt_time_map_n(n);
                      for(int i = 0; i < n; ++i)
                      {
                        NumericVector temp_Bt_n = temp_Bt_G[i];
                        NumericVector temp_Bxt_time_map_n = temp_Bxt_time_map_H[i];
                        NumericVector Byt_xt_time_map_ni(ni[i]);
                        for(int j = 0; j < ni[i]; ++j)
                        {
                          Byt_xt_time_map_ni[j] = temp_Bt_n[j]*temp_Bxt_time_map_n[j];
                        }
                        Byt_xt_time_map_n[i] = Byt_xt_time_map_ni;
                      }
                      Byt_xt_time_map_G[g] = Byt_xt_time_map_n;
                    }
                    Byt_xt_time_map_H[h] = Byt_xt_time_map_G;
                  }
                  Byt_xt_time_map_Dk[d] = Byt_xt_time_map_H;
                }
                vimp_Byt_xt_time_map[k] = Byt_xt_time_map_Dk;
              }
 
              vec linear_pred_vimp(N,fill::zeros);
              for(int k = 0; k < K; ++k)
              {
                List Bxt_Temp_K = vimp_Byt_xt_time_map[k];
                List Beta_Temp_K = Beta_Mopt[k];
                for(int d = 0; d < Dk[k]; ++d)
                {
                  List Bxt_Temp_Dk = Bxt_Temp_K[d];
                  List Beta_Temp_Dk = Beta_Temp_K[d];
                  for(int h = 0; h < H; ++h)
                  {
                    List Bxt_Temp_H = Bxt_Temp_Dk[h];
                    List Beta_Temp_H = Beta_Temp_Dk[h];
                    for(int g = 0; g < G; ++g)
                    {
                      List Bxt_Temp_G = Bxt_Temp_H[g];
                      List Beta_Temp_G = Beta_Temp_H[g];
                      mat Beta_Temp_L = Beta_Temp_G[l];
                      vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                      linear_pred_vimp = linear_pred_vimp + Temp_Temp_linear_pred;
                    }
                  }
                }
              }

              vec Org_linear_pred_vimp(N);
              Org_linear_pred_vimp = ( linear_pred_vimp * y_Std_Error[l] ) + y_Mean[l];
              
              vec Org_mu_vimp(N);
              Org_mu_vimp = extract_mu(Org_linear_pred_vimp,y_scale_summary );
            
              NumericVector Org_y_Temp(N);
              NumericVector Org_mu_Temp(N);
              
              for(int i = 0; i < N; ++i)
              {
                Org_y_Temp[i] = Org_y(i,l);
                Org_mu_Temp[i] = Org_mu_vimp(i);
              }

              if(y_scale_summary == "continuous")
              {
                Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index);
              }else
              {
                if(y_scale_summary == "binary")
                {
                  Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index);
                }
              }

              vimp_Temp(gg, hh) = (Error_Rate_vimp - rmse[l])/rmse[l];

            }
          }
          vimp_int[kk] = vimp_Temp;
        }
     }

     List vimp_combine = List::create(
      _["vimp_main"] = vimp_main,
      _["vimp_int"] = vimp_int
     );
     vimp[l] = vimp_combine;
   }
  }
  

  
  if(vimpFlag_Coef)
  {
    for(int l = 0; l < L; ++l)
    {
      List Beta_Mopt = Beta_Hat_List[ Mopt[l]  ];
      List vimp_coef_main(K);
      List vimp_coef_int(K);
      
      for(int kk = 0; kk < K; ++kk)
      {
        vec linear_pred_vimp(N,fill::zeros);
        for(int k = 0; k < K; ++k)
        {
          List Bxt_Temp_K = Byt_xt_time_map[k];
          List Beta_Temp_K = Beta_Mopt[k];
          for(int d = 0; d < Dk[k]; ++d)
          {
            List Bxt_Temp_Dk = Bxt_Temp_K[d];
            List Beta_Temp_Dk = Beta_Temp_K[d];
            for(int h = 0; h < H; ++h)
            {
              List Bxt_Temp_H = Bxt_Temp_Dk[h];
              List Beta_Temp_H = Beta_Temp_Dk[h];
              for(int g = 0; g < G; ++g)
              {
                List Bxt_Temp_G = Bxt_Temp_H[g];
                List Beta_Temp_G = Beta_Temp_H[g];
                mat Beta_Temp_L = Beta_Temp_G[l];
                if(k != kk)
                {
                vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                linear_pred_vimp = linear_pred_vimp + Temp_Temp_linear_pred;
                }
              }
            }
          }
        }

        vec Org_linear_pred_vimp(N);
        Org_linear_pred_vimp = ( linear_pred_vimp * y_Std_Error[l] ) + y_Mean[l];
        
        vec Org_mu_vimp(N);
        Org_mu_vimp = extract_mu(Org_linear_pred_vimp,y_scale_summary );
        
        NumericVector Org_y_Temp(N);
        NumericVector Org_mu_Temp(N);
        for(int i = 0; i < N; ++i)
        {
          Org_y_Temp[i] = Org_y(i,l);
          Org_mu_Temp[i] = Org_mu_vimp(i);
        }
        if(y_scale_summary == "continuous")
        {
          Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index);
        }else
        {
          if(y_scale_summary == "binary")
          {
            Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index);
          }
        }
        vimp_coef_main[kk] = (Error_Rate_vimp - rmse[l])/rmse[l];
      }
      
      if(Time_Varying)
      {
        for(int kk = 0; kk < K; ++kk)
        {
          NumericMatrix vimp_Temp(G,H);
          for(int hh = 0; hh < H; ++hh)
          {
            for(int gg = 0; gg < G; ++gg)
            {
              vec linear_pred_vimp(N,fill::zeros);
              for(int k = 0; k < K; ++k)
              {
                List Bxt_Temp_K = Byt_xt_time_map[k];
                List Beta_Temp_K = Beta_Mopt[k];
                for(int d = 0; d < Dk[k]; ++d)
                {
                  List Bxt_Temp_Dk = Bxt_Temp_K[d];
                  List Beta_Temp_Dk = Beta_Temp_K[d];
                  for(int h = 0; h < H; ++h)
                  {
                    List Bxt_Temp_H = Bxt_Temp_Dk[h];
                    List Beta_Temp_H = Beta_Temp_Dk[h];
                    for(int g = 0; g < G; ++g)
                    {
                      if(k != kk && h != hh && g != gg)
                      {
                        List Bxt_Temp_G = Bxt_Temp_H[g];
                        List Beta_Temp_G = Beta_Temp_H[g];
                        mat Beta_Temp_L = Beta_Temp_G[l];
                        vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                        linear_pred_vimp = linear_pred_vimp + Temp_Temp_linear_pred;
                      }
                    }
                  }
                }
              }

              vec Org_linear_pred_vimp(N);
              Org_linear_pred_vimp = ( linear_pred_vimp * y_Std_Error[l] ) + y_Mean[l];
              
              vec Org_mu_vimp(N);
              Org_mu_vimp = extract_mu(Org_linear_pred_vimp,y_scale_summary );
              
              NumericVector Org_y_Temp(N);
              NumericVector Org_mu_Temp(N);
              
              for(int i = 0; i < N; ++i)
              {
                Org_y_Temp[i] = Org_y(i,l);
                Org_mu_Temp[i] = Org_mu_vimp(i);
              }
              
              if(y_scale_summary == "continuous")
              {
                Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index);
              }else
              {
                if(y_scale_summary == "binary")
                {
                  Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index);
                }
              }
              vimp_Temp(gg, hh) = (Error_Rate_vimp - rmse[l])/rmse[l];
            }

          }
          vimp_coef_int[kk] = vimp_Temp;
        }
      }
      List vimp_combine = List::create(
      _["vimp_coef_main"] = vimp_coef_main,
      _["vimp_coef_int"] = vimp_coef_int
     );
      vimp_Coef[l] = vimp_combine;
    }
  }  
  
  
  List Data = List::create(
    _["Org_x"] = Org_x, 
    _["Org_y"] = Org_y,
    _["id"] = id,
    _["tm"] = tm,
    _["id_x"] = id_x,
    _["tm_x"] = tm_x,
    _["x_Mean"] = x_Mean,
    _["x_Std_Error"] = x_Std_Error,
    _["y_Mean"] = y_Mean,
    _["y_Std_Error"] = y_Std_Error
  );
  
  List Dimensions = List::create(
    _["n"] = n,
    _["K"] = K,
    _["L"] = L,
    _["H"] = H,
    _["G"] = G,
    _["Dk"] = Dk,
    _["ni"] = ni,
    _["ni_x"] = ni_x,
    _["N"] = N,
    _["N_x"] = N_x,
    _["n_unq_tm"] = n_unq_tm,
    _["n_unq_tm_x"] = n_unq_tm_x
  );
  
  List Index = List::create(
    _["unq_id"] = unq_id,
    _["unq_tm"] = unq_tm,
    _["unq_tm_x"] = unq_tm_x,
    _["unq_x"] = unq_x,
    _["id_index"] = id_index,
    _["id_index_x"] = id_index_x,
    _["tm_index"] = tm_index,
    _["tm_index_x"] = tm_index_x,
    _["x_index"] = x_index,
    _["unq_x_New"] = unq_x_New,
    _["Index_Bt"] = Index_Bt,
    _["Index_Bt_x"] = Index_Bt_x
  );
  
  List BS = List::create(
    _["Bt"] = Bt,
    _["Bt_x"] = Bt_x,
    _["Bxt"] = Bxt,
    _["Bx_K"] = Bx_K,
    _["Bt_H"] = Bt_H,
    _["Bt_G"] = Bt_G,
    _["Bx"] = Bx,
    _["time_map"] = time_map
  );
  
  List Pred_Object = List::create(
    _["Vec_zero"] = Vec_zero,
    _["mu_zero_vec"] = mu_zero_vec
  );
  
  return List::create(
    _["Data"] = Data,
    _["UseRaw"] = UseRaw,
    _["Dimensions"] = Dimensions,
    _["Index"] = Index,
    _["BS"] = BS,
    _["Beta_Hat_List"] = Beta_Hat_List,
    _["Org_mu"] = Org_mu,
    _["Org_mu_Mopt"] = Org_mu_Mopt,
    _["Error_Rate"] = Error_Rate,
    _["Mopt"] = Mopt,
    _["rmse"] = rmse,
    _["vimp"] = vimp,
    _["vimp_Coef"] = vimp_Coef,
    _["Pred_Object"] = Pred_Object
  );
}


// [[Rcpp::export]]
List vimp_BoostMLR_C(NumericMatrix Org_x,
                     NumericMatrix Org_y,
                     NumericVector tm,
                     NumericVector id,
                     NumericVector id_x,
                     NumericVector tm_x,
                     NumericVector x_Mean,
                     NumericVector x_Std_Error,
                     NumericVector y_Mean,
                     NumericVector y_Std_Error,
                     bool intercept,
                     String y_scale_summary,
                     int n,
                     IntegerVector ni,
                     IntegerVector ni_x,
                     int N,
                     int N_x,
                     int L,
                     int K,
                     int p,
                     int H,
                     int G,
                     IntegerVector Dk,
                     int n_unq_tm,
                     int n_unq_tm_x,
                     LogicalVector UseRaw,
                     List id_index,
                     List id_index_x,
                     List tm_index,
                     List tm_index_x,
                     List unq_x_New,
                     IntegerVector Index_Bt,
                     IntegerVector Index_Bt_x,
                     IntegerVector vimp_set,
                     bool joint,
                     NumericMatrix Bt,
                     NumericMatrix Bt_x,
                     List Bt_H,
                     List Bt_G,
                     List Bx,
                     List Bxt,
                     List Bx_K,
                     List time_map,
                     List Beta_Hat_List,
                     IntegerVector Mopt,
                     double nu,
                     NumericVector rmse,
                     bool Time_Varying,
                     NumericVector Vec_zero,
                     NumericMatrix mu_zero_vec,
                     bool setting_seed,
                     unsigned long int seed_value)
{
  List vimp(L);
  NumericMatrix mu_vimp(N,1);
  NumericMatrix Org_mu_vimp(N,1);
  NumericMatrix Org_x_Noise(N,K);
  double Error_Rate_vimp;
  IntegerVector Index_N(N);
  for(int i = 0;i < N; ++i)
  {
    Index_N[i] = i;
  }


  for(int l = 0; l < L; ++l)
  {
    List Beta_Mopt = Beta_Hat_List[ Mopt[l]  ];
    List vimp_main(K);
    List vimp_int(K);

    for(int kk = 0; kk < p; ++kk)
    {
      if(joint)
      {
        for(int k_jt = 0; k_jt < K; ++k_jt)
        {
          if( is_true( any( vimp_set == k_jt ) ) ) 
          {
            seed_value = seed_value + add_seed;
            Org_x_Noise(_,k_jt) = randomShuffle(Org_x(_,k_jt),N, setting_seed,seed_value, false, R_NilValue);
          } else
          {
            Org_x_Noise(_,k_jt) = Org_x(_,k_jt);
          }
        }
      } else
      {
        for(int k_sep = 0; k_sep < K; ++k_sep) 
        {
          if(k_sep == vimp_set[kk]) 
          {
            seed_value = seed_value + add_seed;
            Org_x_Noise(_,k_sep) = randomShuffle(Org_x(_,k_sep),N, setting_seed,seed_value, false, R_NilValue);
          } else
          {
          Org_x_Noise(_,k_sep) = Org_x(_,k_sep);
          }
        }
      }
        
      List vimp_Bxt(K);
      if(joint)
      {
        for(int k = 0; k < K; ++k)
        {
          if(is_true(any( vimp_set == k )))
          {
            NumericMatrix Bx_K_Temp = Bx[k];
            List Bx_Dk(Dk[k]);
            if(UseRaw[k])
            {
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Bx_n(n);
                for(int i = 0; i < n; ++i)
                {
                  IntegerVector id_index_Temp = id_index_x[i];
                  NumericVector Bx_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    Bx_ni[j] = ( Org_x_Noise(id_index_Temp[j],k) - x_Mean[k])/x_Std_Error[k];
                  }
                  Bx_n[i] = Bx_ni;
                }
                Bx_Dk[d] = Bx_n;
              }
            } else
            {
              NumericVector unq_x_Temp = unq_x_New[k];
              List x_index_Subject(n);
              for(int i = 0; i < n; ++i)
              {
                NumericVector x_Temp(ni_x[i]);
                IntegerVector id_index_Temp = id_index_x[i];
                for(int j = 0; j < ni_x[i]; ++j)
                {
                  x_Temp[j] = Org_x_Noise(id_index_Temp[j],k);
                }
                x_index_Subject[i] = Approx_Match_C_NA(x_Temp,unq_x_Temp);
              }
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Bx_n(n);
                for(int i = 0; i < n; ++i)
                {
                  IntegerVector x_index_Temp_n = x_index_Subject[i];
                  NumericVector Bx_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j){
                    if(IntegerVector::is_na(x_index_Temp_n[j])){
                      Bx_ni[j] = NA_REAL;
                    }else
                    {
                      Bx_ni[j] = Bx_K_Temp(x_index_Temp_n[j],d);
                    }
                  }
                  Bx_n[i] = Bx_ni;
                }
                Bx_Dk[d] = Bx_n;
              }
            }
            
            List Bxt_Dk(Dk[k]);
            for(int d = 0; d < Dk[k]; ++d)
            {
              List Bx_Temp_Dk = Bx_Dk[d];
              List Bxt_H(H);
              for(int h = 0; h < H; ++h)
              {
                List Bt_Temp_H = Bt_H[h];
                List Bxt_n(n);
                for(int i = 0; i < n; ++i)
                {
                  NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
                  NumericVector Bt_Temp_n = Bt_Temp_H[i];
                  NumericVector Bxt_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];
                  }
                  Bxt_n[i] = Bxt_ni;
                }
                Bxt_H[h] = Bxt_n;
              }
              Bxt_Dk[d] = Bxt_H;
            }
            vimp_Bxt[k] = Bxt_Dk;
            
          }else
          {
            vimp_Bxt[k] = Bxt[k];
          }
        }
      }else 
      {
        for(int k = 0; k < K; ++k)
        {
          if(k == vimp_set[kk])
          {
            NumericMatrix Bx_K_Temp = Bx[k];
            List Bx_Dk(Dk[k]);
            if(UseRaw[k])
            {
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Bx_n(n);
                for(int i = 0; i < n; ++i)
                {
                  IntegerVector id_index_Temp = id_index_x[i];
                  NumericVector Bx_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    Bx_ni[j] = ( Org_x_Noise(id_index_Temp[j],k) - x_Mean[k])/x_Std_Error[k];
                  }
                  Bx_n[i] = Bx_ni;
                }
                Bx_Dk[d] = Bx_n;
              }
            } else
            {
              NumericVector unq_x_Temp = unq_x_New[k];
              List x_index_Subject(n);
              for(int i = 0; i < n; ++i)
              {
                NumericVector x_Temp(ni_x[i]);
                IntegerVector id_index_Temp = id_index_x[i];
                for(int j = 0; j < ni_x[i]; ++j)
                {
                  x_Temp[j] = Org_x_Noise(id_index_Temp[j],k);
                }
                x_index_Subject[i] = Approx_Match_C_NA(x_Temp,unq_x_Temp);
              }
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Bx_n(n);
                for(int i = 0; i < n; ++i)
                {
                  IntegerVector x_index_Temp_n = x_index_Subject[i];
                  NumericVector Bx_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j)
                  {
                    if(IntegerVector::is_na(x_index_Temp_n[j]))
                    {
                      Bx_ni[j] = NA_REAL;
                    }else
                    {
                      Bx_ni[j] = Bx_K_Temp(x_index_Temp_n[j],d);
                    }
                  }
                  Bx_n[i] = Bx_ni;
                }
                Bx_Dk[d] = Bx_n;
              }
            }
            
            List Bxt_Dk(Dk[k]);
            for(int d = 0; d < Dk[k]; ++d){
              List Bx_Temp_Dk = Bx_Dk[d];
              List Bxt_H(H);
              for(int h = 0; h < H; ++h){
                List Bt_Temp_H = Bt_H[h];
                List Bxt_n(n);
                for(int i = 0; i < n; ++i){
                  NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
                  NumericVector Bt_Temp_n = Bt_Temp_H[i];
                  NumericVector Bxt_ni(ni_x[i]);
                  for(int j = 0; j < ni_x[i]; ++j){
                    Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];
                  }
                  Bxt_n[i] = Bxt_ni;
                }
                Bxt_H[h] = Bxt_n;
              }
              Bxt_Dk[d] = Bxt_H;
            }
            vimp_Bxt[k] = Bxt_Dk;
            
          }else
          {
            vimp_Bxt[k] = Bxt[k];
          }
        }        
      }


        List vimp_Bxt_time_map(K);
        for(int k = 0; k < K; ++k)
        {
          List temp_Bxt_K = vimp_Bxt[k];
          List Bxt_time_map_Dk(Dk[k]);
          for(int d = 0; d < Dk[k]; ++d)
          {
            List temp_Bxt_Dk = temp_Bxt_K[d];
            List Bxt_time_map_H(H);
            for(int h = 0; h < H; ++h)
            {
              List temp_Bxt_H = temp_Bxt_Dk[h];
              List Bxt_time_map_n(n);
              for(int i = 0; i < n; ++i)
              {
                NumericVector temp_Bxt_n = temp_Bxt_H[i];
                mat temp_time_map = time_map[i];
                NumericVector temp_Bxt_time_map_n = Matrix_Vector_Multiplication_C_naObs(temp_time_map,temp_Bxt_n );
                Bxt_time_map_n[i] = temp_Bxt_time_map_n;
              }
              Bxt_time_map_H[h] = Bxt_time_map_n;
            }
            Bxt_time_map_Dk[d] = Bxt_time_map_H;
          }
          vimp_Bxt_time_map[k] = Bxt_time_map_Dk;
        }



        List vimp_Byt_xt_time_map(K);
        for(int k = 0; k < K; ++k)
        {
          List temp_Bxt_time_map_K = vimp_Bxt_time_map[k];
          List Byt_xt_time_map_Dk(Dk[k]);
          for(int d = 0; d < Dk[k]; ++d)
          {
            List temp_Bxt_time_map_Dk = temp_Bxt_time_map_K[d];
            List Byt_xt_time_map_H(H);
            for(int h = 0; h < H; ++h)
            {
              List temp_Bxt_time_map_H = temp_Bxt_time_map_Dk[h];
              List Byt_xt_time_map_G(G);
              for(int g = 0; g < G; ++g)
              {
                List temp_Bt_G = Bt_G[g];
                List Byt_xt_time_map_n(n);
                for(int i = 0; i < n; ++i)
                {
                  NumericVector temp_Bt_n = temp_Bt_G[i];
                  NumericVector temp_Bxt_time_map_n = temp_Bxt_time_map_H[i];
                  NumericVector Byt_xt_time_map_ni(ni[i]);
                  for(int j = 0; j < ni[i]; ++j)
                  {
                    Byt_xt_time_map_ni[j] = temp_Bt_n[j]*temp_Bxt_time_map_n[j];
                  }
                  Byt_xt_time_map_n[i] = Byt_xt_time_map_ni;
                }
                Byt_xt_time_map_G[g] = Byt_xt_time_map_n;
              }
              Byt_xt_time_map_H[h] = Byt_xt_time_map_G;
            }
            Byt_xt_time_map_Dk[d] = Byt_xt_time_map_H;
          }
          vimp_Byt_xt_time_map[k] = Byt_xt_time_map_Dk;
        }

        vec linear_pred_vimp(N,fill::zeros);
        for(int k = 0; k < K; ++k)
        {
          List Bxt_Temp_K = vimp_Byt_xt_time_map[k];
          List Beta_Temp_K = Beta_Mopt[k];
          for(int d = 0; d < Dk[k]; ++d)
          {
            List Bxt_Temp_Dk = Bxt_Temp_K[d];
            List Beta_Temp_Dk = Beta_Temp_K[d];
            for(int h = 0; h < H; ++h)
            {
              List Bxt_Temp_H = Bxt_Temp_Dk[h];
              List Beta_Temp_H = Beta_Temp_Dk[h];
              for(int g = 0; g < G; ++g)
              {
                List Bxt_Temp_G = Bxt_Temp_H[g];
                List Beta_Temp_G = Beta_Temp_H[g];
                mat Beta_Temp_L = Beta_Temp_G[l];
                vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                linear_pred_vimp = linear_pred_vimp + Temp_Temp_linear_pred;
              }
            }
          }
        }
    
        vec Org_linear_pred_vimp(N);
        Org_linear_pred_vimp = ( linear_pred_vimp * y_Std_Error[l] ) + y_Mean[l];
        
        vec Org_mu_vimp(N);
        Org_mu_vimp = extract_mu(Org_linear_pred_vimp,y_scale_summary );
        
        NumericVector Org_y_Temp(N);
        NumericVector Org_mu_Temp(N);
        for(int i = 0; i < N; ++i)
        {
          Org_y_Temp[i] = Org_y(i,l);
          Org_mu_Temp[i] = Org_mu_vimp(i);
        }

        if(y_scale_summary == "continuous")
        {
          Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index);
        }else
        {
          if(y_scale_summary == "binary")
          {
            Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index);
          }
        }
      vimp_main[kk] = (Error_Rate_vimp - rmse[l])/rmse[l];
    }
    
    if(Time_Varying)
    {
      for(int kk = 0; kk < p; ++kk)
      {
        NumericMatrix vimp_Temp(G,H);
        for(int hh = 0; hh < H; ++hh)
        {
          for(int gg = 0; gg < G; ++gg)
          {
            List vimp_Bxt(K);
            if(joint)
            {
              for(int k = 0; k < K; ++k)
              {
                List vimp_Bt_H(H);
                if(is_true(any( vimp_set == k )))
                {
                  seed_value = seed_value + add_seed;
                  IntegerVector Index_Bt_x_noise = int_randomShuffle(Index_Bt_x, n_unq_tm_x,setting_seed,seed_value, false, R_NilValue);                
                  NumericMatrix vimp_Bt_x(n_unq_tm_x,H);
                  for(int i = 0; i < n_unq_tm_x; ++i)
                  {
                    for(int j = 0; j < H; ++j)
                    {
                      if(j == hh)
                      {
                        vimp_Bt_x(i,j) = Bt_x(Index_Bt_x_noise[i],j);
                      }else
                      {
                        vimp_Bt_x(i,j) = Bt_x(Index_Bt_x[i],j);
                      }
                    }
                  }
                  for(int h = 0; h < H; ++h)
                  {
                    List Bt_n(n);
                    for(int i = 0; i < n; ++i)
                    {
                      IntegerVector tm_index_Temp = tm_index_x[i];
                      NumericVector Bt_ni(ni_x[i]);
                      for(int j = 0; j < ni_x[i]; ++j)
                      {
                        if(IntegerVector::is_na(tm_index_Temp[j]))
                        {
                          Bt_ni[j] = NA_REAL;
                        }else
                        {
                          Bt_ni[j] = vimp_Bt_x(tm_index_Temp[j], h);
                        }
                      }
                      Bt_n[i] = Bt_ni;
                    }
                    vimp_Bt_H[h] = Bt_n;
                  }
                  
                  List Bx_Temp_K = Bx_K[k];
                  List Bxt_Dk(Dk[k]);
                  for(int d = 0; d < Dk[k]; ++d)
                  {
                    List Bx_Temp_Dk = Bx_Temp_K[d];
                    List Bxt_H(H);
                    for(int h = 0; h < H; ++h)
                    {
                      List Bt_Temp_H = vimp_Bt_H[h];
                      List Bxt_n(n);
                      for(int i = 0; i < n; ++i)
                      {
                        NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
                        NumericVector Bt_Temp_n = Bt_Temp_H[i];
                        NumericVector Bxt_ni(ni_x[i]);
                        for(int j = 0; j < ni_x[i]; ++j)
                        {
                          Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];
                        }
                        Bxt_n[i] = Bxt_ni;
                      }
                      Bxt_H[h] = Bxt_n;
                    }
                    Bxt_Dk[d] = Bxt_H;
                  }
                  vimp_Bxt[k] = Bxt_Dk;
                }else
                {
                  vimp_Bxt[k] = Bxt[k];
                }
              }
            }else 
            {
              for(int k = 0; k < K; ++k)
              {
                List vimp_Bt_H(H);
                if(k == vimp_set[kk])
                {
                  seed_value = seed_value + add_seed;
                  IntegerVector Index_Bt_x_noise = int_randomShuffle(Index_Bt_x, n_unq_tm_x,setting_seed,seed_value, false, R_NilValue);
                  NumericMatrix vimp_Bt_x(n_unq_tm_x,H);
                  for(int i = 0; i < n_unq_tm_x; ++i)
                  {
                    for(int j = 0; j < H; ++j)
                    {
                      if(j == hh)
                      {
                        vimp_Bt_x(i,j) = Bt_x(Index_Bt_x_noise[i],j);
                      }else
                      {
                        vimp_Bt_x(i,j) = Bt_x(Index_Bt_x[i],j);
                      }
                    }
                  }
                  for(int h = 0; h < H; ++h)
                  {
                    List Bt_n(n);
                    for(int i = 0; i < n; ++i)
                    {
                      IntegerVector tm_index_Temp = tm_index_x[i];
                      NumericVector Bt_ni(ni_x[i]);
                      for(int j = 0; j < ni_x[i]; ++j)
                      {
                        if(IntegerVector::is_na(tm_index_Temp[j]))
                        {
                          Bt_ni[j] = NA_REAL;
                        }else
                        {
                          Bt_ni[j] = vimp_Bt_x(tm_index_Temp[j], h);
                        }
                      }
                      Bt_n[i] = Bt_ni;
                    }
                    vimp_Bt_H[h] = Bt_n;
                  }
                  
                  List Bx_Temp_K = Bx_K[k];
                  List Bxt_Dk(Dk[k]);
                  for(int d = 0; d < Dk[k]; ++d)
                  {
                    List Bx_Temp_Dk = Bx_Temp_K[d];
                    List Bxt_H(H);
                    for(int h = 0; h < H; ++h)
                    {
                      List Bt_Temp_H = vimp_Bt_H[h];
                      List Bxt_n(n);
                      for(int i = 0; i < n; ++i)
                      {
                        NumericVector Bx_Temp_n = Bx_Temp_Dk[i];
                        NumericVector Bt_Temp_n = Bt_Temp_H[i];
                        NumericVector Bxt_ni(ni_x[i]);
                        for(int j = 0; j < ni_x[i]; ++j)
                        {
                          Bxt_ni[j] = Bx_Temp_n[j]*Bt_Temp_n[j];
                        }
                        Bxt_n[i] = Bxt_ni;
                      }
                      Bxt_H[h] = Bxt_n;
                    }
                    Bxt_Dk[d] = Bxt_H;
                  }
                  vimp_Bxt[k] = Bxt_Dk;
                }else
                {
                  vimp_Bxt[k] = Bxt[k];
                }
              }
            }


            List vimp_Bxt_time_map(K);
            for(int k = 0; k < K; ++k)
            {
              List temp_Bxt_K = vimp_Bxt[k];
              List Bxt_time_map_Dk(Dk[k]);
              for(int d = 0; d < Dk[k]; ++d)
              {
                List temp_Bxt_Dk = temp_Bxt_K[d];
                List Bxt_time_map_H(H);
                for(int h = 0; h < H; ++h)
                {
                  List temp_Bxt_H = temp_Bxt_Dk[h];
                  List Bxt_time_map_n(n);
                  for(int i = 0; i < n; ++i)
                  {
                    NumericVector temp_Bxt_n = temp_Bxt_H[i];
                    mat temp_time_map = time_map[i];
                    NumericVector temp_Bxt_time_map_n = Matrix_Vector_Multiplication_C_naObs(temp_time_map,temp_Bxt_n );
                    Bxt_time_map_n[i] = temp_Bxt_time_map_n;
                  }
                  Bxt_time_map_H[h] = Bxt_time_map_n;
                }
                Bxt_time_map_Dk[d] = Bxt_time_map_H;
              }
              vimp_Bxt_time_map[k] = Bxt_time_map_Dk;
            }



            List vimp_Byt_xt_time_map(K);
            for(int k = 0; k < K; ++k)
            {
              List vimp_Bt_G(G);

              seed_value = seed_value + add_seed;
              IntegerVector Index_Bt_noise = int_randomShuffle(Index_Bt,n_unq_tm,setting_seed,seed_value, false, R_NilValue);            
              NumericMatrix vimp_Bt(n_unq_tm,G);
              for(int i = 0; i < n_unq_tm; ++i)
              {
                for(int j = 0; j < G; ++j)
                {
                  if(j == gg)
                  {
                    vimp_Bt(i,j) = Bt(Index_Bt_noise[i],j);  
                  }else
                  {
                    vimp_Bt(i,j) = Bt(Index_Bt[i],j); 
                  }
                }
              }

              for(int g = 0; g < G; ++g)
              {
                List Bt_n(n);
                for(int i = 0; i < n; ++i)
                {
                  IntegerVector tm_index_Temp = tm_index[i];
                  NumericVector Bt_ni(ni[i]);
                  for(int j = 0; j < ni[i]; ++j)
                  {
                    if(IntegerVector::is_na(tm_index_Temp[j]))
                    {
                      Bt_ni[j] = NA_REAL;
                    }else
                    {
                      Bt_ni[j] = vimp_Bt(tm_index_Temp[j], g);  
                    }
                  }
                  Bt_n[i] = Bt_ni;
                }
                vimp_Bt_G[g] = Bt_n;
              }
              
              List temp_Bxt_time_map_K = vimp_Bxt_time_map[k];
              List Byt_xt_time_map_Dk(Dk[k]);
              for(int d = 0; d < Dk[k]; ++d)
              {
                List temp_Bxt_time_map_Dk = temp_Bxt_time_map_K[d];
                List Byt_xt_time_map_H(H);
                for(int h = 0; h < H; ++h)
                {
                  List temp_Bxt_time_map_H = temp_Bxt_time_map_Dk[h];
                  List Byt_xt_time_map_G(G);
                  for(int g = 0; g < G; ++g)
                  {
                    List temp_Bt_G = vimp_Bt_G[g];
                    List Byt_xt_time_map_n(n);
                    for(int i = 0; i < n; ++i)
                    {
                      NumericVector temp_Bt_n = temp_Bt_G[i];
                      NumericVector temp_Bxt_time_map_n = temp_Bxt_time_map_H[i];
                      NumericVector Byt_xt_time_map_ni(ni[i]);
                      for(int j = 0; j < ni[i]; ++j)
                      {
                        Byt_xt_time_map_ni[j] = temp_Bt_n[j]*temp_Bxt_time_map_n[j];
                      }
                      Byt_xt_time_map_n[i] = Byt_xt_time_map_ni;
                    }
                    Byt_xt_time_map_G[g] = Byt_xt_time_map_n;
                  }
                  Byt_xt_time_map_H[h] = Byt_xt_time_map_G;
                }
                Byt_xt_time_map_Dk[d] = Byt_xt_time_map_H;
              }
              vimp_Byt_xt_time_map[k] = Byt_xt_time_map_Dk;
            }



            
            vec linear_pred_vimp(N,fill::zeros);
            for(int k = 0; k < K; ++k)
            {
              List Bxt_Temp_K = vimp_Byt_xt_time_map[k];
              List Beta_Temp_K = Beta_Mopt[k];
              for(int d = 0; d < Dk[k]; ++d)
              {
                List Bxt_Temp_Dk = Bxt_Temp_K[d];
                List Beta_Temp_Dk = Beta_Temp_K[d];
                for(int h = 0; h < H; ++h)
                {
                  List Bxt_Temp_H = Bxt_Temp_Dk[h];
                  List Beta_Temp_H = Beta_Temp_Dk[h];
                  for(int g = 0; g < G; ++g)
                  {
                    List Bxt_Temp_G = Bxt_Temp_H[g];
                    List Beta_Temp_G = Beta_Temp_H[g];
                    mat Beta_Temp_L = Beta_Temp_G[l];
                    vec Temp_Temp_linear_pred = extract_linear_pred(N, id_index, Bxt_Temp_G, Beta_Temp_L, intercept);
                    linear_pred_vimp = linear_pred_vimp + Temp_Temp_linear_pred;
                  }
                }
              }
            }
            
            vec Org_linear_pred_vimp(N);
            Org_linear_pred_vimp = ( linear_pred_vimp * y_Std_Error[l] ) + y_Mean[l];
            
            vec Org_mu_vimp(N);
            Org_mu_vimp = extract_mu(Org_linear_pred_vimp,y_scale_summary );
            
            NumericVector Org_y_Temp(N);
            NumericVector Org_mu_Temp(N);
            
            for(int i = 0; i < N; ++i)
            {
              Org_y_Temp[i] = Org_y(i,l);
              Org_mu_Temp[i] = Org_mu_vimp(i);
            }
            if(y_scale_summary == "continuous")
            {
              Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index)/l2Dist_Vector_C_NA(Org_y_Temp, Vec_zero, id_index);
            }else
            {
              if(y_scale_summary == "binary")
              {
                Error_Rate_vimp = l2Dist_Vector_C_NA(Org_y_Temp, Org_mu_Temp, id_index);
              }
            }
            vimp_Temp(gg, hh) = (Error_Rate_vimp - rmse[l])/rmse[l];
          }
        }
        vimp_int[kk] = vimp_Temp;
      }
    }
    List vimp_combine = List::create(
      _["vimp_main"] = vimp_main,
      _["vimp_int"] = vimp_int
    );
    vimp[l] = vimp_combine;
  }
  
  return List::create(
    _["vimp"] = vimp
  );
}

