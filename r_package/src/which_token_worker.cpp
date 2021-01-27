#include <Rcpp.h>
using namespace Rcpp;
//' (function to check which chars belong to which token)
//' takes a vector of xs to check if these lie between pairs of ys and if so
//' returning their index; assumes xs and ys are sorted; returns only the first
//' span index which enclosing the x
//' @param x a vector of type numeric; a number to be placed into a span
//' @param y1 a vector of type numeric; first element of span
//' @param y2 a vector of type numeric; last element of span
//' @keywords internal
// [[Rcpp::export]]
IntegerVector which_token_worker(
    NumericVector x,
    NumericVector y1,
    NumericVector y2
)
{
  // setting things up
  IntegerVector res(x.length(), NA_INTEGER) ;
  int last_j = 0;
  // loopy-loop
  for( int i = 0; i < x.length(); i++ ){
    // allow for user interupt
    if( i % 10000 == 0 ){
      Rcpp::checkUserInterrupt();
    }
    // doing-duty-to-do:
    for( int j=0+last_j; j < y1.length(); j++ ){
      if( x[i] >= y1[j] &&  x[i] <= y2[j] ){
        res[i] = j+1;
        last_j = j-1;
        break;
      }
    }
  }
  return res;
}


