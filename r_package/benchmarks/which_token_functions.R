library(Rcpp)

cppFunction(
  'IntegerVector fw1(
    NumericVector x,
    NumericVector y1,
    NumericVector y2
  ) {
    IntegerVector res(x.length()) ;
    int last_j = 0;
    for( int i=0; i < x.length(); i++ ){
      Rcpp::checkUserInterrupt() ;
      res[i] = NA_INTEGER;
      for( int j=0+last_j; j < y1.length(); j++ ){
        if( x[i] >= y1[j] &&  x[i] <= y2[j] ){
          res[i] = j+1;
          last_j = j-1;
          break;
        }
      }
    }
    return res;
  }'
)


cppFunction(
  'IntegerVector fw2(
    NumericVector x,
    NumericVector y1,
    NumericVector y2
) {
  IntegerVector res(x.length()) ;
  int last_j = 0;
  for( int i=0; i < x.length(); i++ ){
    Rcpp::checkUserInterrupt() ;
    for( int j=0+last_j; j < y1.length(); j++ ){
      if( x[i] >= y1[j] &&  x[i] <= y2[j] ){
        res[i] = j+1;
        last_j = j-1;
        break;
      }
    }
  }
  return res;
}'
)

cppFunction(
  'IntegerVector fw3(
    NumericVector x,
    NumericVector y1,
    NumericVector y2
) {
  IntegerVector res(x.length()) ;
  int last_j = 0;
  for( int i=0; i < x.length(); i++ ){
    for( int j=0+last_j; j < y1.length(); j++ ){
      if( x[i] >= y1[j] &&  x[i] <= y2[j] ){
        res[i] = j+1;
        last_j = j-1;
        break;
      }
    }
  }
  return res;
}'
)



cppFunction(
  'IntegerVector fw4(
    NumericVector x,
    NumericVector y1,
    NumericVector y2
) {
  IntegerVector res(x.length()) ;
  int last_j = 0;
  for( int i=0; i < x.length(); i++ ){
    for( int j=0+last_j; j < y1.length(); j++ ){
      if(j % 10000 == 0){
        Rcpp::checkUserInterrupt();
      }
      if( x[i] >= y1[j] &&  x[i] <= y2[j] ){
        res[i] = j+1;
        last_j = j-1;
        break;
      }
    }
  }
  return res;
}'
)

cppFunction(
  'IntegerVector fw5(
  NumericVector x,
  NumericVector y1,
  NumericVector y2
) {
  IntegerVector res(x.length()) ;
  int last_j = 0;
  for( int i=0; i < x.length(); i++ ){
  for( int j=0+last_j; j < y1.length(); j++ ){

  Rcpp::checkUserInterrupt();

  if( x[i] >= y1[j] &&  x[i] <= y2[j] ){
  res[i] = j+1;
  last_j = j-1;
  break;
  }
  }
  }
  return res;
}'
)


cppFunction(
  '
IntegerVector fw6(
    NumericVector x,
  NumericVector y1,
  NumericVector y2
) {
  IntegerVector res(x.length()) ;
  int last_j = 0;
  for( int i=0; i < x.length(); i++ ){
  if(i % 10000 == 0){
  Rcpp::checkUserInterrupt();
  }
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
  '
)

f_fw2 <- function(x,y1,y2){
  tmp <- fw2(x,y1,y2)
  tmp[tmp==0] <- NA
  tmp
}

f_fw3 <- function(x,y1,y2){
  tmp <- fw3(x,y1,y2)
  tmp[tmp==0] <- NA
  tmp
}

f_fw4 <- function(x,y1,y2){
  tmp <- fw4(x,y1,y2)
  tmp[tmp==0] <- NA
  tmp
}

f_fw5 <- function(x,y1,y2){
  tmp <- fw5(x,y1,y2)
  tmp[tmp==0] <- NA
  tmp
}

f_fw6 <- function(x,y1,y2){
  tmp <- fw6(x,y1,y2)
  tmp[tmp==0] <- NA
  tmp
}




