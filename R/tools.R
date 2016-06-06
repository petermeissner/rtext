#' function that shifts vector values to right or left
#'
#' @param x Vector for which to shift values
#' @param n Number of places to be shifted.
#'    Positive numbers will shift to the right by default.
#'    Negative numbers will shift to the left by default.
#'    The direction can be inverted by the invert parameter.
#' @param default The value that should be inserted by default.
#' @param invert Whether or not the default shift directions
#'    should be inverted.
#' @export
shift <- function(x, n=0, default=NA, invert=FALSE){
  n <-
    switch (
      as.character(n),
      right    =  1,
      left     = -1,
      forward  =  1,
      backward = -1,
      lag      =  1,
      lead     = -1,
      as.numeric(n)
    )
  stopifnot(length(x)>=n)
  if(n==0){
    return(x)
  }
  n <- ifelse(invert, n*(-1), n)
  if(n<0){
    n <- abs(n)
    forward=FALSE
  }else{
    forward=TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
}

#' function forcing value to fall between min and max
#' @param x the values to be bound
#' @param max upper boundary
#' @param min lower boundary
bind_between <- function(x, min, max){
  x[x<min] <- min
  x[x>max] <- max
  return(x)
}


#' function for binding data.frames even if names do not match
#' @param df1 first data.frame to rbind
#' @param df2 second data.frame to rbind
#' @export
rbind_fill <- function(df1=data.frame(), df2=data.frame()){
  names_df <- c(names(df1), names(df2))
  df1[, names_df[!(names_df %in% names(df1))]] <- rep(NA, dim(df1)[1])
  df2[, names_df[!(names_df %in% names(df2))]] <- rep(NA, dim(df2)[1])
  rbind(df1, df2)
}


#' function to get hash for R objects
#' @export
dp_hash <- function(x){
  digest::digest(x, algo="xxhash64")
}




#' function that checks is values are in between values
#' @export
#' @param x input vector
#' @param y lower bound
#' @param z upper bound
is_between <- function(x,y,z){
  return(x>=y & x<=z)
}








