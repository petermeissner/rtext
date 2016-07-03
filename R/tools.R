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
  if( dim1(df1) > 0 ){
    df1[, names_df[!(names_df %in% names(df1))]] <- rep(NA, dim1(df1))
  }else{
    df1 <- data.frame()
  }
  if( dim1(df2) > 0 ){
    df2[, names_df[!(names_df %in% names(df2))]] <- rep(NA, dim1(df2))
  }else{
    df2 <- data.frame()
  }
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


#' function that extracts elements from vector
#'
#' @param vec the chars field
#' @param length number of elements to be returned
#' @param from first element to be returned
#' @param to last element to be returned
#'
#' @export
#'
get_vector_element <-
  function(vec, length=100, from=NULL, to=NULL){
    # helper functions
    bind_to_vecrange <- function(x){bind_between(x, 1, length(vec))}
    bind_length       <- function(x){bind_between(x, 0, length(vec))}
    return_from_to    <- function(from, to, split){
      res  <- vec[seq(from=from, to=to)]
      return(res)
    }
    # only length
    if( !is.null(length) & ( is.null(from) & is.null(to) ) ){
      length <- max(0, min(length, length(vec)))
      length <- bind_length(length)
      if(length==0){
        return("")
      }
      from   <- 1
      to     <- length
      return(return_from_to(from, to, split))
    }
    # from and to (--> ignores length argument)
    if( !is.null(from) & !is.null(to) ){
      from <- bind_to_vecrange(from)
      to   <- bind_to_vecrange(to)
      return(return_from_to(from, to, split))
    }
    # length + from
    if( !is.null(length) & !is.null(from) ){
      if( length<=0 | from + length <=0 ){
        return("")
      }
      to   <- from + length-1
      if((to < 1 & from < 1) | (to > length(vec) & from > length(vec) )){
        return("")
      }
      to   <- bind_to_vecrange(to)
      from <- bind_to_vecrange(from)
      return(return_from_to(from, to, split))
    }
    # length + to
    if( !is.null(length) & !is.null(to) ){
      if( length<=0 | to - (length-1) > length(vec) ){
        return("")
      }
      from <- to - length + 1
      if((to < 1 & from < 1) | (to > length(vec) & from > length(vec) )){
        return("")
      }
      from <- bind_to_vecrange(from)
      to   <- bind_to_vecrange(to)
      return(return_from_to(from, to, split))
    }
    stop("get_vector_element() : I do not know how to make sense of given length, from, to argument values passed")
  }



#' get first dimension or length of object
#' @param x object, matrix, vector, data.frame, ...
#' @export
dim1 <- function(x){
  ifelse(is.null(dim(x)[1]), length(x), dim(x)[1])
}


#' get first dimension or length of object
#' @param x object, matrix, vector, data.frame, ...
#' @export
dim2 <- function(x){
  dim(x)[2]
}


#' seq along first dimension / length
#' @param x x
#' @export
seq_dim1 <- function(x){
  seq_len(dim1(x))
}


#' function returning index of spans that entail x
#' @param x position of the character
#' @param y1 start position of the token
#' @param y2 end position of the token
#' @export
which_token <- function(x, y1, y2){
  # how to order x and y?
  order_x <- order(x)
  order_y <- order(y1)
  # order x and y! - which_token_worker expects inputs to be ordered
  ordered_x  <- x[order_x]
  ordered_y1 <- y1[order_y]
  ordered_y2 <- y2[order_y]
  # doing-duty-to-do
  index <- which_token_worker(ordered_x, ordered_y1, ordered_y2)
  # ordering back to input ordering
  index <- order_y[index[order(order_x)]]
  # return
  index
}




#' function giving back the mode
#' @export
#' @param x vector to get mode for
#' @param multimodal wether or not all modes should be returned in case of more than one
modus <- function(x, multimodal=FALSE, warn=TRUE) {
  x_unique <- unique(x)
  tab_x    <- tabulate(match(x, x_unique))
  res      <- x_unique[which(tab_x==max(tab_x))]
  if( identical(multimodal, TRUE) ){
    return(res)
  }else{
    if( warn & length(res) > 1 ){
      warning("modus : multimodal but only one value returned (use warn=FALSE to turn this off)")
    }
    if( !identical(multimodal, FALSE) ){
      return(multimodal)
    }else{
      return(res[1])
    }
  }
}





































