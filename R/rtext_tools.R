
#' storage for internals
dp_storage <- new.env(parent = emptyenv())


#' list of ready to use functions for rtext initialization and tokenization
#' @export
rtext_tokenizer <- list(
  words = function(x){text_tokenize_words(x, non_token = TRUE)}
)


#' function to get text from rtext object
#'
#' @param chars the chars field
#' @param length number of characters to be returned
#' @param from first character to be returned
#' @param to last character to be returned
#' @export
rtext_get_character <- function(chars, length=100, from=NULL, to=NULL){
  # helper functions
  bind_to_charrange <- function(x){bind_between(x, 1, length(chars))}
  bind_length       <- function(x){bind_between(x, 0, length(chars))}
  return_from_to    <- function(from, to, split){
    res  <- chars[seq(from=from, to=to)]
    return(res)
  }
  # only length
  if( !is.null(length) & ( is.null(from) & is.null(to) ) ){
    length <- max(0, min(length, length(chars)))
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
    from <- bind_to_charrange(from)
    to   <- bind_to_charrange(to)
    return(return_from_to(from, to, split))
  }
  # length + from
  if( !is.null(length) & !is.null(from) ){
    if( length<=0 | from + length <=0 ){
      return("")
    }
    to   <- from + length-1
    if((to < 1 & from < 1) | (to > length(chars) & from > length(chars) )){
      return("")
    }
    to   <- bind_to_charrange(to)
    from <- bind_to_charrange(from)
    return(return_from_to(from, to, split))
  }
  # length + to
  if( !is.null(length) & !is.null(to) ){
    if( length<=0 | to - (length-1) > length(chars) ){
      return("")
    }
    from <- to - length + 1
    if((to < 1 & from < 1) | (to > length(chars) & from > length(chars) )){
      return("")
    }
    from <- bind_to_charrange(from)
    to   <- bind_to_charrange(to)
    return(return_from_to(from, to, split))
  }
  stop("rtext$get_character() : I do not know how to make sense of given length, from, to argument values passed")
}

#' function used to delete parts from a vector
#' @param x input vector
#' @param n number of items to be deleted
#' @param from from which position onwards elements should be deleted
#' @param to up to which positions elements should be deleted
#' @export
vector_delete <- function(x, n=NULL, from=NULL, to=NULL){
  # shortcuts
  if( is.null(n) ){
    if(is.null(from) & is.null(to)){
      return(x)
    }
  }else{
    if( n==0){
      return(x)
    }
  }
  # iffer
  iffer <- TRUE
  if( is.null(from) & is.null(to)  & !is.null(n) ){ # only n
    iffer <- seq_along(x) > length(x) | seq_along(x) <= length(x)-n
  }else if( !is.null(from) & is.null(to)  & is.null(n) ){ # only from
    iffer   <- seq_along(x) < from
  }else if( is.null(from) & !is.null(to) & is.null(n) ){ # only to
    iffer   <- seq_along(x) > to
  }else if( !is.null(from) & !is.null(to)  & is.null(n) ){ # from + to
    iffer   <- seq_along(x) > to | seq_along(x) < from
  }else if( !is.null(from) & is.null(to)  & !is.null(n) ){ # from + n
    if( n > 0 ){
      n     <- bind_between(n-1, 0, length(x))
      iffer <- seq_along(x) > from+n | seq_along(x) < from
    }
  }else if( is.null(from) & !is.null(to)  & !is.null(n) ){ # to + n
      iffer <- seq_along(x) > to | seq_along(x) <= to-n
  }
  # return
  return( x[iffer] )
}




#' function that loads saved rtext
#' @param save_file a saved rtext object in Rdata format
#' @export
load_into <- function(save_file){
  tmp_env <- new.env(parent = emptyenv())
  load(save_file, envir = tmp_env)
  lapply(tmp_env, I)
}


























