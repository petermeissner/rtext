#' helper function: to read in text
#'
#' a wrapper to readLines() to make things more ordered and convenient
#'
#' @param file name of the file to be read in
#' @param tokenize either
#'    NULL so that no splitting is done;
#'    a character to use to split text into parts;
#'    or a function that does the splitting
#' @param encoding character encoding of file
#' @export

text_read <- function(file, tokenize=NULL, encoding="UTF-8")
{
  tmp <- readLines(file, encoding = encoding)
  if(encoding!="UTF-8"){
    tmp <- iconv(tmp, encoding, "UTF-8")
  }
  # all within one vector element
  if( is.null(tokenize) ){
    return(paste0(tmp, collapse = "\n"))
  }
  # tokenized by function
  if(is.function(tokenize)){
    return( unlist(tokenize(paste0(tmp, collapse = "\n"))) )
  }
  # vector elements should correspond to lines
  if(tokenize == "\n"){
    return(tmp)
  }
  # tokenized by other pattern
  if(is.character(tokenize)){
    return(unlist(strsplit(paste0(tmp, collapse = "\n"), tokenize)))
  }
}



#' helper function: retrieving text snippet
#'
#' function will give back snippets of length length for length-from or
#' length-to combinations but will give snippets of to-from length for parameter
#' from-to-combinations
#' @param x character vector to be snipped
#' @param length length of snippet
#' @param from starting character
#' @param to last character
#' @param coll should a possible vector x with length > 1 collapsed with newline
#'    character as separator?
#' @export
text_snippet <-function(x, length=500, from=NULL, to=NULL, coll=FALSE){
  # input check
  stopifnot( length(length)!=0 | (length(from)!=0 & length(to)!=0) ) # any input
  # collapse before snipping?
  if(coll!=FALSE){
    if( identical(coll, TRUE) ){
      x <- paste0(x, collapse = "\n")
    }else{
      x <- paste0(x, collapse = coll)
    }
  }
  # snipping cases
  if( !is.null(from) & !is.null(to) ){                          # from + to
    return(substring(x, from, to))
  }else if( !is.null(from) & is.null(to) ){                     # from + length
    return(substring(x, from, from+length-1))
  }else if( is.null(from) & !is.null(to) ){                     # to + length
    return(substring(x, to-length, to))
  }else if( length(length)!=0 & is.null(from) & is.null(to) ){  # length
    return(substring(x, 0, length))
  }
}



#' text function: wrapper for system.file() to access test files
#' @export
#' @param x name of the file
tf <- function(x=NULL){
  if(is.null(x)){
    return(list.files(system.file("testfiles", package = "diffrprojects")))
  }else if(x==""){
    return(list.files(system.file("testfiles", package = "diffrprojects")))
  }else{
    return(system.file(paste("testfiles", x, sep="/"), package = "diffrprojects") )
  }
}
























