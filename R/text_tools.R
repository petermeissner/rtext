#' read in text
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



#' retrieving text snippet
#'
#' function will give back snippets of text via using length,
#' length and from, length and to, or from and to to specify the snippet
#' @param x character vector to be snipped
#' @param length length of snippet
#' @param from starting character
#' @param to last character
#' @param coll should a possible vector x with length > 1 collapsed with newline
#'    character as separator?
#' @describeIn text_snippet retrieving text snippet
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


#' function for showing text
#'
#' shows text or portions of the text via cat and the usage of text_snippet()
#' @param x text to be shown
#' @param length number of characters to be shown
#' @param from show from ith character
#' @param to show up to ith character
#' @param coll should x be collapsed using newline character as binding?
#' @param wrap should text be wrapped, or wrapped to certain width, or wrapped
#'    by certain function
#' @export
text_show = function(x, length=500, from=NULL, to=NULL, coll=FALSE, wrap=FALSE){
  tmp       <- text_snippet(x, length, from, to, coll)
  diff_char <- sum(nchar(x)) - sum(nchar(tmp)) > 0
  diff_sum  <- sum(nchar(x)) - sum(nchar(tmp))
  diff_note <- ifelse(diff_char, paste0(" [... ", format(diff_sum, big.mark = " "), " characters not shown]"),"")
  if(wrap==FALSE){
    cat( tmp, diff_note)
  }else if(is.function(wrap)){
    cat(wrap(tmp), diff_note)
  }else{
    cat(unlist(strsplit(tmp, " ")), diff_note, fill=wrap)
  }
}





#' function to tokenize text
#' @param x character vector to be tokenized
#' @param regex regex to use for tokenization
#' @return data.frame,
#'    token: string of the token;
#'    from: position in text at which token starts;
#'    to: position in text at which the token ends
#'    length: length of the token;
#'    type: type of the token, either its matched by regular expression used for tokenization or not matched
text_tokenize <- function(x, regex){

}















