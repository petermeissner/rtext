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

#' wrapper around nchar to return text length
#' @param x see \link{nchar}
#' @param type see \link{nchar}
#' @param allowNA see \link{nchar}
#' @param keepNA see \link{nchar}
#' @export
text_nchar <- function(x, type = "chars", allowNA = FALSE, keepNA = TRUE){
  nchar(x, type, allowNA, keepNA)
}

#' wrapper around nchar to return text length
#' @param x see \link{nchar}
#' @param type see \link{nchar}
#' @param allowNA see \link{nchar}
#' @param keepNA see \link{nchar}
#' @param na.rm see \link{nchar}
#' @export
text_length <- function(x, type = "chars", allowNA = FALSE, keepNA = TRUE, na.rm=FALSE){
  sum(text_nchar(x, type, allowNA, keepNA), na.rm=na.rm)
}


#' function to tokenize text
#' @param x character vector to be tokenized
#' @param regex regex to use for tokenization
#' @param ignore.case see \link{grep}, interanlly passed through to gregexpr()
#' @param fixed see \link{grep}, interanlly passed through to gregexpr()
#' @param useBytes see \link{grep}, interanlly passed through to gregexpr()
#' @param group predefined regular expressions
#' @return data.frame,
#'    token: string of the token;
#'    from: position in text at which token starts;
#'    to: position in text at which the token ends
#'    length: length of the token;
#'    type: type of the token, either its matched by regular expression used for tokenization or not matched
#' @export
text_tokenize <- function(x, regex=NULL, ignore.case=FALSE, fixed=FALSE, useBytes=FALSE, group=c("words", "lines", "paragraphs")){
  tlength <- text_length(x)

  if( is.null(regex) ){
    regex <-
      switch(
        group[1],
        words = "\\w+",
        lines = "\n",
        paragraphs = "\n\n"
      )
  }

  found        <- gregexpr(regex, x, ignore.case, fixed, useBytes)
  found_from   <- found[[1]]
  found_length <- attributes(found[[1]])$match.length
  found_to     <- found_length+found_from-1

  token <-
    do.call(
      rbind,
      mapply(
        function(x, from, to){ data.frame(from, to, token = substring(x, from, to)) },
        from=found_from,
        to=found_to,
        MoreArgs = list(x      = x),
        SIMPLIFY = FALSE
      )
    )
  token$is_token <- TRUE

  non_token <-
    data.frame(
      from=c(1,token$to+1),
      to=c(token$from-1, tlength)
    )
  non_token <- non_token[non_token$from<=non_token$to,]
  non_token$token <- mapply(substring, first=non_token$from, last=non_token$to, MoreArgs = list(text=x))
  non_token$is_token=FALSE

  res <- rbind(token, non_token)
  res <- res[order(res$from),]
  res
}














