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
  diff_note <- ifelse(diff_char, paste0("\n[... ", format(diff_sum, big.mark = " "), " characters not shown]"),"")
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
#' @param non_token whether or not to return non-tokens as well
#' @return data.frame,
#'    token: string of the token;
#'    from: position in text at which token starts;
#'    to: position in text at which the token ends
#'    length: length of the token;
#'    type: type of the token, either its matched by regular expression used for tokenization or not matched
#' @export
text_tokenize <-
  function(
    x,
    regex       = NULL,
    ignore.case = FALSE,
    fixed       = FALSE,
    useBytes    = FALSE,
    non_token   = FALSE
  ){
    # recursion
    if(length(x)>1){
      lapply(
        x,
        text_tokenize,
        regex       = regex,
        ignore.case = ignore.case,
        fixed       = fixed,
        useBytes    = useBytes,
        non_token   = non_token
      )
    }else{
      # special cases
      if( any(grepl(regex, "")==TRUE) ){
        tmp <- strsplit(x, regex)[[1]]
        token <- data.frame(
          from     = seq_along(tmp),
          to       = seq_along(tmp),
          token    = tmp,
          is_token = rep(TRUE, length(tmp))
        )
        return(token)
      }
      if( is.null(regex) ){
        regex <- ".*"
      }
      # finding characters spans where to split
      tlength <- text_length(x)
      found_splitter        <- gregexpr(regex, x, ignore.case, fixed, useBytes)
      found_splitter_from   <- found_splitter[[1]]
      found_splitter_length <- attributes(found_splitter[[1]])$match.length
      found_splitter_to     <- found_splitter_length+found_splitter_from-1

      # infering tokens
      char_splitter <-
        unique(
          unlist(
            mapply(seq, found_splitter_from, found_splitter_to, SIMPLIFY = FALSE)
          )
        )

      char_token <-
        sort(unique(seq_len(tlength)[!(seq_len(tlength) %in% char_splitter)]))

      char_token_from     <- c(1,found_splitter_to+1)
      char_token_to       <- c(ifelse(found_splitter[[1]]==1, 1, found_splitter[[1]]-1),tlength)

      token <-
        data.frame(
          from  = char_token_from,
          to    = char_token_to
        )

      token <-
        subset(token, !(token$from %in% char_splitter | token$to %in% char_splitter))

      # handling special cases
      if( tlength>0 & dim(token)[1]==0 & !all(found_splitter[[1]]>0) ){
        token <- rbind(token, c(1, tlength))
        names(token) <- c("from", "to")
      }

      # filling with tokens
      tmp <- unlist(strsplit(x, regex))
      tmp <- tmp[tmp!=""]

      token$token    <- tmp[seq_along(token$from)]
      token$is_token <- rep(TRUE, dim(token)[1])

      # adding non-tokens
      if(non_token==TRUE){
        # handling special cases
        if( any(found_splitter_to<0) | any(found_splitter_from<0) ){
          found_splitter_to   <- integer(0)
          found_splitter_from <- integer(0)
        }
        # adding to token
        non_token <-
          data.frame(
            from     = found_splitter_from,
            to       = found_splitter_to,
            token    = regmatches(x, found_splitter)[[1]],
            is_token = rep(FALSE, length(found_splitter_to))
          )
        token <-
          rbind(token, non_token )
      }

      # return
      return(token)
    }
  }


#' tokenize text into words
#'
#' A wrapper to text_tokenize that tokenizes text into words.
#' Since using text_tokenize()'s option non_token might slow things
#' down considerably this one purpose wrapper is a little more clever
#' than the general implementation and hence much faster.
#'
#' @param x the text to be tokenized
#' @param non_token whether or not token as well as non tokens shall be returned.
#' @export
text_tokenize_words <-
  function(
    x,
    non_token = FALSE
  ){
    res <- text_tokenize(x, "\\W+")
    if(non_token){
      tmp <- text_tokenize(x, "\\w+")
      tmp$is_token <- rep(FALSE, dim(tmp)[1])
      res <- rbind(res, tmp)
    }
    return(res)
  }


#' extract regex matches
#'
#' wrapper function around regexec and regmatches
#'
#' @param x text from which to extract
#' @param pattern see \link{grep}
#' @param ignore.case see \link{grep}
#' @param perl see \link{grep}
#' @param fixed see \link{grep}
#' @param useBytes see \link{grep}
#' @export
text_extract <-
  function(
    x,
    pattern,
    ignore.case = FALSE,
    perl = FALSE,
    fixed = FALSE,
    useBytes = FALSE
  ){
    regmatches(
      x,
       regexpr(
         pattern=pattern,
         text=x,
         ignore.case = ignore.case,
         perl = perl,
         fixed = fixed,
         useBytes = useBytes
       )
    )
  }



#' extract regex matches
#'
#' wrapper function around gregexec and regmatches
#'
#' @param x text from which to extract
#' @param pattern see \link{grep}
#' @param ignore.case see \link{grep}
#' @param perl see \link{grep}
#' @param fixed see \link{grep}
#' @param useBytes see \link{grep}
#' @export
text_extract_all <-
  function(
    x,
    pattern,
    ignore.case = FALSE,
    perl = FALSE,
    fixed = FALSE,
    useBytes = FALSE
  ){
    regmatches(
      x,
      gregexpr(
        pattern=pattern,
        text=x,
        ignore.case = ignore.case,
        perl = perl,
        fixed = fixed,
        useBytes = useBytes
      )
    )
  }



#' function for collapsing text vectors
#' @param x object to be collapsed
#' @param sep separator between text parts
#' @export
text_collapse <- function (x, ..., sep) {
  UseMethod("text_collapse")
}

#' default method for text_collapse()
#' @rdname text_collapse
#' @method text_collapse default
#' @export
text_collapse.default <- function(x, ..., sep=""){
  paste0(x, ..., sep="", collapse = sep)
}

#' text_collapse() method for lists
#' @export
#' @rdname text_collapse
#' @method text_collapse list
text_collapse.list <- function(x, ..., sep=c("","")){
  if(is.list(x)){
    x <- lapply(x, text_collapse, sep=sep)
    x <- unlist(x, recursive = FALSE)
  }
  if(length(sep)>1){
    sep <- sep[2]
  }
  text_collapse(x, sep=sep)
}


#' text_collapse() method for data.frames
#' @export
#' @rdname text_collapse
#' @method text_collapse data.frame
text_collapse.data.frame <- function(x, ..., sep=c("", "\n")){
  if(is.data.frame(x)){
    x <- apply(x, 1, text_collapse, sep=sep[1])
    x <- unlist(x, recursive = FALSE)
  }
  if(length(sep)>1){
    sep <- sep[2]
  }
  text_collapse(x, sep=sep)
}



#' wrapper function of eval() and parse() to evaluate character vector
#' @param x character vector to be parsed and evaluated
#' @param envir where to evaluate character vector
#' @param ... arguments passed through to eval()
#' @export
text_eval <- function(x, envir=parent.frame(), ...){
  eval(
    parse(text = x),
    envir = envir,
    ...
  )
}




