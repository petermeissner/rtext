#' text class
#'
#'
#' @docType class
#'
#' @name rtext
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{R6Class}}
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field text
#'  a single character string / character vector of length one
#'
#' @field tokens
#'  a data frame with a character vector representing the the text as tokens,
#'  and two integer vectors capturing the span of the token measured in
#'  characters starting from beginning of text
#'
#' @field file
#'  path to a file from which text was read in
#'
#' @field encoding
#'  encoding to be assumed for the text (will always be UTF-8, because.)
#'
#' @field sourcetype
#'  list of logicals that capture where the text came from when
#'  initializing object: text or file.
#'  (text=NULL, file=NULL) : "empty";
#'  (text="", file=NULL | file="") : "text";
#'  (text=NULL, file="") : "file"
#'
#'
#' @section Methods:
#' \describe{
#'  \item{
#'    \code{
#'    new(text=NULL, file=NULL, tokenize = "\n", encoding="UTF-8", id=NULL)
#'  }}{
#'    Method called when initialising object via
#'    \code{robotstxt$new()}. Needs either \code{text} or \code{domain} to be
#'    present at initialization. If only domain is supplied -- should be seen as
#'    default -- than the robots.txt file for that domain will be downloaded. If
#'    \code{text} is supplied as well, nothing will be downloaded. If only
#'    \code{text} is supplied than domain is set to '???'.
#'  }
#'  \item{\code{
#'    dummy(buuh)
#'  }}{
#'    TBD
#'  }
#' }
#'
#' @examples
#' mytext <- rtext$new("Hallo World")
#' mytext$text
#' mytext$show_text()
#'
#' # dings
#' mytext <- rtext$new(c("Hallo","World"))
#' mytext$text
#' mytext$show_text()
#'
rtext <-
  R6::R6Class(

  #### class name ==============================================================
  "rtext",

  #### private =================================================================
  private = list(
    text = function(){
      paste0(private$chars, collapse = "")
    },
    tokenize = function(){
      self$token <- self$tokenizer(private$text())
    },
    chars      = character(0),
    chars_data = data.frame(),
    token      = data.frame(),
    hash_text  = function(){
      self$hash <- digest::digest(private$chars)
    }
  ),


  #### public ==================================================================
  public = list(


    #### puplic data fields ====================================================
    file       = NA,
    tokenizer  = NA,
    encoding   = NA,
    sourcetype = NA,
    id         = NULL,
    hash       = NULL,

    #### startup function ====================================================

    initialize =
      function(
        text       = NULL,
        file       = NULL,
        tokenizer  = rtext_tokenizer$words,
        encoding   = "UTF-8",
        id         = NULL,
        tokenize_by= NULL
      )
    {

      ##### read in text // set field: sourcetype
      if(is.null(text) & is.null(file)){ # nothing at all
        private$chars <- ""
        self$sourcetype <- "empty"
      }else if(is.null(text) & !is.null(file)){ # read from file
        private$chars <- text_read(file, tokenize = "", encoding = encoding)
        self$sourcetype <- "file"
      }else{ # take text as supplied
        private$chars <-
          unlist(strsplit(paste0(iconv(text, encoding, "UTF-8"), collapse = "\n"),""))
        self$sourcetype <- "text"
      }
      self$hash <- private$hash_text()

      ##### set field: file
      if( !is.null(file) ){
        self$file <- file
      }

      ##### Encoding
      Encoding(private$chars) <- encoding
      self$encoding <- "UTF-8"

      #### tokenize
      self$tokenizer <- tokenizer
      if( !is.null(tokenize_by) ){
        self$tokenizer <-
          function(x){
            text_tokenize(x, regex = tokenize_by, non_token = TRUE)
          }
      }

      ##### id
      if( is.null(id) ){
        self$id <- digest::digest(self)
      }
    }
      ,

    #### methods ============================================================
    # info
    info = function(){
      res <-
        list(
          file       = self$file,
          character  = length(private$chars),
          token      = dim(self$token),
          encoding   = self$encoding,
          sourcetype = self$sourcetype
        )
      return(res)
      },
    # show text
    show_text = function(length=500, from=NULL, to=NULL, coll=FALSE, wrap=FALSE){
      text_show(x=self$get_text(Inf), length=length, from=from, to=to, coll=coll, wrap=wrap)
    },
    get_text = function(length=100, from=NULL, to=NULL, split=NULL){
      hash <- paste(self$text_hash(), deparse(match.call()))
      if( !(hash %in% ls(dp_storage)) ){
        res <- rtext_get_character(chars=private$chars, length=length, from=from, to=to)
        res <- paste0(res, collapse = "")
        if( !is.null(split) ){
          res <- unlist(strsplit(res, split = split))
        }
        assign(hash, res, envir = dp_storage)
        return(res)
      }else{
        return(get(hash, envir = dp_storage))
      }
    },
    # get_character
    get_character = function(length=100, from=NULL, to=NULL){
      rtext_get_character(chars=private$chars, length=length, from=from, to=to)
    },
    # add
    add = function(what=NULL, after=NULL){
      if(is.null(after)){
        after <- length(private$chars)
      }
      index  <- seq_along(private)
      part1  <- index[index <= after]
      part2  <- index[index >  after]
      private$chars <-
        c( part1, unlist(strsplit(what,"")), part2)
      private$hash_text()
      invisible(self)
    },
    # delete
    delete = function(from, to){

    },
    # replace
    replace = function(from, to, replacement, regex){

    },
    # save
    save = function(){

    },
    # reload
    reload = function(){

    },
    # save_as
    save_as = function(){

    },
    text_hash = function(){
      self$hash
    }
  )
)



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

























