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
      paste0(self$chars, collapse = "")
    },
    tokenize = function(){
      self$token <- self$tokenizer(private$text())
    }
  ),


  #### public ==================================================================
  public = list(


    #### puplic data fields ====================================================
    file       = NA,
    tokenizer  = NA,
    encoding   = NA,
    sourcetype = NA,
    token      = data.frame(),
    id         = NULL,
    chars      = NULL,
    chars_data = data.frame(),

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
        self$chars <- ""
        self$sourcetype <- "empty"
      }else if(is.null(text) & !is.null(file)){ # read from file
        self$chars <- text_read(file, tokenize = "", encoding = encoding)
        self$sourcetype <- "file"
      }else{ # take text as supplied
        self$chars <-
          unlist(strsplit(paste0(iconv(text, encoding, "UTF-8"), collapse = "\n"),""))
        self$sourcetype <- "text"
      }

      ##### set field: file
      if( !is.null(file) ){
        self$file <- file
      }

      ##### Encoding
      Encoding(self$chars) <- encoding
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
          character  = length(self$chars),
          token      = dim(self$token),
          encoding   = self$encoding,
          sourcetype = self$sourcetype
        )
      return(res)
      },
    # show text
    show_text = function(length=500, from=NULL, to=NULL, coll=FALSE, wrap=FALSE){
      # text_show(x=self$text(), length=length, from=from, to=to, coll=coll, wrap=wrap)
    },
    # get_text
    get_text = function(length=100, from=NULL, to=NULL, split=NULL){
      # helper functions
      bind_to_charrange <- function(x){bind_between(x, 1, length(self$chars))}
      bind_length       <- function(x){bind_between(x, 0, length(self$chars))}
      return_from_to    <- function(from, to, split){
        res  <- paste0( self$chars[seq(from=from, to=to)], collapse = "")
        if( !is.null(split) ){
          res <- unlist(strsplit(res, split = split))
        }
        return(res)
      }
      # only length
      if( !is.null(length) & ( is.null(from) & is.null(to) ) ){
        length <- max(0, min(length, length(self$chars)))
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
        if((to < 1 & from < 1) | (to > length(self$chars) & from > length(self$chars) )){
          return("")
        }
        to   <- bind_to_charrange(to)
        from <- bind_to_charrange(from)
        return(return_from_to(from, to, split))
      }
      # length + to
      if( !is.null(length) & !is.null(to) ){
        if( length<=0 | to - (length-1) > length(self$chars) ){
          return("")
        }
        from <- to - length + 1
        if((to < 1 & from < 1) | (to > length(self$chars) & from > length(self$chars) )){
          return("")
        }
        from <- bind_to_charrange(from)
        to   <- bind_to_charrange(to)
        return(return_from_to(from, to, split))
      }
      stop("rtext$get_text() : I do not know how to make sense of given length, from, to argument values passed")
    },
    # insert
    insert = function(what=NULL, after=text_length(self$text())){
      # stopifnot( after >= 0 & after <= text_length(self$text()) )
      # after <- paste0(what, collapse="\n")
      # what_length <- text_length(what)
      # if( what_length == 0){
      #   return(TRUE)
      # }
      # if( after==0 ){
      #   self$text() <- paste0(what, self$text())
      #   return(TRUE)
      # }
      # if( after==text_length(self$text()) ){
      #   self$text() <- paste0(self$text(), what)
      #   return(TRUE)
      # }
      # FALSE
    },
    # delete
    delete = function(){

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
      digest::digest(chars)
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
































