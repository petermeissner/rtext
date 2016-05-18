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
      paste0(private$char, collapse = "")
    },
    tokenize = function(){
      self$token <- self$tokenizer(private$text())
    },
    char       = character(0),
    char_data  = data.frame(),
    token      = data.frame(),
    token_data = data.frame(),
    hash       = character(),
    hasht      = character(),
    hashd      = character(),
    hash_text  = function(){
      private$hasht <- digest::digest(private$char)
      private$hash  <- digest::digest(list(private$hashd, private$hasht))
    },
    hash_data  = function(){
      private$hashd <- digest::digest(private$char_data)
      private$hash  <- digest::digest(list(private$hashd, private$hasht))
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
        private$char <- ""
        self$sourcetype <- "empty"
      }else if(is.null(text) & !is.null(file)){ # read from file
        private$char <- text_read(file, tokenize = "", encoding = encoding)
        self$sourcetype <- "file"
      }else{ # take text as supplied
        private$char <-
          unlist(strsplit(paste0(iconv(text, encoding, "UTF-8"), collapse = "\n"),""))
        self$sourcetype <- "text"
      }
      private$hash <- private$hash_text()

      ##### set field: file
      if( !is.null(file) ){
        self$file <- file
      }

      ##### Encoding
      Encoding(private$char) <- encoding
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
          character  = length(private$char),
          token      = dim(self$token),
          encoding   = self$encoding,
          sourcetype = self$sourcetype
        )
      return(res)
      },
    # show text
    text_show = function(length=500, from=NULL, to=NULL, coll=FALSE, wrap=FALSE){
      text_show(x=self$text_get(Inf), length=length, from=from, to=to, coll=coll, wrap=wrap)
    },
    text_get = function(length=100, from=NULL, to=NULL, split=NULL){
      hash <- paste(self$text_hash(), deparse(match.call()))
      if( !(hash %in% ls(dp_storage)) ){
        res <- rtext_get_character(chars=private$char, length=length, from=from, to=to)
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
    # char_get
    char_get = function(length=100, from=NULL, to=NULL){
      rtext_get_character(chars=private$char, length=length, from=from, to=to)
    },
    # add
    char_add = function(what=NULL, after=NULL){
      if( is.null(after) ) {
        private$char <- c(private$char, unlist(strsplit(what,"")))
      }else if ( after==0 ) {
        private$char <- c(unlist(strsplit(what,"")), private$char)
      }else{
        index  <- seq_along(private$char)
        part1  <- private$char[index <= after]
        part2  <- private$char[index >  after]
        private$char <- c( part1, unlist(strsplit(what, "")), part2)
      }
      # necessary updates
      private$hash_text()
      # return for piping
      invisible(self)
    },
    # delete
    char_delete = function(n=NULL, from=NULL, to=NULL){
      between <- function(x,y,z){x>=y & x<=z}
      if( is.null(from) & is.null(to)  & !is.null(n)){ # only n
        iffer <- private$char[seq_along(private$char) <= length(private$char)-n]

        private$char <-
          private$char[iffer]
      }else if( !is.null(from) & is.null(to)  & is.null(n)){ # only from
        iffer <-
            seq_along(private$char) < from

        private$char <-
          private$char[iffer]
      }else if( is.null(from) & !is.null(to) & is.null(n) ){ # only to
        iffer <-
          seq_along(private$char) > to

        private$char <-
          private$char[iffer]
      }else if( !is.null(from) & !is.null(to)  & is.null(n)){ # from + to
        iffer <- seq_along(private$char) > to | seq_along(private$char) < from

        private$char <-
          private$char[iffer]
      }else if( !is.null(from) & is.null(to)  & !is.null(n) ){ # from + n
        if( n > 0 ){
          n     <-
            bind_between(n-1, 0, length(private$char))

          iffer <-
            seq_along(private$char) > from+n | seq_along(private$char) < from

          private$char <-
            private$char[iffer]
        }
      }else if( is.null(from) & !is.null(to)  & !is.null(n) ){ # to + n
        if( n > 0 ){
          n     <-
            bind_between(n-1, 0, length(private$char))

          iffer <-
            seq_along(private$char) > to | seq_along(private$char) < to-n

          private$char <-
            private$char[iffer]
        }
      }
      # necessary updates
      private$hash_text()
      # return for piping
      invisible(self)
    },
    # replace
    char_replace = function(from=NULL, to=NULL, by=NULL){
      # check input
      stopifnot( !is.null(from), !is.null(to), !is.null(by) )
      # doing-duty-to-do
      index <- seq_along(private$char)
      private$char <-
        c(
          private$char[index < from],
          unlist(strsplit(by, "")),
          private$char[index > to]
        )
      # necessary updates
      private$has_text()
      # return for piping
      invisible(self)
    },
    # code characters
    char_code = function(x=NULL, val=NULL, from=NULL, to=NULL){
      # update data already in self$char_data
      iffer <- char_data$i > 1
      private$char_data[iffer, x] <- rep(val, sum(iffer))
      # add data not already in self$char_data
      index        <- seq(from, to)
      index        <- index[ !(index %in% private$char_data) ]
      add_df       <- data.frame(i=index)
      add_df[[x]]  <- val
      private$char <- rbind_fill(private$char, add_df)
      # necessary updates
      private$hash_data()
      # return for piping
      invisible(self)
    },
    # save
    save = function(){
      message("TBD")
    },
    # reload
    load = function(){
      message("TBD")
    },
    # save_as
    save_as = function(){
      message("TBD")
    },
    # text_hash
    text_hash = function(){
      private$hash
    },
    # data_hash
    data_hash = function(){
      private$hash
    }
  )
)


















