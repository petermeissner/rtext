#' text class
#'
#'
#' @docType class
#'
#' @name dp_text
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
#' mytext <- dp_text$new("Hallo World")
#' mytext$text
#' mytext$show_text()
#'
#' # dings
#' mytext <- dp_text$new(c("Hallo","World"))
#' mytext$text
#' mytext$show_text()
#'
dp_text <-
  R6::R6Class(

  #### class name ==============================================================
  "dp_text",

  #### public ==================================================================
  public = list(

    #### puplic data fields ==================================================
    text       = NA,
    file       = NA,
    tokenize   = NA,
    encoding   = NA,
    sourcetype = NA,
    token      = data.frame(),
    id         = NULL,

    #### startup function ====================================================
    initialize =
      function( text=NULL, file=NULL, tokenize = "\n", encoding="UTF-8", id=NULL)
    {

      # read in text // set field: sourcetype
      if(is.null(text) & is.null(file)){
        self$text <- ""
        self$sourcetype <- "empty"
      }else if(is.null(text) & !is.null(file)){
        self$text <- text_read(file, tokenize = NULL, encoding = encoding)
        self$sourcetype <- "file"
      }else{
        self$text <- paste0(text, collapse = "\n")
        self$sourcetype <- "text"
      }
      # set field: file
      if( !is.null(file) ){
        self$file <- file
      }
      # Encoding
      Encoding(self$text) <- encoding
      iconv(self$text, from = encoding, to = "UTF-8")
      self$encoding <- "UTF-8"
      # tokenize
      self$tokenize <- tokenize
      #self$token <- data.frame(token)
      # id
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
          character  = nchar(self$text),
          token      = dim(self$token)[1],
          tokendata  = dim(self$token)[2],
          tokenize   = self$tokenize,
          encoding   = self$encoding,
          sourcetype = self$sourcetype
        )
      return(res)
      },
    # show text
    show_text = function(length=500, from=NULL, to=NULL, coll=FALSE, wrap=FALSE){
      text_show(x=self$text, length=length, from=from, to=to, coll=coll, wrap=wrap)
    },
    # get_text
    get_text = function(length=nchar(self$text), from=NULL, to=NULL, coll=FALSE){
      text_snippet(self$text, length, from, to, coll)
    }
  )
)








































