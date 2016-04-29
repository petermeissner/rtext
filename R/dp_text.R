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
#' @return Object of \code{\link{R6Class}} with method(s) for bot permission checking.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @field text
#'  a single character string / character  vector of length one
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
#'  encoding to be assumed for the text
#'
#' @field soourcetype
#'  list of logicals that captures where the text came from when
#'  initializing object: text or file
#'
#'
#' @section Methods:
#' \describe{
#'  \item{
#'    \code{
#'    initialize(domain, text) }}{
#'    Method called when initialising object via
#'    \code{robotstxt$new()}. Needs either \code{text} or \code{domain} to be
#'    present at initialization. If only domain is supplied -- should be seen as
#'    default -- than the robots.txt file for that domain will be downloaded. If
#'    \code{text} is supplied as well, nothing will be downloaded. If only
#'    \code{text} is supplied than domain is set to '???'.
#'  }
#'  \item{\code{
#'    check( path="/", bot="*" ) }}{
#'    Method for checking whether or not paths are allowed to be accessed by a bot
#'  }
#' }
#'
#' @examples
#' mytext <- dp_text$new("Hallo World")
#' mytext$show_text()
#'
#' mytext <- dp_text$new(c("Hallo","World"))
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
    tokens     = NA,
    encoding   = NA,
    sourcetype = NA,

    #### startup function ====================================================
    initialize =
      function( text=NULL, file=NULL, tokenize = "\n", encoding="UTF-8" )
    {

      # read in text
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
    }
      ,

    # methods
    show_text = function(text=self$text, length=500, from=NULL, to=NULL){

    }
  )
)








































