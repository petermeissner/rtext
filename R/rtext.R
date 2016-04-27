#' text class
#'
#'
#' @docType class
#'
#' @name rtext
#'
#' @export
#' @importFrom R6 R6Class
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
#' mytext <- rtext$new("Hallo World")
#' mytext$text
#'
#' mytext <- rtext$new(c("Hallo","World"))
#' mytext$text
#'
rtext <-
  R6::R6Class(

  #### class name ==============================================================
    "rtext",

  #### public ==================================================================
    public = list(

      #### puplic data fields ==================================================
      text       = NA,
      file       = NA,
      tokens     = NA,
      encoding   = NA,
      sourcetype = list(text=NA, file=NA),
      #### startup function ====================================================
      initialize = function( text=NULL, file=NULL, tokenize = "\n", encoding="UTF-8" )
      {
        stopifnot( (is.null(text) & !is.null(file)) | (!is.null(text) &  is.null(file)) )
        # check input
        if( is.null(text) ){
          self$text <- NULL
        }else{
          self$text <- paste0( text, collapse = "\n")
        }
        self$file <- file
        self$sourcetype$text <- ifelse( !is.null(text), TRUE, FALSE )
        self$sourcetype$file <- ifelse( !is.null(file), TRUE, FALSE )
      },
      # methods
      # checking bot permissions
      hallo_world = function(paths="/", bot="*", permission=self$permissions){
        paths_allowed(permissions=permission, paths=paths, bot=bot)
      }
    )
  )











