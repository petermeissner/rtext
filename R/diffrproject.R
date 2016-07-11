#' class for diffrproject
#'
#' @docType class
#'
#' @name diffrproject
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{R6Class}}
#'
#' @format \code{\link{R6Class}} object.
#'
#'
diffrproject <-
  R6::R6Class(

    #### class name ============================================================
    "diffrproject",



    #### private ===============================================================
    private = list(

    ),



    #### public ================================================================
    public = list(


      #### data ================================================================
      meta     = list(),
      options  = list(),
      tracks   = list(),
      linkage  = list(),
      distance = list(),
      texts    = list(),


      #### methods =============================================================
      # add text
      text_add = function( rtext, name = NULL ){
          stopifnot("rtext"  %in% class(rtext))
          self$texts[[name]] <- rtext
          # return self for piping
          return(invisible(self))
      },

      # delete text
      text_delete = function(name){
        self$texts[[name]] <- NULL
        # return self for piping
        return(invisible(self))
      },

      # universal getter
      get = function(name){
        if(name=="private"){
          return(private)
        }
        if( name %in% names(self) ){
          return(get(name, envir=self))
        }else if( name %in% names(private) ){
          return(get(name, envir=private))
        }else{
          return(NULL)
        }
      }

    )
  )
















