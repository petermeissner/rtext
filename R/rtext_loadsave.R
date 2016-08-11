#' R6 class - load and save methods for rtext
#'
#' @docType class
#' @name rtext_loadsave
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#'
rtext_loadsave <-
  R6::R6Class(

    #### misc ====================================================================
    classname    = "rtext_loadsave",
    active       = NULL,
    inherit      = rtext_base,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('rtext'),

    #### private =================================================================
    private = list(
    ),

    #### public ==================================================================
    public = list(


    #### [ save ] #### .........................................................
    save = function(file=NULL, id=c("self_id", "hash")){
      # gather information
      tb_saved <-
        list(
          id           = self$id,
          char         = private$char,
          char_data    = private$char_data,
          text_file    = self$text_file,
          encoding     = self$encoding,
          save_file    = self$save_file,
          sourcetype   = self$sourcetype,
          session_info = list(
            dp_version=packageVersion("diffrprojects"),
            r_version=paste(version$major, version$minor, sep="."),
            version=version
          )
        )
      class(tb_saved) <- c("rtext_save","list")
      # handle id option
      if( id[1] == "self_id"){
        id <- self$id
      }else if( id[1] == "hash"){
        id <- self$hash()
      }else{
        id <- id[1]
      }
      id <- paste0("rtext_", id, collapse = "_")
      # handle file option
      if( is.null(self$save_file) & is.null(file) ){
        stop("rtext$save() : Neither file nor save_file given, do not know where to store file.")
      }else if( !is.null(file) ){
        file <- file
      }else if( !is.null(self$save_file) ){
        file <- self$save_file
      }
      # save to file
      assign(id, tb_saved)
      base::save(list = id, file = file)
      # return for piping
      invisible(self)
    },

    #### [ load ] ..............................................................
    load = function(file=NULL){
      # handle file option
      if( is.null(file) ){
        stop("rtext$load() : file is not given, do not know where to load file from.")
      }else{
        file <- file
      }
      tmp <- load_into(file)[[1]]

      # setting public
      self$id         <- tmp$id
      self$text_file  <- tmp$text_file
      self$encoding   <- tmp$encoding
      self$sourcetype <- tmp$sourcetype
      self$save_file  <- tmp$save_file

      # setting private
      private$char       <- tmp$char
      private$char_data  <- tmp$char_data

      # updating rest
      private$hash()

      # return for piping
      invisible(self)
    },

    #### [ export ] #### .......................................................
    export = function(){
      message("TBD")
    }
  )
)

