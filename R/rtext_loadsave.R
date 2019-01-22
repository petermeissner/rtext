#' R6 class - load and save methods for rtext
#'
#' @docType class
#' @name rtext_loadsave
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @seealso \code{\link{rtext}}
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

      #### [ prepare_save ] #### ...............................................
      prepare_save = function(id=NULL){
        # handle id option
        if( is.null(id) ){
          id <- self$id
        }else if( id[1] == "hash"){
          tb_saved$meta$id <- self$hash()
        }else{
          tb_saved$meta$id <- id[1]
        }
        # put together information
        tb_saved <-
          list(
            meta = data.frame(
              id           = id,
              date         = as.character(Sys.time()),
              text_file    = self$text_file,
              encoding     = self$encoding,
              save_file    = ifelse(is.null(self$save_file), NA, self$save_file),
              sourcetype   = self$sourcetype,
              rtext_version= as.character(packageVersion("rtext")),
              r_version    = paste(version$major, version$minor, sep="."),
              save_format_version = 1
            ),
            hashes       = as.data.frame(private$hash()),
            char         = private$char,
            char_data    = private$char_data
          )
        class(tb_saved) <- c("rtext_save","list")
        # return
        return(tb_saved)
      },


      #### [ execute_load ] #### ...............................................
      execute_load = function(tmp){
        # setting public
        self$id         <- tmp$meta$id
        self$text_file  <- tmp$meta$text_file
        self$encoding   <- tmp$meta$encoding
        self$sourcetype <- tmp$meta$sourcetype
        self$save_file  <- tmp$meta$save_file

        # setting private
        private$char       <- tmp$char
        private$char_data  <- tmp$char_data

        # updating rest
        private$hash()

        # return for piping
        invisible(self)
      }
    ),

    #### public ==================================================================
    public = list(


    #### [ save ] #### .........................................................
    save = function(file=NULL, id=NULL){
      rtext_save <- as.environment(private$prepare_save(id=id))
      # handle file option
      if(
        (is.na(rtext_save$meta$save_file) | is.null(rtext_save$meta$save_file)) &
        is.null(file)
      ){
        stop("rtext$save() : Neither file nor save_file given, do not know where to store file.")
      }else if( !is.null(file) ){
        file <- file
      }else if( !is.null(rtext_save$meta$save_file) ){
        file <- rtext_save$meta$save_file
      }
      # save to file
      base::save(
        list = ls(rtext_save),
        file = file,
        envir = rtext_save
      )
      # return for piping
      return(invisible(self))
    },

    #### [ load ] ..............................................................
    load = function(file=NULL){
      # handle file option
      if( is.null(file) ){
        stop("rtext$load() : file is not given, do not know where to load file from.")
      }else{
        file <- file
      }
      # loading info
      tmp <- load_into(file)
      # applying loaded info to self
      private$execute_load(tmp)
      # return self for piping
      return(invisible(self))
    }
  )
)

