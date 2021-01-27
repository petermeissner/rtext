#' R6 class - linking text and data
#'
#' @docType class
#' @name rtext_export
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#' @seealso \code{\link{rtext}}
#'
rtext_export <-
  R6::R6Class(

    #### misc ====================================================================
    classname    = "rtext_export",
    active       = NULL,
    inherit      = rtext_loadsave,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('rtext'),



    #### private =================================================================
    private = list(),



    #### public ==================================================================
    public = list(

      #### [ export_csv ] #### .......................................................
      export_csv = function(folder_name = ""){
        stopifnot(file.info(folder_name)$isdir)
        "TBD"
      },

      #### [ import_csv ] #### .......................................................
      import_csv = function(folder_name = ""){
        stopifnot(file.info(folder_name)$isdir)
        "TBD"
      },

      #### [ export_sqlite ] #### .......................................................
      export_sqlite = function(db_name = ""){
        # establish connection
        if( is.character(db_name) ){
          con <- RSQLite::dbConnect( RSQLite::SQLite(), db_name)
          on.exit({
              RSQLite::dbDisconnect(con)
          })
        }else{
          con <- db_name
        }
        # preapare data to be exportd
        tb_exported <- private$prepare_save()
        # export data
        RSQLite::dbBegin(con)
          RSQLite::dbWriteTable(con, "meta",      tb_exported$meta, overwrite=TRUE)
          RSQLite::dbWriteTable(con, "hashes",    tb_exported$hashes, overwrite=TRUE)
          RSQLite::dbWriteTable(con, "char",      as.data.frame(tb_exported$char), overwrite=TRUE)
          for(i in seq_along(tb_exported$char_data) ){
            if(i==1){
              overwrite <- TRUE
              append    <- FALSE
            }else{
              overwrite <- FALSE
              append    <- TRUE
            }
            tmp_name <- names(tb_exported$char_data[[i]])[3]
            tmp <- tb_exported$char_data[[i]]
            names(tmp) <- c("i","hl","val")
            tmp[["var"]] <- tmp_name
            RSQLite::dbWriteTable(
              con,
              "char_data",
              tmp,
              overwrite=overwrite,
              append=append
            )
          }
        RSQLite::dbCommit(con)
        # return
        return(invisible(self))
      },

      #### [ import_sqlite ] #### .......................................................
      import_sqlite = function(db_name = ""){
        # establish connection
        if( is.character(db_name) ){
          con <- RSQLite::dbConnect(RSQLite::SQLite(), db_name)
          on.exit({
            RSQLite::dbDisconnect(con)
          })
        }else{
          con <- db_name
        }
        # import data
        imported <- list()
        imported$meta   <- RSQLite::dbReadTable(con, "meta")
        imported$hashes <- RSQLite::dbReadTable(con, "hashes")
        imported$char   <- RSQLite::dbReadTable(con, "char")[[1]]
        # import char_data
        if( RSQLite::dbExistsTable(con, "char_data") ){
          tmp <- RSQLite::dbReadTable(con, "char_data")
          tmp <- split(tmp, f=tmp$var)
          for( i in seq_along(tmp) ) {
            nam <- tmp[[i]]$var[1]
            tmp[[i]][[4]] <- NULL
            names(tmp[[i]]) <- c("i", "hl", nam)
          }
        }else{
          tmp <- list()
        }
        imported$char_data   <- tmp
        # incorporate data
        private$execute_load(imported)
        # return self for piping
        return(invisible(self))
      }
    )
  )












