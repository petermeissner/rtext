#' R6 class - linking text and data
#'
#' @docType class
#' @name rtext_export
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
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
        RSQLite::dbConnect(SQLite(), db_name)
        "TBD"
      },

      #### [ export_sqlite ] #### .......................................................
      import_sqlite = function(db_name = ""){
        "TBD"
      }

    )

  )












