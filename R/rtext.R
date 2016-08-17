#' R6 class - linking text and data
#'
#' @docType class
#' @name rtext
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format \code{\link{R6Class}} object.
#'
rtext <-
  R6::R6Class(

    #### misc ====================================================================
    classname    = "rtext",
    active       = NULL,
    inherit      = rtext_tokenize,
    lock_objects = TRUE,
    class        = TRUE,
    portable     = TRUE,
    lock_class   = FALSE,
    cloneable    = TRUE,
    parent_env   = asNamespace('rtext'),

    #### private =================================================================
    private = list(),



    #### public ==================================================================
    public = list()

)












