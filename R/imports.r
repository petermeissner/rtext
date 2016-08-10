#' imports
#' @importFrom R6 R6Class
#' @import hellno
#' @importFrom magrittr %>%
#' @import stringb
dummyimport <- function(){
  R6::R6Class()
  1 %>% magrittr::add(1)
}

#' @useDynLib rtext
#' @importFrom Rcpp sourceCpp
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
NULL
