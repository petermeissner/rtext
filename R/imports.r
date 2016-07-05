#' imports
#' @importFrom R6 R6Class
#' @import hellno
#' @importFrom magrittr %>%
dummyimport <- function(){
  R6::R6Class()
  1 %>% magrittr::add(1)
}

#' @useDynLib diffrprojects
#' @importFrom Rcpp sourceCpp
NULL
