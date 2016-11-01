#' R6 class - linking text and data
#'
#' @docType class
#' @name rtext
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}}
#' @format An \code{\link{R6Class}} generator object.
#' @section The rtext class family:
#'
#' Rtext consists of an set of R6 classes that are conencted by inheritance.
#' Each class handles a different set of functionalities that are - despite
#' needing the data structure provided by rtext_base - independent.
#'
#' \describe{
#'    \item{R6_rtext_extended}{
#'      A class that has nothing to do per se with rtext
#'      but merely adds some basic features to the base R6 class (debugging,
#'      hashing, getting fields and handling warnings and messages as well as
#'      listing content)
#'    }
#'
#'    \item{rtext_base}{
#'    [inherits from R6_rtext_extended] The foundation of the rtext class.
#'      This class allows to load and store text, its meta data, as well as data
#'      about the text in a character by character level.
#'    }
#'
#'    \item{rtext_loadsave}{
#'      [inherits from rtext_base] Adds load and save methods for loading and saving
#'      rtext objects (text and data) into/from Rdata files.}
#'
#'    \item{rtext_export}{
#'      [inherits from rtext_loadsave] Adds methods to import and export from and
#'      to SQLite databases - like load and save but for SQLite.
#'    }
#'
#'    \item{rtext_tokenize}{
#'      [inherits from rtext_export] Adds methods to aggregate character level data
#'      onto token level. (the text itself can be tokenized via S3 methods from
#'      the stringb package - e.g. text_tokenize_words())
#'    }
#'
#'    \item{rtext}{
#'      [inherits from rtext_tokenize] Adds no new features at all but is just a
#'      handy label sitting on top of all the functionality provided by the
#'      inheritance chain.
#'    }
#' }
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












