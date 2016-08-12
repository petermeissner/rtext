#' extended R6 class
#'
#'
#' @docType class
#'
#' @name R6_rtext_extended
#'
#' @export
#'
#' @keywords data
#'
#' @return Object of \code{\link{R6Class}}
#'
#' @format \code{\link{R6Class}} object.
#'
R6_rtext_extended <-
  R6::R6Class(


    #### class name ============================================================
    "R6_rtext_extended",



    #### private ===============================================================
    private = list(
      hashes = list(),
      hashed =
        function(name=NULL){
          # special case NULL
          if( is.null(name) ){
            name <- self$ls()$name
          }
          # recursion
          if( length(name)>1 ){
            tmp <- lapply(name, private$hashed)
            names(tmp) <- name
            return(tmp)
          }
          # doing-duty-to-do
          if( is.null(private$hashes[[name]]) ){
            private$hashes[[name]] <- private$hash(name)
          }
          return(
            private$hashes[[name]]
          )
        },
      hash   =
        function(name=NULL){
          # special case NULL
          if( is.null(name) ){
            name <- self$ls()$name
          }
          # recursion
          if( length(name)>1 ){
            tmp <- lapply(name, private$hash)
            names(tmp) <- name
            return(tmp)
          }
          # doing-duty-to-do
          tmp <- self$get(name)
          if( is.null(tmp) ){
            tmp <- NULL
          }else{
            tmp <- rtext_hash(tmp)
          }
          private$hashes[[name]] <- tmp
          # return
          return(tmp)
        }
    ),



    #### public ================================================================
    public = list(

      #### [ verbose ] #### ....................................................
      #      should message method print messages or not
      verbose = TRUE,

      #### [ get() ] #### ......................................................
      #      get stuff (private or public out of instance)
      get = function(name=NULL){
        # recursion
        if( length(name)>1 ){
          tmp <- lapply(name, self$get)
          names(tmp) <- name
          return(tmp)
        }
        if(is.null(name)){
          self$message("no input, returning NULL")
          return(NULL)
        }
        # self
        if(name=="self"){
          return(self)
        }
        # in self
        if( name %in% names(self) ){
          return(base::get(name, envir=self))
        }
        # private or in private
        if( exists("private") ){
          if(name=="private"){
            return(private)
          }else if(name %in% names(private) ){
            return(base::get(name, envir=private))
          }
        }
        # else
        self$message("name not found")
        return(NULL)
      },

      debug = function(){
        assign("self", self, envir = globalenv())
        assign("private", private, envir = globalenv())
        self$message("[self] and [private] assigned to global environment")
        return(NULL)
      },

      #### [ ls() ] #### ......................................................
      #      list contents of instance
      ls = function( what=c("self","private"), class=NULL){
        tmp_where   <- character(0)
          tmp_names <- character(0)
        tmp_classes <- character(0)
        df <- data.frame()
        if( "self" %in% what ){
          tmp_where   <- "self"
          tmp_names   <- ls(self)
          tmp_classes <-
            vapply(
              X         = tmp_names,
              FUN       =
                function(x){
                  paste(class(self[[x]]), sep = ", ")
                },
              FUN.VALUE = character(1)
            )
          df <-
            rbind(
              data.frame(
                name  = tmp_names,
                where = tmp_where,
                class = tmp_classes
              ),
              df,
              make.row.names = FALSE
            )
        }
        if( "private" %in% what & exists("private") ){
          tmp_where   <- "private"
          tmp_names   <- ls(private)
          tmp_classes <-
            vapply(
              X         = tmp_names,
              FUN       =
                function(x){
                  paste(class(private[[x]]), sep = ", ")
                },
              FUN.VALUE = character(1)
            )
          df <-
            rbind(
              data.frame(
                name  = tmp_names,
                where = tmp_where,
                class = tmp_classes
              ),
              df,
              make.row.names = FALSE
            )
        }
        if( dim(df)[1] > 0 ){
          df <- df[order(df$where, df$class, df$name), ]
        }
        if( !is.null(class) ){
          df <- df[grep(class, df$class), ]
        }
        return(df)
      },

      #### [ message() ] #### ..................................................
      #      post a message (if verbose is set to TRUE)
      message = function(x, ...){
        xname <- as.character(as.list(match.call()))[2]
        if(self$verbose){
          if(is.character(x)){
            message(class(self)[1], " : ", x, ...)
          }else{
            message(class(self)[1], " : ", xname, " : \n", x)
          }
        }
      }
    )


)


